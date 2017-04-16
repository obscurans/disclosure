{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
    GeneralizedNewtypeDeriving, PartialTypeSignatures #-}
module Disclosure.Test.Util
( module Disclosure.Test.Util
, module Test.Tasty
, module Test.Tasty.HUnit
) where

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import qualified Test.Tasty.QuickCheck as Q
import Disclosure.Base.Util (Monoid'(..), _', compareMaybeR)

sTestProperty :: Testable IO a => TestName -> a -> TestTree
sTestProperty = testProperty

qTestProperty :: Q.Testable a => TestName -> a -> TestTree
qTestProperty = Q.testProperty

setSCDepth :: SmallCheckDepth -> TestTree -> TestTree
setSCDepth = localOption

adjSCDepth :: SmallCheckDepth -> TestTree -> TestTree
adjSCDepth x = adjustOption (x +)

-- | Somehow not exported from Test.SmallCheck.Series
suchThat :: Series m a -> (a -> Bool) -> Series m a
suchThat s p = s >>= \x -> if p x then pure x else empty

orderedPair :: (Monad m, Ord a) => Series m a -> Series m (a, a)
orderedPair = decDepth . flip suchThat (\(x, y) -> x <= y) . join (><)

orderedMPair :: (Monad m, Ord a) => Series m (Maybe a)
                                 -> Series m (Maybe a, Maybe a)
orderedMPair = decDepth . decDepth .      -- used after its own test
                flip suchThat (\(x, y) -> compareMaybeR x y /= GT) . join (><)

orderedPList :: (Monad m, Ord a) => Series m a -> Series m [(a, a)]
orderedPList s = cons0 [] \/ (flip suchThat ordP $ decDepth $ (:) <$>
                    orderedPair s <~> orderedPList s)
    where ordP [] = True
          ordP [x] = True
          ordP ((_,x2):(y1,_):_) = x2 < y1

orderedMPList :: (Monad m, Ord a) => Series m (Maybe a)
                                  -> Series m [(Maybe a, Maybe a)]
orderedMPList s = cons0 [] \/ (flip suchThat ordMP $ decDepth $ (:) <$>
                    orderedMPair s <~> orderedMPList s)
    where ordMP [] = True
          ordMP [x] = True
          ordMP ((_,x2):(y1,_):_) = fromMaybe False $ liftM2 (<) x2 y1

-- | 0, 1, -1, 13, 14, [2 .. 12], 15, -2, 16, -3 ... Serial type (one per depth)
newtype Smallint = SI {
    unSI :: Int }
    deriving (Eq, Ord, Enum, Num, Integral, Real, Show)

-- | Bounded instance is specifically incorrect
instance Bounded Smallint where
    minBound = 0
    maxBound = 13

instance Monad m => Serial m Smallint where
    series = generate $ flip take $ map SI $
            0 : 1 : -1 : 13 : 14 : [2 .. 12] ++ ([2 ..] >>= \x -> [13 + x, -x])

nothingSmallint = Nothing :: Maybe Smallint

-- | Ordered pair of Maybe Smallints, such that x <= y if both defined
newtype MOPSmallint = MOPSI {
    unMOPSI :: (Maybe Smallint, Maybe Smallint) }
    deriving (Eq, Ord, Show)

instance Monad m => Serial m MOPSmallint where
    series = MOPSI <$> orderedMPair series

-- | Specific order of 0 .. 13 (only) Serial type (one per depth)
newtype BSmallint = BSI {
    unBSI :: Int }
    deriving (Eq, Ord, Enum, Num, Integral, Real, Show)

instance Bounded BSmallint where
    minBound = 0
    maxBound = 13

instance Monad m => Serial m BSmallint where
    series = generate $ flip take $ map BSI $
            0 : 1 : 13 : 7 : ([2 .. 6] >>= \x -> [x, 14 - x])

-- | Ordered pair of BSmallints, such that x <= y
newtype OPBSmallint = OPBSI {
    unOPBSI :: (BSmallint, BSmallint) }
    deriving (Eq, Ord, Bounded, Show)

instance Monad m => Serial m OPBSmallint where
    series = OPBSI <$> orderedPair series

newtype OLMPSmallint = OLMPSI {
    unOLMPSI :: [(Maybe Smallint, Maybe Smallint)] }
    deriving (Eq, Ord, Show)

instance Monad m => Serial m OLMPSmallint where
    series = OLMPSI <$> orderedMPList series

newtype OLPBSmallint = OLPBSI {
    unOLPBSI :: [(BSmallint, BSmallint)] }
    deriving (Eq, Ord, Show)

instance Monad m => Serial m OLPBSmallint where
    series = OLPBSI <$> orderedPList series

-- | [2 .. 6] * [2 .. 6] Serial type
newtype Latticeint = Latticeint Int
    deriving (Eq, Ord, Enum, Num, Integral, Real, Show)

instance Monad m => Serial m Latticeint where
    series = generate $ flip take $ map Latticeint $
                (*) <$> [2 .. 6] <*> [2 .. 6]

-- | Commutative monoid is GCD, identity is finite LCM
instance Monoid Latticeint where
    mempty = join (*) $ foldr lcm 1 [1 .. 6]
    mappend = gcd

-- | Commutative monoid is GCD, fails if coprime, identity is finite LCM
instance Monoid' Latticeint where
    mempty' = join (*) $ foldr lcm 1 [1 .. 6]
    mappend' x y = if gcd x y == 1 then Nothing else Just $ gcd x y

-- | Test harnesses for setting depth on specific input types
testLI2 :: String -> (Latticeint -> Latticeint -> Bool) -> TestTree
testLI2 = _' (setSCDepth 15) . sTestProperty

testSI :: String -> (Smallint -> Bool) -> TestTree
testSI = _' (setSCDepth 30) . sTestProperty

testLSI :: String -> ([Smallint] -> Bool) -> TestTree
testLSI = _' (setSCDepth 6) . sTestProperty

testMSI2 :: String -> (Maybe Smallint -> Maybe Smallint -> Bool) -> TestTree
testMSI2 = _' (setSCDepth 15) . sTestProperty

testSI2 :: String -> (Smallint -> Smallint -> Bool) -> TestTree
testSI2 = _' (setSCDepth 15) . sTestProperty

testMSIP :: String -> ((Maybe Smallint, Maybe Smallint) -> Bool) -> TestTree
testMSIP = _' (setSCDepth 16) . sTestProperty

testSIP :: String -> ((Smallint, Smallint) -> Bool) -> TestTree
testSIP = _' (setSCDepth 16) . sTestProperty

testMOPSI :: String -> (MOPSmallint -> Bool) -> TestTree
testMOPSI = _' (setSCDepth 21) . sTestProperty

testOPBSI :: String -> (OPBSmallint -> Bool) -> TestTree
testOPBSI = _' (setSCDepth 21) . sTestProperty

testMOPSI2 :: String -> (MOPSmallint -> MOPSmallint -> Bool) -> TestTree
testMOPSI2 = _' (setSCDepth 8) . sTestProperty

testOPBSI2 :: String -> (OPBSmallint -> OPBSmallint -> Bool) -> TestTree
testOPBSI2 = _' (setSCDepth 8) . sTestProperty

testMOPSIL :: String -> ([MOPSmallint] -> Bool) -> TestTree
testMOPSIL = _' (setSCDepth 7) . sTestProperty

testOPBSIL :: String -> ([OPBSmallint] -> Bool) -> TestTree
testOPBSIL = _' (setSCDepth 6) . sTestProperty

testOLMPSI :: String -> (OLMPSmallint -> Bool) -> TestTree
testOLMPSI = _' (setSCDepth 8) . sTestProperty

testOLPBSI :: String -> (OLPBSmallint -> Bool) -> TestTree
testOLPBSI = _' (setSCDepth 7) . sTestProperty

testOLMPSI2 :: String -> (OLMPSmallint -> OLMPSmallint -> Bool) -> TestTree
testOLMPSI2 = _' (setSCDepth 7) . sTestProperty

testOLPBSI2 :: String -> (OLPBSmallint -> OLPBSmallint -> Bool) -> TestTree
testOLPBSI2 = _' (setSCDepth 6) . sTestProperty
