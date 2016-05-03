{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
    GeneralizedNewtypeDeriving #-}
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

orderedPair :: (Monad m, Ord a) => Series m a -> Series m a -> Series m (a, a)
orderedPair = _' (flip suchThat (\(x, y) -> x <= y)) . (><)

orderedMPair :: (Monad m, Ord a) => Series m (Maybe a) -> Series m (Maybe a)
                                 -> Series m (Maybe a, Maybe a)
                                             -- used after its own test
orderedMPair = _' (flip suchThat (\(x, y) -> compareMaybeR x y /= GT)) . (><)

-- | 0, 1, -1, 13, 14, [2 .. 12], 15, -2, 16, -3 ... Serial type (one per depth)
newtype Smallint = SI Int
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
newtype MOPSmallint = MOPSI (Maybe Smallint, Maybe Smallint)
    deriving (Eq, Ord, Show)

instance Monad m => Serial m MOPSmallint where
    series = MOPSI <$> orderedMPair series series

-- | Specific order of 0 .. 13 (only) Serial type (one per depth)
newtype BSmallint = BSI Int
    deriving (Eq, Ord, Enum, Num, Integral, Real, Show)

instance Bounded BSmallint where
    minBound = 0
    maxBound = 13

instance Monad m => Serial m BSmallint where
    series = generate $ flip take $ map BSI $
            0 : 1 : 13 : 7 : ([2 .. 6] >>= \x -> [x, 14 - x])

-- | Ordered pair of BSmallints, such that x <= y
newtype OPBSmallint = OPBSI (BSmallint, BSmallint)
    deriving (Eq, Ord, Bounded, Show)

instance Monad m => Serial m OPBSmallint where
    series = OPBSI <$> orderedPair series series

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

