{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Disclosure.TestUtil where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck
import Disclosure.Base.Util (Monoid'(..))

adjSCDepth :: SmallCheckDepth -> TestTree -> TestTree
adjSCDepth x = adjustOption (x +)

newtype Smallint = Smallint Int
    deriving (Eq, Ord, Enum, Num, Integral, Real, Show)

instance Bounded Smallint where
    minBound = 0
    maxBound = 13

instance Monad m => Serial m Smallint where
    series = generate $ flip take $ map Smallint $
            0 : 1 : -1 : 13 : 14 : [2 .. 12] ++ ([2 ..] >>= \x -> [13 + x, -x])

nothingSmallint = Nothing :: Maybe Smallint

newtype Latticeint = Latticeint Int
    deriving (Eq, Ord, Enum, Num, Integral, Real, Show)

instance Monad m => Serial m Latticeint where
    series = generate $ flip take $ map Latticeint $
            pure (*) <*> [2 .. 6] <*> [2 .. 6]

instance Monoid Latticeint where
    mempty = join (*) $ foldr lcm 1 [1 .. 6]
    mappend = gcd

instance Monoid' Latticeint where
    mempty' = join (*) $ foldr lcm 1 [1 .. 6]
    mappend' x y = if gcd x y == 1 then Nothing else Just $ gcd x y

