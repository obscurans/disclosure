{-|
Module      : Disclosure.Constraint.Range
Description : Datatypes for strength ranges
Copyright   : (c) 2016 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines types for unbounded and bounded ranges, which are closed intervals over
ordered types, represented by a 'Tuple2'. For unbounded ranges, interval bounds
are wrapped in 'Maybe', with 'Nothing' representing the appropriate infinity for
half-open or open ranges. For bounded ranges, 'minBound', 'maxBound' are treated
as infinities.
-}
module Disclosure.Constraint.Range
( URange
, BRange
, toURange
, toBRange
, unURange
, unBRange
, toURange'
, uRangeEQ
, bRangeEQ
, uRangeLE
, bRangeLE
, uRangeGE
, bRangeGE
, intURangeR
, intBRangeR
) where

import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Data.Tuple.Homogenous
import Control.Monad
import Disclosure.Base.Util

newtype Min a = Min { unMin :: Maybe a }
instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mappend (Min Nothing) y = y
    mappend x@(Min (Just _)) (Min Nothing) = x
    mappend (Min (Just x)) (Min (Just y)) = Min $ Just $ min x y

newtype Max a = Max { unMax :: Maybe a }
instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mappend (Max Nothing) y = y
    mappend x@(Max (Just _)) (Max Nothing) = x
    mappend (Max (Just x)) (Max (Just y)) = Max $ Just $ max x y

{-| A validated closed interval over an ordered type, represented as a boxed
@'Maybe' ('Tuple2' ('Maybe' a))@ for [low, high]. The inner 'Maybe' injects
'Nothing' as a representation of infinity for half-open or open intervals.
Ranges are valid if low ≤ high. Invalid and\/or empty ranges are (outer)
'Nothing'.
-}
newtype URange a = URange {
    -- | Unboxes a 'URange'
    unURange :: Maybe (Tuple2 (Maybe a)) } deriving (Eq)

{-| A validated closed interval over a bounded, ordered type, represented as a
boxed @'Maybe' ('Tuple2' a)@ for [low, high]. Ranges are valid if @minBound@ ≤
low ≤ high ≤ @maxBound@, and @minBound@, @maxBound@ also stand for infinities.
Invalid and\/or empty ranges are 'Nothing'.
-}
newtype BRange a = BRange {
    -- | Unboxes a 'BRange'
    unBRange :: Maybe (Tuple2 a) } deriving (Eq)

-- | Constructs and validates a 'URange'
toURange :: Ord a => Maybe a -> Maybe a -> URange a
toURange = _'' (URange . valURange) tuple2

valURange :: Ord a => Tuple2 (Maybe a) -> Maybe (Tuple2 (Maybe a))
valURange = toMaybe (\(Tuple2 (x, y)) -> maybe True id (liftM2 (>=) x y))

-- | Constructs and validates a 'BRange'
toBRange :: (Bounded a, Ord a) => a -> a -> BRange a
toBRange = _'' (BRange . valBRange) tuple2

valBRange :: (Bounded a, Ord a) => Tuple2 a -> Maybe (Tuple2 a)
valBRange = toMaybe (\(Tuple2 (x, y)) -> x >= minBound && x <= maxBound &&
                     y >= minBound && y <= maxBound && x <= y)

instance (Eq a, Show a) => Show (URange a) where
    show = maybe "Null" show' . unURange where
        show' (Tuple2 (mx, my))
            | mx == my = maybe "?" show mx
            | Nothing <- mx, Just y <- my = show y ++ "-"
            | Just x <- mx, Nothing <- my = show x ++ "+"
            | Just x <- mx, Just y <- my, otherwise = show x ++ "–" ++ show y

instance (Bounded a, Eq a, Show a) => Show (BRange a) where
    show = maybe "Null" show' . unBRange where
        show' (Tuple2 (x, y))
            | x == minBound && y == maxBound = "?"
            | x == y = show x
            | x == minBound = show y ++ "-"
            | y == maxBound = show x ++ "+"
            | otherwise = show x ++ "–" ++ show y

instance Bounded (URange a) where
    minBound = URange Nothing
    maxBound = URange $ Just $ Tuple2 (Nothing, Nothing)

instance Bounded a => Bounded (BRange a) where
    minBound = BRange Nothing
    maxBound = BRange $ Just $ Tuple2 (minBound, maxBound)

-- | Linear extension of the subset inclusion ordering. Compares upper bounds
-- under normal ≤ ordering, lexicographically lower bounds under reversed ≥
-- ordering.
instance Ord a => Ord (URange a) where
    compare = comparing (fmap (fmap Down . swap . fmap NLast)
                        . fmap untuple2 . unURange)

-- | Linear extension of the subset inclusion ordering. Compares upper bounds
-- under normal ≤ ordering, lexicographically lower bounds under reversed ≥
-- ordering.
instance Ord a => Ord (BRange a) where
    compare = comparing (fmap (fmap Down . swap) . fmap untuple2 . unBRange)

-- | The commutative operation intersects the two ranges. Identity is the
-- universal range.
instance Ord a => Monoid (URange a) where
    mempty = maxBound
    mappend = (liftN2 unURange URange . _'' join . liftM2) intURangeR

-- | Intersection operator for unboxed 'URange'
intURangeR :: Ord a => Tuple2 (Maybe a) -> Tuple2 (Maybe a)
                        -> Maybe (Tuple2 (Maybe a))
intURangeR = (_'' valURange . liftN2 (fmap AbsorbN) (fmap unAbsorbN) . applyA2
              . fmap liftAbsorb2 . Tuple2) (min, max)

-- | The commutative operation intersects the two ranges. Identity is the
-- universal range.
instance (Bounded a, Ord a) => Monoid (BRange a) where
    mempty = maxBound
    mappend = (liftN2 unBRange BRange . _'' join . liftM2) intBRangeR

-- | Intersection operator for unboxed 'BRange'
intBRangeR :: (Bounded a, Ord a) => Tuple2 a -> Tuple2 a -> Maybe (Tuple2 a)
intBRangeR = (_'' valBRange . applyA2 . Tuple2) (max, min)

-- | Constructs and validates a 'URange' for [@x@, @y@]
toURange' :: Ord a => a -> a -> URange a
toURange' = liftN2 Just id toURange

-- | Constructs a 'Range' for [@x@, @x@]
uRangeEQ :: Ord a => a -> URange a
uRangeEQ = join toURange . Just

-- | Constructs and validates a 'BRange' for [@x@, @x@]
bRangeEQ :: (Bounded a, Ord a) => a -> BRange a
bRangeEQ = join toBRange

-- | Constructs a 'URange' for (-∞, @x@]
uRangeLE :: Ord a => a -> URange a
uRangeLE = toURange Nothing . Just

-- | Constructs and validates a 'BRange' for [@minBound@, @x@]
bRangeLE :: (Bounded a, Ord a) => a -> BRange a
bRangeLE = toBRange minBound

-- | Constructs a 'URange' for [@x@, ∞)
uRangeGE :: Ord a => a -> URange a
uRangeGE = flip toURange Nothing . Just

-- | Constructs and validates a 'BRange' for [@x@, @maxBound@]
bRangeGE :: (Bounded a, Ord a) => a -> BRange a
bRangeGE = flip toBRange maxBound

