{-|
Module      : Disclosure.Base.Range.Internal
Description : Internal datatype definitions and functions
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable
-}
module Disclosure.Base.Range.Internal where

import Data.Ord
import Data.Tuple
import Control.Monad
import Disclosure.Base.Util

{-| A validated, nonempty closed interval over an ordered type, represented as a
boxed @('Maybe' a, 'Maybe' a)@ for [low, high]. The 'Maybe' injects 'Nothing' as
a representation of infinity for half-open or open intervals. Ranges are valid
if low ≤ high; empty ranges are invalid.
-}
newtype URange a = URange {
    -- | Unboxes a 'URange'
    unURange :: (Maybe a, Maybe a) } deriving Eq

-- | Constructs and validates a 'URange'
toURange :: Ord a => (Maybe a, Maybe a) -> Maybe (URange a)
{-# INLINABLE toURange #-}
toURange = fmap URange . valURangeR
    where valURangeR = toMaybe (\(x, y) -> maybe True id (liftM2 (<=) x y))

{-| A validated, nonempty closed interval over a bounded, ordered type,
represented as a boxed @(a, a)@ for [low, high]. Ranges are valid if @minBound@
≤ low ≤ high ≤ @maxBound@, and @minBound@, @maxBound@ are functionally
infinities; empty ranges are invalid.
-}
newtype BRange a = BRange {
    -- | Unboxes a 'BRange'
    unBRange :: (a, a) } deriving Eq

-- | Constructs and validates a 'BRange'
toBRange :: (Bounded a, Ord a) => (a, a) -> Maybe (BRange a)
{-# INLINABLE toBRange #-}
toBRange = fmap BRange . valBRangeR
    where valBRangeR = toMaybe (\(x, y) -> x >= minBound && x <= maxBound &&
                                y >= minBound && y <= maxBound && x <= y)

{-|
* (-∞, ∞) or the universal range becomes @\"?\"@

* [@x@, @x@] or a singleton range becomes @\"x\"@

* (-∞, @x@] or a left-infinite range becomes @\"x-\"@

* [@x@, ∞) or a right-infinite range becomes @\"x+\"@

* All other [@x@, @y@] ranges become @\"x–y\"@, note @&ndash;@
-}
instance (Eq a, Show a) => Show (URange a) where
    {-# INLINABLE show #-}
    show (URange (mx, my))
        | mx == my = maybe "?" show mx
        | Nothing <- mx, Just y <- my = show y ++ "-"
        | Just x <- mx, Nothing <- my = show x ++ "+"
        | Just x <- mx, Just y <- my, otherwise = show x ++ "–" ++ show y
        | otherwise = undefined -- Nothings are equal

{-|
* [@minBound@, @maxBound@] or the universal range becomes @\"?\"@

* [@x@, @x@] or a singleton range becomes @\"x\"@

* [@minBound@, @x@] or a left-infinite range becomes @\"x-\"@

* [@x@, @maxBound@] or a right-infinite range becomes @\"x+\"@

* All other [@x@, @y@] ranges become @\"x–y\"@, note @&ndash;@
-}
instance (Bounded a, Eq a, Show a) => Show (BRange a) where
    {-# INLINABLE show #-}
    show (BRange (x, y))
        | x == minBound && y == maxBound = "?"
        | x == y = show x
        | x == minBound = show y ++ "-"
        | y == maxBound = show x ++ "+"
        | otherwise = show x ++ "–" ++ show y

-- | Linear extension of the subset inclusion ordering. Compares upper bounds
-- under normal ≤ ordering, lexicographically lower bounds under reversed ≥
-- ordering.
instance Ord a => Ord (URange a) where
    {-# INLINABLE compare #-}
    compare = comparing (fmap Down . swap . fmap NLast . unURange)

-- | Linear extension of the subset inclusion ordering. Compares upper bounds
-- under normal ≤ ordering, lexicographically lower bounds under reversed ≥
-- ordering.
instance Ord a => Ord (BRange a) where
    {-# INLINABLE compare #-}
    compare = comparing (fmap Down . swap . unBRange)

-- | The commutative operation, which may fail if the result is empty,
-- intersects the two ranges. Identity is the universal range.
instance Ord a => Monoid' (URange a) where
    {-# INLINABLE mempty' #-}
    mempty' = URange (Nothing, Nothing)
    {-# INLINABLE mappend' #-}
    mappend' = liftN2 unURange toURange intURange where
        intURange (l, h) (l', h') = (liftAbsorb2 max l l', liftAbsorb2 min h h')

-- | The commutative operation, which may fail if the result is empty,
-- intersects the two ranges. Identity is the universal range.
instance (Bounded a, Ord a) => Monoid' (BRange a) where
    {-# INLINABLE mempty' #-}
    mempty' = BRange (minBound, maxBound)
    {-# INLINABLE mappend' #-}
    mappend' = liftN2 unBRange toBRange intBRange where
        intBRange (l, h) (l', h') = (max l l', min h h')

{-| Takes the permissive union: for two ranges [@l@, @h@] and [@l@', @h@'], the
result is [@min l l@', @max h h@'].

See also 'Disclosure.Base.CompoundRange.unionURange' for true union.
-}
punionUR :: Ord a => URange a -> URange a -> URange a
{-# INLINABLE punionUR #-}
punionUR (URange (l, h)) (URange (l', h')) =
    URange (min l l', liftN2 NLast unNLast max h h')

{-| Takes the permissive union: for two ranges [@l@, @h@] and [@l@', @h@'], the
result is [@min l l@', @max h h@'].

See also 'Disclosure.Base.CompoundRange.unionBRange' for true union.
-}
punionBR :: Ord a => BRange a -> BRange a -> BRange a
{-# INLINABLE punionBR #-}
punionBR (BRange (l, h)) (BRange (l', h')) = BRange (min l l', max h h')
