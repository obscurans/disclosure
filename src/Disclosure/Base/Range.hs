{-|
Module      : Disclosure.Base.Range
Description : Datatypes for unbounded and bounded ranges
Copyright   : (c) 2016 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines types for unbounded and bounded ranges, which are closed intervals over
ordered types, represented by a 2-tuple. For unbounded ranges, interval bounds
are wrapped in 'Maybe', with 'Nothing' representing the appropriate infinity for
half-open or open ranges. For bounded ranges, 'minBound', 'maxBound' are treated
as infinities.
-}
module Disclosure.Base.Range (
-- * Datatypes and constructors
  URange
, BRange
, toURange
, toBRange
, unURange
, unBRange
-- ** Convenience constructors
, toURange'
, uRangeEQ
, bRangeEQ
, uRangeLE
, bRangeLE
, uRangeGE
, bRangeGE
-- * Set algebra operations
, IntersectUR(..)
, IntersectBR(..)
, PUnionUR(..)
, PUnionBR(..)
-- * Internal unboxed operations
, valURangeR
, valBRangeR
, showURangeR
, showBRangeR
, intURangeR
, intBRangeR
, unsafeIntURangeR
, unsafeIntBRangeR
, punURangeR
, punBRangeR
, unsafePunURangeR
, unsafePunBRangeR
) where

import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Control.Monad
import Disclosure.Base.Util

{-| A validated closed interval over an ordered type, represented as a boxed
@'Maybe' ('Maybe' a, 'Maybe' a)@ for [low, high]. The inner 'Maybe' injects
'Nothing' as a representation of infinity for half-open or open intervals.
Ranges are valid if low ≤ high. Invalid and\/or empty ranges are (outer)
'Nothing'.
-}
newtype URange a = URange {
    -- | Unboxes a 'URange'
    unURange :: Maybe (Maybe a, Maybe a) } deriving Eq

{-| A validated closed interval over a bounded, ordered type, represented as a
boxed @'Maybe' (a, a)@ for [low, high]. Ranges are valid if @minBound@ ≤ low ≤
high ≤ @maxBound@, and @minBound@, @maxBound@ are functionally infinities.
Invalid and\/or empty ranges are 'Nothing'.
-}
newtype BRange a = BRange {
    -- | Unboxes a 'BRange'
    unBRange :: Maybe (a, a) } deriving Eq

-- | Constructs and validates a 'URange'
toURange :: Ord a => Maybe a -> Maybe a -> URange a
{-# INLINABLE toURange #-}
toURange = _'' (URange . valURangeR) (,)

-- | Validates an unboxed 'URange'
valURangeR :: Ord a => (Maybe a, Maybe a) -> Maybe (Maybe a, Maybe a)
{-# INLINABLE valURangeR #-}
valURangeR = toMaybe (\(x, y) -> maybe True id (liftM2 (<=) x y))

-- | Constructs and validates a 'BRange'
toBRange :: (Bounded a, Ord a) => a -> a -> BRange a
{-# INLINABLE toBRange #-}
toBRange = _'' (BRange . valBRangeR) (,)

-- | Validates an unboxed 'BRange'
valBRangeR :: (Bounded a, Ord a) => (a, a) -> Maybe (a, a)
{-# INLINABLE valBRangeR #-}
valBRangeR = toMaybe (\(x, y) -> x >= minBound && x <= maxBound &&
                     y >= minBound && y <= maxBound && x <= y)

{-|
* An invalid and\/or empty range becomes @\"Null\"@

* (-∞, ∞) or the universal range becomes @\"?\"@

* [@x@, @x@] or a singleton range becomes @\"x\"@

* (-∞, @x@] or a left-infinite range becomes @\"x-\"@

* [@x@, ∞) or a right-infinite range becomes @\"x+\"@

* All other [@x@, @y@] ranges become @\"x–y\"@, note @&ndash;@
-}
instance (Eq a, Show a) => Show (URange a) where
    {-# INLINABLE show #-}
    show = maybe "Null" showURangeR . unURange

-- | 'Show's an unboxed 'URange'
showURangeR :: (Eq a, Show a) => (Maybe a, Maybe a) -> String
{-# INLINABLE showURangeR #-}
showURangeR (mx, my)
    | mx == my = maybe "?" show mx
    | Nothing <- mx, Just y <- my = show y ++ "-"
    | Just x <- mx, Nothing <- my = show x ++ "+"
    | Just x <- mx, Just y <- my, otherwise = show x ++ "–" ++ show y
    | otherwise = undefined -- Nothings are equal

{-|
* An invalid and\/or empty range becomes @\"Null\"@

* [@minBound@, @maxBound@] or the universal range becomes @\"?\"@

* [@x@, @x@] or a singleton range becomes @\"x\"@

* [@minBound@, @x@] or a left-infinite range becomes @\"x-\"@

* [@x@, @maxBound@] or a right-infinite range becomes @\"x+\"@

* All other [@x@, @y@] ranges become @\"x–y\"@, note @&ndash;@
-}
instance (Bounded a, Eq a, Show a) => Show (BRange a) where
    {-# INLINABLE show #-}
    show = maybe "Null" showBRangeR . unBRange

-- | 'Show's an unboxed 'BRange'
showBRangeR :: (Bounded a, Eq a, Show a) => (a, a) -> String
{-# INLINABLE showBRangeR #-}
showBRangeR (x, y)
    | x == minBound && y == maxBound = "?"
    | x == y = show x
    | x == minBound = show y ++ "-"
    | y == maxBound = show x ++ "+"
    | otherwise = show x ++ "–" ++ show y

instance Bounded (URange a) where
    {-# INLINABLE minBound #-}
    minBound = URange Nothing
    {-# INLINABLE maxBound #-}
    maxBound = URange $ Just (Nothing, Nothing)

instance Bounded a => Bounded (BRange a) where
    {-# INLINABLE minBound #-}
    minBound = BRange Nothing
    {-# INLINABLE maxBound #-}
    maxBound = BRange $ Just (minBound, maxBound)

-- | Linear extension of the subset inclusion ordering. Compares upper bounds
-- under normal ≤ ordering, lexicographically lower bounds under reversed ≥
-- ordering.
instance Ord a => Ord (URange a) where
    {-# INLINABLE compare #-}
    compare = comparing (fmap (fmap Down . swap . fmap NLast) . unURange)

-- | Linear extension of the subset inclusion ordering. Compares upper bounds
-- under normal ≤ ordering, lexicographically lower bounds under reversed ≥
-- ordering.
instance Ord a => Ord (BRange a) where
    {-# INLINABLE compare #-}
    compare = comparing (fmap (fmap Down . swap) . unBRange)

-- | The commutative operation intersects the two ranges. Identity is the
-- universal range.
instance Ord a => Monoid (URange a) where
    {-# INLINABLE mempty #-}
    mempty = maxBound
    {-# INLINABLE mappend #-}
    mappend = (liftN2 unURange URange . _'' join . liftM2) intURangeR

-- | Intersection operator for unboxed 'URange'
intURangeR :: Ord a => (Maybe a, Maybe a) -> (Maybe a, Maybe a)
                    -> Maybe (Maybe a, Maybe a)
{-# INLINABLE intURangeR #-}
intURangeR = _' valURangeR . unsafeIntURangeR

-- | __UNCHECKED__ intersection operator for unboxed 'URange'
unsafeIntURangeR :: Ord a => (Maybe a, Maybe a) -> (Maybe a, Maybe a)
                          -> (Maybe a, Maybe a)
{-# INLINABLE unsafeIntURangeR #-}
unsafeIntURangeR (l, h) (l', h') = (liftAbsorb2 max l l', liftAbsorb2 min h h')

-- | The commutative operation intersects the two ranges. Identity is the
-- universal range.
instance (Bounded a, Ord a) => Monoid (BRange a) where
    {-# INLINABLE mempty #-}
    mempty = maxBound
    {-# INLINABLE mappend #-}
    mappend = (liftN2 unBRange BRange . _'' join . liftM2) intBRangeR

-- | Intersection operator for unboxed 'BRange'
intBRangeR :: (Bounded a, Ord a) => (a, a) -> (a, a) -> Maybe (a, a)
{-# INLINABLE intBRangeR #-}
intBRangeR = _' valBRangeR . unsafeIntBRangeR

-- | __UNCHECKED__ intersection operator for unboxed 'BRange'
unsafeIntBRangeR :: Ord a => (a, a) -> (a, a) -> (a, a)
{-# INLINABLE unsafeIntBRangeR #-}
unsafeIntBRangeR (l, h) (l', h') = (max l l', min h h')

-- | Newtype wrapper on a 'URange' whose 'Ord'ering is subset inclusion, for
-- testing of inclusion. Note that 'Eq' is inherited directly and __IS
-- INCONSISTENT__ with 'Ord' on incomparable ranges.
newtype IntersectUR a = IntersectUR { unIntersectUR :: URange a } deriving Eq

-- | True subset inclusion ordering, where 'EQ' denotes true equality or
-- incomparability
instance Ord a => Ord (IntersectUR a) where
    {-# INLINABLE compare #-}
    compare (IntersectUR x) (IntersectUR y)
        | x == y = EQ
        | intersection == x = LT
        | intersection == y = GT
        | otherwise = EQ
        where intersection = mappend x y

-- | Newtype wrapper on a 'BRange' whose 'Ord'ering is subset inclusion, for
-- testing of inclusion. Note that 'Eq' is inherited directly and __IS
-- INCONSISTENT__ with 'Ord' on incomparable ranges.
newtype IntersectBR a = IntersectBR { unIntersectBR :: BRange a} deriving Eq

-- | True subset inclusion ordering, where 'EQ' denotes true equality or
-- incomparability
instance (Bounded a, Ord a) => Ord (IntersectBR a) where
    {-# INLINABLE compare #-}
    compare (IntersectBR x) (IntersectBR y)
        | x == y = EQ
        | intersection == x = LT
        | intersection == y = GT
        | otherwise = EQ
        where intersection = mappend x y

-- | Newtype wrapper on a 'URange' whose commutative 'Monoid' is permissive
-- union: for two ranges [@l@, @h@] and [@l@', @h@'], the result is [@min l l@',
-- @max h h@']. Identity is the empty range.
newtype PUnionUR a = PUnionUR { unPUnionUR :: URange a } deriving Eq

instance Ord a => Monoid (PUnionUR a) where
    {-# INLINABLE mempty #-}
    mempty = PUnionUR minBound
    {-# INLINABLE mappend #-}
    mappend = (liftN2 (unURange . unPUnionUR) (PUnionUR . URange)
              . liftAbsorbM2) punURangeR

-- | Permissive union operator for unboxed 'URange'
punURangeR :: Ord a => (Maybe a, Maybe a) -> (Maybe a, Maybe a)
                    -> Maybe (Maybe a, Maybe a)
{-# INLINABLE punURangeR #-}
punURangeR = _' valURangeR . unsafePunURangeR

-- | __UNCHECKED__ permissive union operator for unboxed 'URange'
unsafePunURangeR :: Ord a => (Maybe a, Maybe a) -> (Maybe a, Maybe a)
                          -> (Maybe a, Maybe a)
{-# INLINABLE unsafePunURangeR #-}
unsafePunURangeR (l, h) (l', h') = (min l l', liftN2 NLast unNLast max h h')

-- | Newtype wrapper on a 'BRange' whose commutative 'Monoid' is permissive
-- union: for two ranges [@l@, @h@] and [@l@', @h@'], the result is [@min l l@',
-- @max h h@']. Identity is the empty range.
newtype PUnionBR a = PUnionBR { unPUnionBR :: BRange a } deriving Eq

instance (Bounded a, Ord a) => Monoid (PUnionBR a) where
    {-# INLINABLE mempty #-}
    mempty = PUnionBR minBound
    {-# INLINABLE mappend #-}
    mappend = (liftN2 (unBRange . unPUnionBR) (PUnionBR . BRange)
              . liftAbsorbM2) punBRangeR

-- | Permissive union operator for unboxed 'BRange'
punBRangeR :: (Bounded a, Ord a) => (a, a) -> (a, a) -> Maybe (a, a)
{-# INLINABLE punBRangeR #-}
punBRangeR = _' valBRangeR . unsafePunBRangeR

-- | __UNCHECKED__ permissive union operator for unboxed 'BRange'
unsafePunBRangeR :: Ord a => (a, a) -> (a, a) -> (a, a)
{-# INLINABLE unsafePunBRangeR #-}
unsafePunBRangeR (l, h) (l', h') = (min l l', max h h')

-- | Constructs and validates a 'URange' for [@x@, @y@]
toURange' :: Ord a => a -> a -> URange a
{-# INLINABLE toURange' #-}
toURange' = liftN2 Just id toURange

-- | Constructs a 'URange' for [@x@, @x@]
uRangeEQ :: Ord a => a -> URange a
{-# INLINABLE uRangeEQ #-}
uRangeEQ = join toURange . Just

-- | Constructs and validates a 'BRange' for [@x@, @x@]
bRangeEQ :: (Bounded a, Ord a) => a -> BRange a
{-# INLINABLE bRangeEQ #-}
bRangeEQ = join toBRange

-- | Constructs a 'URange' for (-∞, @x@]
uRangeLE :: Ord a => a -> URange a
{-# INLINABLE uRangeLE #-}
uRangeLE = toURange Nothing . Just

-- | Constructs and validates a 'BRange' for [@minBound@, @x@]
bRangeLE :: (Bounded a, Ord a) => a -> BRange a
{-# INLINABLE bRangeLE #-}
bRangeLE = toBRange minBound

-- | Constructs a 'URange' for [@x@, ∞)
uRangeGE :: Ord a => a -> URange a
{-# INLINABLE uRangeGE #-}
uRangeGE = flip toURange Nothing . Just

-- | Constructs and validates a 'BRange' for [@x@, @maxBound@]
bRangeGE :: (Bounded a, Ord a) => a -> BRange a
{-# INLINABLE bRangeGE #-}
bRangeGE = flip toBRange maxBound

