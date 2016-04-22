{-|
Module      : Disclosure.Base.CompoundRange
Description : Datatypes for unbounded and bounded compound ranges
Copyright   : (c) 2016 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines types for unbounded and bounded compound ranges, which are Fσ sets
(union of closed intervals) over ordered types, represented by an ordered list
of ranges in "Disclosure.Base.Range".
-}
module Disclosure.Base.CompoundRange (
-- * Datatypes and constructors
  CURange
, CBRange
, toCURange
, toCBRange
, unCURange
, unCBRange
-- ** Convenience constructors
, toCURange'
, singURange
, singBRange
, compURange
, compBRange
-- * Set algebra operations
, colCURange
, colCBRange
, punCURange
, punCBRange
, IntersectCUR(..)
, IntersectCBR(..)
, UnionCUR(..)
, UnionCBR(..)
-- * Transformation operations
, transCURange
, transCBRange
, unsafeTransCURange
, unsafeTransCBRange
-- * Internal unboxed operations
, normCURangeR
, normCBRangeR
, colCURangeR
, colCBRangeR
, punCURangeR
, punCBRangeR
, punCURangeR'
, punCBRangeR'
, insCURangeR
, insCBRangeR
, intCURangeR
, intCBRangeR
, uniCURangeR
, uniCBRangeR
, transCURangeR
, transCBRangeR
) where

import Data.Monoid
import Data.Maybe
import Data.List
import Control.Monad
import Disclosure.Base.Util
import Disclosure.Base.Range

{-| A validated, normalized union of closed intervals over an ordered type,
represented as a boxed ordered @[('Maybe' a, 'Maybe' a)]@, where each item in
the list is [low, high] for a subinterval. The 'Maybe' injects 'Nothing' as a
representation of the appropriate infinity for half-open or open intervals.

Normalization requires that all subintervals are valid (as 'URange's) and
nonempty, all subintervals are disjoint (unioned if overlapping), and
subintervals are listed in strictly increasing order.

__TODO__: Investigate 'Ord' instance by linear extension
-}
newtype CURange a = CURange {
    -- | Unboxes a 'CURange'
    unCURange :: [(Maybe a, Maybe a)] } deriving Eq

{-| A validated, normalized union of closed intervals over an ordered type,
represented as a boxed ordered @[(a, a)]@, where each item in the list is [low,
high] for a subinterval.

Normalization requires that all subintervals are valid (as 'BRange's) and
nonempty, all subintervals are disjoint (unioned if overlapping), and
subintervals are listed in strictly increasing order.

__TODO__: Investigate 'Ord' instance by linear extension
-}
newtype CBRange a = CBRange {
    -- | Unboxes a 'CBRange'
    unCBRange :: [(a, a)] } deriving Eq

-- | Normalizes and constructs a 'CURange' by enforcing all subintervals are
-- valid, subintervals are pairwise disjoint (unioned if overlapping), and
-- subintervals are listed in strictly ascending order.
toCURange :: Ord a => [(Maybe a, Maybe a)] -> CURange a
{-# INLINABLE toCURange #-}
toCURange = CURange . normCURangeR

-- | Normalizes and constructs a 'CURange' with no infinite intervals by
-- enforcing all subintervals are valid, subintervals are pairwise disjoint
-- (unioned if overlapping), and subintervals are listed in strictly ascending
-- order.
toCURange' :: Ord a => [(a, a)] -> CURange a
{-# INLINABLE toCURange' #-}
toCURange' = toCURange . map (\(x, y) -> (Just x, Just y))

-- | Normalizes an unboxed 'CURange', invariant listed in 'toCURange'.
-- Essentially does insertion sort with unioning.
normCURangeR :: Ord a => [(Maybe a, Maybe a)] -> [(Maybe a, Maybe a)]
{-# INLINABLE normCURangeR #-}
normCURangeR = foldr insCURangeR [] . catMaybes . map valURangeR

{-| Inserts a single unboxed 'URange' into an unboxed 'CURange'.

* __PRECONDITION__: assumes inputs are normalized.

* __POSTCONDITION__: output is normalized.
-}
insCURangeR :: Ord a => (Maybe a, Maybe a) -> [(Maybe a, Maybe a)]
                     -> [(Maybe a, Maybe a)]
{-# INLINABLE insCURangeR #-}
insCURangeR x [] = [x]
insCURangeR x@(x1, x2) ys@(y@(y1, y2):yt)
    | maybe False id (liftM2 (<) x2 y1) = x : ys
    | maybe False id (liftM2 (>) x1 y2) = y : insCURangeR x yt
    | otherwise = insCURangeR (unsafePunURangeR x y) yt -- guaranteed nonempty

-- | Normalizes and constructs a 'CBRange' by enforcing all subintervals are
-- valid, subintervals are pairwise disjoint (unioned if overlapping), and
-- subintervals are listed in strictly ascending order.
toCBRange :: (Bounded a, Ord a) => [(a, a)] -> CBRange a
{-# INLINABLE toCBRange #-}
toCBRange = CBRange . normCBRangeR

-- | Normalizes an unboxed 'CBRange', invariant listed in 'toCBRange'.
-- Essentially does insertion sort with unioning.
normCBRangeR :: (Bounded a, Ord a) => [(a, a)] -> [(a, a)]
{-# INLINABLE normCBRangeR #-}
normCBRangeR = foldr insCBRangeR [] . catMaybes . map valBRangeR

{-| Inserts a single unboxed 'BRange' into an unboxed 'CBRange'.

* __PRECONDITION__: assumes inputs are normalized.

* __POSTCONDITION__: output is normalized.
-}
insCBRangeR :: (Bounded a, Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
{-# INLINABLE insCBRangeR #-}
insCBRangeR x [] = [x]
insCBRangeR x@(x1, x2) ys@(y@(y1, y2):yt)
    | x2 < y1 = x : ys
    | x1 > y2 = y : insCBRangeR x yt
    | otherwise = insCBRangeR (unsafePunBRangeR x y) yt -- guaranteed nonempty

{-| Lifts a single 'URange' into a 'CURange' consisting of that one (or zero)
interval. For normalized 'URange',

prop> punCURange . singURange = id
-}
singURange :: URange a -> CURange a
{-# INLINABLE singURange #-}
singURange = CURange . maybeToList . unURange

{-| Lifts a single 'BRange' into a 'CBRange' consisting of that one (or zero)
interval. For normalized 'BRange',

prop> punCBRange . singBRange = id
-}
singBRange :: BRange a -> CBRange a
{-# INLINABLE singBRange #-}
singBRange = CBRange . maybeToList . unBRange

-- | Lifts a list of 'URange's into a 'CURange' consisting of those intervals
compURange :: Ord a => [URange a] -> CURange a
{-# INLINABLE compURange #-}
compURange = toCURange . catMaybes . map unURange

-- | Lifts a list of 'BRange's into a 'CBRange' consisting of those intervals
compBRange :: (Bounded a, Ord a) => [BRange a] -> CBRange a
{-# INLINABLE compBRange #-}
compBRange = toCBRange . catMaybes . map unBRange

{-|
* An invalid and\/or empty set of intervals becomes @\"Null\"@

* A single interval is 'show'n as a 'URange'

* Multiple intervals are 'show'n as 'URange's, spaced with @\" or \"@ and
  wrapped with @\"(\"@, @\")\"@.
-}
instance (Eq a, Show a) => Show (CURange a) where
    {-# INLINABLE show #-}
    show (CURange []) = "Null"
    show (CURange [x]) = showURangeR x
    show (CURange x) = "(" ++ (intercalate " or " (fmap showURangeR x)) ++ ")"

{-|
* An invalid and\/or empty set of intervals becomes @\"Null\"@

* A single interval is 'show'n as a 'BRange'

* Multiple intervals are 'show'n as 'BRange's, spaced with @\" or \"@ and
  wrapped with @\"(\"@, @\")\"@.
-}
instance (Bounded a, Eq a, Show a) => Show (CBRange a) where
    {-# INLINABLE show #-}
    show (CBRange []) = "Null"
    show (CBRange [x]) = showBRangeR x
    show (CBRange x) = "(" ++ (intercalate " or " (fmap showBRangeR x)) ++ ")"

instance Bounded (CURange a) where
    {-# INLINABLE minBound #-}
    minBound = CURange []
    {-# INLINABLE maxBound #-}
    maxBound = CURange [(Nothing, Nothing)]

instance Bounded a => Bounded (CBRange a) where
    {-# INLINABLE minBound #-}
    minBound = CBRange []
    {-# INLINABLE maxBound #-}
    maxBound = CBRange [(minBound, maxBound)]

-- | The commutative operation intersects the two ranges. Identity is the
-- universal set.
instance Ord a => Monoid (CURange a) where
    {-# INLINABLE mempty #-}
    mempty = maxBound
    {-# INLINABLE mappend #-}
    mappend = liftN2 unCURange CURange intCURangeR

{-| Intersects two unboxed 'CURange's.

* __PRECONDITION__: assumes inputs are normalized.

* __POSTCONDITION__: output is normalized.
-}
intCURangeR :: Ord a => [(Maybe a, Maybe a)] -> [(Maybe a, Maybe a)]
                     -> [(Maybe a, Maybe a)]
{-# INLINABLE intCURangeR #-}
intCURangeR _ [] = []
intCURangeR [] _ = []
intCURangeR xs@(x@(x1, x2):xt) ys@(y@(y1, y2):yt)
    | maybe False id (liftM2 (<) x2 y1) = intCURangeR xt ys
    | maybe False id (liftM2 (>) x1 y2) = intCURangeR xs yt
    | otherwise = (unsafeIntURangeR x y) : intCURangeR rx ry -- guaranteed safe
    where (rx, ry) = case compare (NLast x2) (NLast y2) of
                        LT -> (xt, ys)
                        EQ -> (xt, yt)
                        GT -> (xs, yt)

-- | The commutative operation intersects the two ranges. Identity is the
-- universal set.
instance (Bounded a, Ord a) => Monoid (CBRange a) where
    {-# INLINABLE mempty #-}
    mempty = maxBound
    {-# INLINABLE mappend #-}
    mappend = liftN2 unCBRange CBRange intCBRangeR

{-| Intersects two unboxed 'CBRange's.

* __PRECONDITION__: assumes inputs are normalized.

* __POSTCONDITION__: output is normalized.
-}
intCBRangeR :: (Bounded a, Ord a) => [(a, a)] -> [(a, a)] -> [(a, a)]
{-# INLINABLE intCBRangeR #-}
intCBRangeR _ [] = []
intCBRangeR [] _ = []
intCBRangeR xs@(x@(x1, x2):xt) ys@(y@(y1, y2):yt)
    | x2 < y1 = intCBRangeR xt ys
    | x1 > y2 = intCBRangeR xs yt
    | otherwise = (unsafeIntBRangeR x y) : intCBRangeR rx ry -- guaranteed safe
    where (rx, ry) = case compare x2 y2 of
                        LT -> (xt, ys)
                        EQ -> (xt, yt)
                        GT -> (xs, yt)

-- | Collapses the subintervals of a 'CURange' with a function that determines
-- whether [@_@, @x@] and [@y@, @_@] are \"close enough\" to be treated as
-- touching and combined. Function can assume it will be called with @x < y@.
colCURange :: Ord a => (a -> a -> Bool) -> CURange a -> CURange a
{-# INLINABLE colCURange #-}
colCURange f (CURange x) = CURange $ colCURangeR f x

-- | Collapses the subintervals of an unboxed 'CURange', see 'colCURange'
colCURangeR :: Ord a => (a -> a -> Bool) -> [(Maybe a, Maybe a)]
                     -> [(Maybe a, Maybe a)]
{-# INLINABLE colCURangeR #-}
colCURangeR f [] = []
colCURangeR f [x] = [x]
colCURangeR f (x@(x1, x2):ys@(y@(y1, y2):yt))
    | maybe False id (liftM2 f x2 y1) = -- guaranteed nonempty
                        colCURangeR f ((unsafePunURangeR x y) : yt)
    | otherwise = x : colCURangeR f ys

-- | Collapses the subintervals of a 'CBRange' with a function that determines
-- whether [@_@, @x@] and [@y@, @_@] are \"close enough\" to be treated as
-- touching and combined. Function can assume it will be called with @x < y@.
colCBRange :: (Bounded a, Ord a) => (a -> a -> Bool) -> CBRange a -> CBRange a
{-# INLINABLE colCBRange #-}
colCBRange f (CBRange x) = CBRange $ colCBRangeR f x

-- | Collapses the subintervals of an unboxed 'CBRange', see 'colCBRange'
colCBRangeR :: (Bounded a, Ord a) => (a -> a -> Bool) -> [(a, a)] -> [(a, a)]
{-# INLINABLE colCBRangeR #-}
colCBRangeR f [] = []
colCBRangeR f [x] = [x]
colCBRangeR f (x@(x1, x2):ys@(y@(y1, y2):yt))
    | f x2 y1 = colCBRangeR f ((unsafePunBRangeR x y) : yt) -- guaranteed safe
    | otherwise = x : colCBRangeR f ys

{-| Completely collapses a 'CURange' into one single 'URange' by taking the
permissive union: the output range is from the lowest lower bound to the highest
upper bound, discarding all holes inside. For normalized 'URange',

prop> punCURange . singURange = id
-}
punCURange :: Ord a => CURange a -> URange a
{-# INLINABLE punCURange #-}
punCURange = punCURangeR' . unCURange

-- | Takes the permissive union of an unboxed 'CURange', see 'punCURange'
punCURangeR :: Ord a => [(Maybe a, Maybe a)] -> Maybe (Maybe a, Maybe a)
{-# INLINABLE punCURangeR #-}
punCURangeR = unURange . punCURangeR'

-- | Takes the boxed permissive union of an unboxed 'CURange', see 'punCURange'
punCURangeR' :: Ord a => [(Maybe a, Maybe a)] -> URange a
{-# INLINABLE punCURangeR' #-}
punCURangeR' [] = minBound
punCURangeR' x = toURange (minimum l) $ unNLast $ maximum $ map NLast h
    where (l, h) = unzip x

{-| Completely collapses a 'CBRange' into one single 'BRange' by taking the
permissive union: the output range is from the lowest lower bound to the highest
upper bound, discarding all holes inside. For normalized 'BRange',

prop> punCBRange . singBRange = id
-}
punCBRange :: (Bounded a, Ord a) => CBRange a -> BRange a
{-# INLINABLE punCBRange #-}
punCBRange = punCBRangeR' . unCBRange

-- | Takes the permissive union of an unboxed 'CBRange', see 'punCBRange'
punCBRangeR :: (Bounded a, Ord a) => [(a, a)] -> Maybe (a, a)
{-# INLINABLE punCBRangeR #-}
punCBRangeR = unBRange . punCBRangeR'

-- | Takes the boxed permissive union of an unboxed 'CBRange', see 'punCBRange'
punCBRangeR' :: (Bounded a, Ord a) => [(a, a)] -> BRange a
{-# INLINABLE punCBRangeR' #-}
punCBRangeR' [] = minBound
punCBRangeR' x = toBRange (minimum l) (maximum h) where (l, h) = unzip x

-- | Newtype wrapper on a 'CURange' whose 'Ord'ering is subset inclusion, for
-- testing of inclusion. Note that 'Eq' is inherited directly and __IS
-- INCONSISTENT__ with 'Ord' on incomparable ranges.
newtype IntersectCUR a = IntersectCUR { unIntersectCUR :: CURange a }
    deriving Eq

-- | True subset inclusion ordering, where 'EQ' denotes true equality or
-- incomparability
instance (Bounded a, Ord a) => Ord (IntersectCUR a) where
    {-# INLINABLE compare #-}
    compare (IntersectCUR x) (IntersectCUR y)
        | x == y = EQ
        | intersection == x = LT
        | intersection == y = GT
        | otherwise = EQ
        where intersection = mappend x y

-- | Newtype wrapper on a 'CBRange' whose 'Ord'ering is subset inclusion, for
-- testing of inclusion. Note that 'Eq' is inherited directly and __IS
-- INCONSISTENT__ with 'Ord' on incomparable ranges.
newtype IntersectCBR a = IntersectCBR { unIntersectCBR :: CBRange a }
    deriving Eq

-- | True subset inclusion ordering, where 'EQ' denotes true equality or
-- incomparability
instance (Bounded a, Ord a) => Ord (IntersectCBR a) where
    {-# INLINABLE compare #-}
    compare (IntersectCBR x) (IntersectCBR y)
        | x == y = EQ
        | intersection == x = LT
        | intersection == y = GT
        | otherwise = EQ
        where intersection = mappend x y

-- | Newtype wrapper for a 'CURange' whose commutative 'Monoid' is set union,
-- with identity being the empty set.
newtype UnionCUR a = UnionCUR { unUnionCUR :: CURange a } deriving Eq

instance Ord a => Monoid (UnionCUR a) where
    {-# INLINABLE mempty #-}
    mempty = UnionCUR minBound
    {-# INLINABLE mappend #-}
    mappend = liftN2 (unCURange . unUnionCUR) (UnionCUR . CURange) uniCURangeR

{-| Unions two unboxed 'CURange's.

* __PRECONDITION__: assumes inputs are normalized.

* __POSTCONDITION__: output is normalized.
-}
uniCURangeR :: Ord a => [(Maybe a, Maybe a)] -> [(Maybe a, Maybe a)]
                     -> [(Maybe a, Maybe a)]
{-# INLINABLE uniCURangeR #-}
uniCURangeR x [] = x
uniCURangeR [] y = y
uniCURangeR xs@(x@(x1, x2):xt) ys@(y@(y1, y2):yt)
    | maybe False id (liftM2 (<) x2 y1) = x : uniCURangeR xt ys
    | maybe False id (liftM2 (>) x1 y2) = y : uniCURangeR xs yt
    | otherwise = uniCURangeR rx ry
    where (rx, ry) = if NLast x2 < NLast y2 then (xt, c:yt) else (c:xt, yt)
          c = unsafePunURangeR x y -- guaranteed nonempty

-- | Newtype wrapper for a 'CBRange' whose commutative 'Monoid' is set union,
-- with identity being the empty set.
newtype UnionCBR a = UnionCBR { unUnionCBR :: CBRange a } deriving Eq

instance (Bounded a, Ord a) => Monoid (UnionCBR a) where
    {-# INLINABLE mempty #-}
    mempty = UnionCBR minBound
    {-# INLINABLE mappend #-}
    mappend = liftN2 (unCBRange . unUnionCBR) (UnionCBR . CBRange) uniCBRangeR

{-| Unions two unboxed 'CBRange's.

* __PRECONDITION__: assumes inputs are normalized.

* __POSTCONDITION__: output is normalized.
-}
uniCBRangeR :: Ord a => [(a, a)] -> [(a, a)] -> [(a, a)]
{-# INLINABLE uniCBRangeR #-}
uniCBRangeR x [] = x
uniCBRangeR [] y = y
uniCBRangeR xs@(x@(x1, x2):xt) ys@(y@(y1, y2):yt)
    | x2 < y1 = x : uniCBRangeR xt ys
    | x1 > y2 = y : uniCBRangeR xs yt
    | otherwise = uniCBRangeR rx ry
    where (rx, ry) = if x2 < y2 then (xt, c:yt) else (c:xt, yt)
          c = unsafePunBRangeR x y -- guaranteed nonempty

-- | Maps a function over all subintervals of a 'CURange', and renormalizes.
transCURange :: (Ord a, Ord b) => (a -> b) -> CURange a -> CURange b
{-# INLINABLE transCURange #-}
transCURange f = toCURange . transCURangeR f . unCURange

{-| Maps a function, presumed monotonic wrt orderings on @a@ and @b@, over all
subintervals of a 'CURange'. Does __NOT__ renormalize.

* __PRECONDITION__: @compare x y == compare (f x) (f y)@

* __POSTCONDITION__: output is normalized.
-}
unsafeTransCURange :: (Ord a, Ord b) => (a -> b) -> CURange a -> CURange b
{-# INLINABLE unsafeTransCURange #-}
unsafeTransCURange f = CURange . transCURangeR f . unCURange

{-| Maps a function, presumed monotonic wrt orderings on @a@ and @b@, over all
subintervals of an unboxed 'CURange'.

* __PRECONDITION__: @compare x y == compare (f x) (f y)@, assumes input is
normalized.

* __POSTCONDITION__: output is normalized.
-}
transCURangeR :: (a -> b) -> [(Maybe a, Maybe a)] -> [(Maybe b, Maybe b)]
{-# INLINABLE transCURangeR #-}
transCURangeR _ [] = []
transCURangeR f ((l, h):xr) = (fmap f l, fmap f h) : transCURangeR f xr

-- | Maps a function over all subintervals of a 'CBRange', and renormalizes.
transCBRange :: (Ord a, Bounded b, Ord b) => (a -> b) -> CBRange a -> CBRange b
{-# INLINABLE transCBRange #-}
transCBRange f = toCBRange . transCBRangeR f . unCBRange

{-| Maps a function, presumed monotonic wrt orderings on @a@ and @b@, over all
subintervals of a 'CBRange'. Does __NOT__ renormalize.

* __PRECONDITION__: @compare x y == compare (f x) (f y)@

* __POSTCONDITION__: output is normalized.
-}
unsafeTransCBRange :: (Ord a, Ord b) => (a -> b) -> CBRange a -> CBRange b
{-# INLINABLE unsafeTransCBRange #-}
unsafeTransCBRange f = CBRange . transCBRangeR f . unCBRange

{-| Maps a function, presumed monotonic wrt orderings on @a@ and @b@, over all
subintervals of an unboxed 'CBRange'.

* __PRECONDITION__: @compare x y == compare (f x) (f y)@, assumes input is
normalized.

* __POSTCONDITION__: output is normalized.
-}
transCBRangeR :: (a -> b) -> [(a, a)] -> [(b, b)]
{-# INLINABLE transCBRangeR #-}
transCBRangeR _ [] = []
transCBRangeR f ((l, h):xr) = (f l, f h) : transCBRangeR f xr

