{-|
Module      : Disclosure.Base.CompoundRange
Description : Datatypes for unbounded and bounded compound ranges
Copyright   : (c) 2016-2017 Jeffrey Tsang
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
, liftURange
, liftBRange
, unionURange
, unionBRange
-- * Set algebra operations
, compareMeet
, unionCUR
, unionCBR
, punionCUR
, punionCBR
-- * Transformation operations
, collapseCUR
, collapseCBR
, transformCUR
, transformCBR
) where

import Disclosure.Base.Util
import Disclosure.Base.Range
import Disclosure.Base.CompoundRange.Internal

{-| Lifts a single 'URange' into a 'CURange' consisting of that one interval.

prop> punionCBR . liftBRange = Just
-}
liftURange :: Ord a => URange a -> CURange a
{-# INLINABLE liftURange #-}
liftURange = toCURange . (:[])

{-| Lifts a single 'BRange' into a 'CBRange' consisting of that one interval.

prop> punionCBR . liftBRange = Just
-}
liftBRange :: Ord a => BRange a -> CBRange a
{-# INLINABLE liftBRange #-}
liftBRange = toCBRange . (:[])

{-| Unions two 'URange's into a 'CURange' consisting of those two (or one if
overlapping) intervals.

See also 'Disclosure.Base.Range.punionUR' for the permissive union.
-}
unionURange :: Ord a => URange a -> URange a -> CURange a
{-# INLINABLE unionURange #-}
unionURange x y = toCURange [x, y]

{-| Unions two 'BRange's into a 'CBRange' consisting of those two (or one if
overlapping) intervals.

See also 'Disclosure.Base.Range.punionBR' for the permissive union.
-}
unionBRange :: Ord a => BRange a -> BRange a -> CBRange a
{-# INLINABLE unionBRange #-}
unionBRange x y = toCBRange [x, y]

-- | Takes the union of two 'CURange's.
unionCUR :: Ord a => CURange a -> CURange a -> CURange a
{-# INLINABLE unionCUR #-}
unionCUR = liftN2 unCURange toCURange unionCUR'
    where unionCUR' x [] = x
          unionCUR' [] y = y
          unionCUR' xs@(x:xt) ys@(y:yt)
            | x2 < y1 = x : unionCUR' xt ys
            | x1 > y2 = y : unionCUR' xs yt
            | otherwise = (punionUR x y) : unionCUR' xt yt
            where (x1, x2) = unURange x
                  (y1, y2) = unURange y

-- | Takes the union of two 'CBRange's.
unionCBR :: Ord a => CBRange a -> CBRange a -> CBRange a
{-# INLINABLE unionCBR #-}
unionCBR = liftN2 unCBRange toCBRange unionCBR'
    where unionCBR' x [] = x
          unionCBR' [] y = y
          unionCBR' xs@(x:xt) ys@(y:yt)
            | x2 < y1 = x : unionCBR' xt ys
            | x1 > y2 = y : unionCBR' xs yt
            | otherwise = (punionBR x y) : unionCBR' xt yt
            where (x1, x2) = unBRange x
                  (y1, y2) = unBRange y

{-| Maps a function over all subintervals of a 'CURange' and renormalizes.

__PRECONDITION__: the function should be (non-strictly) monotonic increasing,
and infinities are presumed fixed points.
-}
transformCUR :: (Ord a, Ord b) => (a -> b) -> CURange a -> CURange b
{-# INLINABLE transformCUR #-}
transformCUR f = collapseCUR (==) . toCURange . transformCUR' f . unCURange
    where transformCUR' _ [] = []
          transformCUR' f (x:xr) = maybe r (:r) $ toURange (fmap f l, fmap f h)
            where (l, h) = unURange x
                  r = transformCUR' f xr

{-| Maps a function over all subintervals of a 'CBRange' and renormalizes.

__PRECONDITION__: the function should be (non-strictly) monotonic increasing.
-}
transformCBR :: (Ord a, Ord b, Bounded b) => (a -> b) -> CBRange a -> CBRange b
{-# INLINABLE transformCBR #-}
transformCBR f = collapseCBR (==) . toCBRange . transformCBR' f . unCBRange
    where transformCBR' _ [] = []
          transformCBR' f (x:xr) = maybe r (:r) $ toBRange (f l, f h)
            where (l, h) = unBRange x
                  r = transformCBR' f xr
