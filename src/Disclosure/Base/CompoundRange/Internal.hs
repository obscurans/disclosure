{-|
Module      : Disclosure.Base.CompoundRange.Internal
Description : Internal datatype definitions and functions
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable
-}
module Disclosure.Base.CompoundRange.Internal where

import Data.Maybe
import Data.List
import Control.Monad
import Disclosure.Base.Util
import Disclosure.Base.Range

{-| A validated, normalized union of closed intervals over an ordered type,
represented as a boxed ordered @['URange' a]@, see 'URange' for details on each
item.

Normalization requires, in addition to 'URange' validation, that all
subintervals are disjoint (unioned if overlapping), and subintervals are listed
in strictly increasing order.

__TODO__: Investigate 'Ord' instance by linear extension
-}
newtype CURange a = CURange {
    -- | Unboxes a 'CURange'
    unCURange :: [URange a] } deriving Eq

-- | Normalizes and constructs a 'CURange' by enforcing all subintervals are
-- pairwise disjoint (unioned if overlapping), and subintervals are listed in
-- strictly ascending order.
toCURange :: Ord a => [URange a] -> CURange a
{-# INLINABLE toCURange #-}
toCURange = CURange . normCURangeR
    where normCURangeR = foldr insCURangeR []
          insCURangeR x [] = [x]
          insCURangeR x ys@(y:yt)
            | fromMaybe False (liftM2 (<) x2 y1) = x : ys
            | fromMaybe False (liftM2 (>) x1 y2) = y : insCURangeR x yt
            | otherwise = insCURangeR (punionUR x y) yt
            where (x1, x2) = unURange x
                  (y1, y2) = unURange y

{-| A validated, normalized union of closed intervals over an ordered type,
represented as a boxed ordered @['BRange' a]@, see 'BRange'for details on each
item.

Normalization requires, in addition to 'BRange' validation, that all
subintervals are disjoint (unioned if overlapping), and subintervals are listed
in strictly increasing order.

__TODO__: Investigate 'Ord' instance by linear extension
-}
newtype CBRange a = CBRange {
    -- | Unboxes a 'CBRange'
    unCBRange :: [BRange a] } deriving Eq

-- | Normalizes and constructs a 'CBRange' by enforcing all subintervals are
-- pairwise disjoint (unioned if overlapping), and subintervals are listed in
-- strictly ascending order.
toCBRange :: Ord a => [BRange a] -> CBRange a
{-# INLINABLE toCBRange #-}
toCBRange = CBRange . normCBRangeR
    where normCBRangeR = foldr insCBRangeR []
          insCBRangeR x [] = [x]
          insCBRangeR x ys@(y:yt)
            | x2 < y1 = x : ys
            | x1 > y2 = y : insCBRangeR x yt
            | otherwise = insCBRangeR (punionBR x y) yt
            where (x1, x2) = unBRange x
                  (y1, y2) = unBRange y

{-|
* An invalid and\/or empty set of intervals becomes @\"Null\"@

* A single interval is 'show'n as a 'URange'

* Multiple intervals are 'show'n as 'URange's, spaced with @\" or \"@ and
  wrapped with @\"(\"@, @\")\"@.
-}
instance (Eq a, Show a) => Show (CURange a) where
    {-# INLINABLE show #-}
    show (CURange []) = "Null"
    show (CURange [x]) = show x
    show (CURange x) = "(" ++ (intercalate " or " (fmap show x)) ++ ")"

{-|
* An invalid and\/or empty set of intervals becomes @\"Null\"@

* A single interval is 'show'n as a 'BRange'

* Multiple intervals are 'show'n as 'BRange's, spaced with @\" or \"@ and
  wrapped with @\"(\"@, @\")\"@.
-}
instance (Bounded a, Eq a, Show a) => Show (CBRange a) where
    {-# INLINABLE show #-}
    show (CBRange []) = "Null"
    show (CBRange [x]) = show x
    show (CBRange x) = "(" ++ (intercalate " or " (fmap show x)) ++ ")"

-- | The commutative operation intersects the two ranges. Identity is the
-- universal set.
instance Ord a => Monoid (CURange a) where
    {-# INLINABLE mempty #-}
    mempty = CURange [mempty']
    {-# INLINABLE mappend #-}
    mappend = liftN2 unCURange CURange intersectCUR
        where intersectCUR _ [] = []
              intersectCUR [] _ = []
              intersectCUR xs@(x:xt) ys@(y:yt)
                | fromMaybe False (liftM2 (<) x2 y1) = intersectCUR xt ys
                | fromMaybe False (liftM2 (>) x1 y2) = intersectCUR xs yt
                | otherwise = maybe r (:r) $ mappend' x y
                where (x1, x2) = unURange x
                      (y1, y2) = unURange y
                      r = intersectCUR rx ry
                      (rx, ry) = case compare (NLast x2) (NLast y2) of
                                    LT -> (xt, ys)
                                    EQ -> (xt, yt)
                                    GT -> (xs, yt)

-- | The commutative operation intersects the two ranges. Identity is the
-- universal set.
instance (Bounded a, Ord a) => Monoid (CBRange a) where
    {-# INLINABLE mempty #-}
    mempty = CBRange [mempty']
    {-# INLINABLE mappend #-}
    mappend = liftN2 unCBRange CBRange intersectCBR
        where intersectCBR _ [] = []
              intersectCBR [] _ = []
              intersectCBR xs@(x:xt) ys@(y:yt)
                | x2 < y1 = intersectCBR xt ys
                | x1 > y2 = intersectCBR xs yt
                | otherwise = maybe r (:r) $ mappend' x y
                where (x1, x2) = unBRange x
                      (y1, y2) = unBRange y
                      r = intersectCBR rx ry
                      (rx, ry) = case compare x2 y2 of
                                    LT -> (xt, ys)
                                    EQ -> (xt, yt)
                                    GT -> (xs, yt)

-- | Takes the permissive union of all subintervals in a 'CURange': the result
-- is [@min@ all intervals, @max@ all intervals]. Result on an empty 'CURange'
-- is 'Nothing'.
punionCUR :: Ord a => CURange a -> Maybe (URange a)
{-# INLINABLE punionCUR #-}
punionCUR (CURange []) = Nothing
punionCUR (CURange x) = Just $ punionUR (head x) (last x)

-- | Takes the permissive union of all subintervals in a 'CBRange': the result
-- is [@min@ all intervals, @max@ all intervals]. Result on an empty 'CBRange'
-- is 'Nothing'.
punionCBR :: Ord a => CBRange a -> Maybe (BRange a)
{-# INLINABLE punionCBR #-}
punionCBR (CBRange []) = Nothing
punionCBR (CBRange x) = Just $ punionBR (head x) (last x)

-- | Collapses the subintervals of a 'CURange' with a function that determines
-- whether [@_@, @x@] and [@y@, @_@] are \"close enough\" to be treated as
-- touching and combined. Function can assume it will be called with @x < y@.
collapseCUR :: Ord a => (a -> a -> Bool) -> CURange a -> CURange a
{-# INLINABLE collapseCUR #-}
collapseCUR f (CURange x) = CURange $ collapseCUR' f x
    where collapseCUR' _ [] = []
          collapseCUR' _ [x] = [x]
          collapseCUR' f (x:ys@(y:yt))
            | fromMaybe False (liftM2 f x2 y1) =
                                            collapseCUR' f ((punionUR x y) : yt)
            | otherwise = x : collapseCUR' f ys
            where (_, x2) = unURange x
                  (y1, _) = unURange y

-- | Collapses the subintervals of a 'CBRange' with a function that determines
-- whether [@_@, @x@] and [@y@, @_@] are \"close enough\" to be treated as
-- touching and combined. Function can assume it will be called with @x < y@.
collapseCBR :: Ord a => (a -> a -> Bool) -> CBRange a -> CBRange a
{-# INLINABLE collapseCBR #-}
collapseCBR f (CBRange x) = CBRange $ collapseCBR' f x
    where collapseCBR' _ [] = []
          collapseCBR' _ [x] = [x]
          collapseCBR' f (x:ys@(y:yt))
            | f x2 y1 = collapseCBR' f ((punionBR x y) : yt)
            | otherwise = x : collapseCBR' f ys
            where (_, x2) = unBRange x
                  (y1, _) = unBRange y
