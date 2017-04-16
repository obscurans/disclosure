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

