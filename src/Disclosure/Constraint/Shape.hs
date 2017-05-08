{-|
Module      : Disclosure.Constraint.Shape
Description : Datatypes for ranges of individual suit lengths and hand shapes
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines the type 'ShapeRange' for a validated range of hand shapes, as a 4-tuple
of 'Disclosure.Base.BRange's of 'Suit's, which are a newtype of 'Int' bounded
within [@0@, @13@].

Hand ranges are conceptualized as a combination of any number of ≤ and ≥
constraints on individual suit lengths, represented by the
'Disclosure.Base.BRange's in order, with normalization according to one of two
schema:

* /Closed/ form, corresponding to 'Shape', infers all derived constraints based
on the requirement that a hand contain exactly 13 cards, and presents the most
restrictive form. For example, ≥5♠ implies ≤8♥, ≤8♦, and ≤8♣ simultaneously.

* /Open/ form, corresponding to 'minShape', conceptually uses the minimal number
of individual constraints such that the full shape is derivable. Note that the
intent is for clarity of human viewing and the normalization rules reflect that.
For example, knowing =6♠ and =6♥ is sufficient to derive 6♠ 6♥ 0-1♦ 0-1♣; ≤1♦,
≤1♣ are human-redundant.
-}
module Disclosure.Constraint.Shape (
-- * Single suit range datatypes
  Suit
, toSuit
, unSuit
, SuitRange
-- ** Convenience constructors
, toSuitRange
, suitEQ
, suitLE
, suitGE
-- * Hand shape range datatype
, Shape
, toShape
, unShape
, minShape
-- ** Convenience constructors
, shapeHand
, shapeHand'
, shapeS
, shapeH
, shapeD
, shapeC
) where

import Data.List
import Data.Tuple.Curry (curryN) --tuple
import Data.Tuple.Sequence (sequenceT) --tuple
import Data.Tuple.Homogenous (Tuple4(..)) --tuples-homogenous-h98
import Disclosure.Base.Util
import Disclosure.Constraint.Shape.Internal

{-| Prints all nontrivially constrained suits in rank order (reverse
alphabetical) as 'Disclosure.Base.BRange's, followed by their (filled) unicode
suit symbols, separated by spaces. A universal 'Shape' becomes @\"?\"@.
-}
instance Show Shape where
    {-# INLINABLE show #-}
    show = intercalate " " . defaultL "?" . filter (\x -> head x /= '?')
         . zipWith (flip (++)) ["♠", "♥", "♦", "♣"]
         . map show . foldr (:) [] . minShape
         where defaultL y x = if null x then [y] else x

-- | Constructs, validates, and normalizes a 'Shape' from 4 'SuitRange's
shapeHand :: SuitRange -> SuitRange -> SuitRange -> SuitRange -> Maybe Shape
{-# INLINABLE shapeHand #-}
shapeHand = curryN $ toShape . Tuple4

-- | Constructs, validates, and normalizes a 'Shape' from 4 'Maybe' 'SuitRange's
shapeHand' :: Maybe SuitRange -> Maybe SuitRange
           -> Maybe SuitRange -> Maybe SuitRange -> Maybe Shape
{-# INLINABLE shapeHand' #-}
shapeHand' = _''' ((>>= toShape . Tuple4) . sequenceT) . (,,,)

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♠
shapeS :: SuitRange -> Maybe Shape
{-# INLINABLE shapeS #-}
shapeS x = shapeHand x e e e where e = mempty'

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♥
shapeH :: SuitRange -> Maybe Shape
{-# INLINABLE shapeH #-}
shapeH x = shapeHand e x e e where e = mempty'

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♦
shapeD :: SuitRange -> Maybe Shape
{-# INLINABLE shapeD #-}
shapeD x = shapeHand e e x e where e = mempty'

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♣
shapeC :: SuitRange -> Maybe Shape
{-# INLINABLE shapeC #-}
shapeC = shapeHand e e e where e = mempty'
