{-|
Module      : Disclosure.Constraint.Shape
Description : Datatypes for ranges of individual suit lengths and hand shapes
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines three types: 'SuitRange' for a validated range of lengths in an
unspecified suit, as a closed interval of integers within [0, 13]; 'ShapeRange'
for a validated range of hand shapes, as a 4-tuple of 'SuitRange's;
'ShapeMinimal' is also a range of hand shapes but is minimized for
human-readibility.

Hand ranges are conceptualized as a combination of any number of ≤ and ≥
constraints on individual suit lengths, represented by the 'SuitRange's, with
normalization according to one of two schema:

* /Closed/ form, corresponding to 'ShapeRange', infers all derived constraints
based on the requirement that a hand contain exactly 13 cards, and presents the
most restrictive form. For example, ≥5♠ implies ≤8♥, ≤8♦, and ≤8♣
simultaneously.

* /Open/ form, corresponding to 'ShapeMinimal', conceptually uses the minimal
number of individual constraints such that the full shape is derivable. Note
that the intent is for clarity of human viewing and the normalization rules
reflect that. For example, knowing =6♠ and =6♥ is sufficient to derive 6♠ 6♥
0-1♦ 0-1♣; ≤1♦ is human-redundant.

'ShapeRange' is isomorphic to 'ShapeMinimal'.
-}
module Disclosure.Constraint.Shape (
-- * Single suit range datatypes
  SuitInt(..)
, SuitNum
, SuitRange
, toSuitR
, unSuitR
-- ** Convenience constructors
, toSuitR'
, suitEQ
, suitLE
, suitGE
-- * Hand shape range datatypes
, ShapeNum
, ShapeRange
, ShapeMinimal
, toShapeR
, toShapeM
, unShapeR
, unShapeM
, minShapeR
, expShapeM
-- ** Convenience constructors
, shapeHand
, shapeS
, shapeH
, shapeD
, shapeC
) where

import Data.List
import Data.Monoid
import Data.Tuple.Curry --tuple
import Data.Tuple.Sequence --tuple
import Data.Tuple.Homogenous --tuples-homogenous-h98
import Control.Applicative
import Control.Monad
import Disclosure.Base.Util
import Disclosure.Base.Range

-- | Wrapper over 'Int' which is 'Bounded' to [@0@, @13@]. Performs no bounds
-- checking.
newtype SuitInt = SuitInt { unSuitInt :: Int }
    deriving (Eq, Ord)

instance Bounded SuitInt where
    {-# INLINABLE minBound #-}
    minBound = SuitInt 0
    {-# INLINABLE maxBound #-}
    maxBound = SuitInt 13

-- | Directly as 'Int'
instance Show SuitInt where
    {-# INLINABLE show #-}
    show = show . unSuitInt

-- | Directly as 'Int'
instance Read SuitInt where
    {-# INLINABLE readsPrec #-}
    readsPrec = _'' (map (\(x, y) -> (SuitInt x, y))) readsPrec

-- | A range of lengths [low, high] in an unspecified suit. Trivial\/nonexistent
-- constraints are represented by low = 0 and\/or high = 13.
type SuitNum = (SuitInt, SuitInt)

{-| A validated range of lengths in an unspecified suit, as a boxed @'Maybe'
'SuitNum'@.

Ranges are valid if 0 ≤ low ≤ high ≤ 13. Invalid and\/or empty ranges are
'Nothing'.
-}
type SuitRange = BRange SuitInt

-- | Constructs and validates a 'SuitRange'
toSuitR :: SuitInt -> SuitInt -> SuitRange
{-# INLINABLE toSuitR #-}
toSuitR = toBRange

-- | Unboxes a 'SuitRange'
unSuitR :: SuitRange -> Maybe SuitNum
{-# INLINABLE unSuitR #-}
unSuitR = unBRange

valSuitN :: SuitNum -> Maybe SuitNum
{-# INLINABLE valSuitN #-}
valSuitN = valBRangeR

showSuitN :: SuitNum -> String
{-# INLINABLE showSuitN #-}
showSuitN = showBRangeR

intSuitN :: SuitNum -> SuitNum -> Maybe SuitNum
{-# INLINABLE intSuitN #-}
intSuitN = intBRangeR

-- | Constructs and validates a 'SuitRange' for [@x@, @y@]
toSuitR' :: Int -> Int -> SuitRange
{-# INLINABLE toSuitR' #-}
toSuitR' = liftN2 SuitInt id toSuitR

-- | Constructs and validates a 'SuitRange' for =@x@
suitEQ :: Int -> SuitRange
{-# INLINABLE suitEQ #-}
suitEQ = bRangeEQ . SuitInt

-- | Constructs and validates a 'SuitRange' for ≤@x@
suitLE :: Int -> SuitRange
{-# INLINABLE suitLE #-}
suitLE = bRangeLE . SuitInt

-- | Constructs and validates a 'SuitRange' for ≥@x@
suitGE :: Int -> SuitRange
{-# INLINABLE suitGE #-}
suitGE = bRangeGE . SuitInt

-- | (♠,♥,♦,♣) constraints for a range of hand shapes
type ShapeNum = (SuitNum, SuitNum, SuitNum, SuitNum)

{-| A validated range of hand shapes, as a boxed 'Maybe' 'ShapeNum';
normalization rules are described in 'toShapeR'.

'ShapeNum' ranges are valid if each of the constituent 'SuitNum' ranges is
valid (and nonempty), and the entire set of constraints is satisfiable. Invalid
and\/or empty ranges are 'Nothing'.

Isomorphic to 'ShapeMinimal'. For general computations, use this form; the
'ShapeMinimal' form is used for pretty-printing.
-}
newtype ShapeRange = ShapeRange {
    -- | Unboxes a 'ShapeRange'
    unShapeR :: Maybe ShapeNum } deriving (Eq, Ord)

{-| A validated range of hand shapes, as a boxed 'Maybe' 'ShapeNum';
normalization rules are described in 'minShapeR'.

'ShapeNum' ranges are valid if each of the constituent 'SuitNum' ranges is
valid (and nonempty), and the entire set of constraints is satisfiable. Invalid
and\/or empty ranges are 'Nothing'.

Isomorphic to 'ShapeRange'. For general computations, use the 'ShapeRange' form;
this form is used for pretty-printing.
-}
newtype ShapeMinimal = ShapeMinimal {
    -- | Unboxes a 'ShapeMinimal'
    unShapeM :: Maybe ShapeNum } deriving (Eq, Ord)

-- | Converts to a 'ShapeMinimal' and prints it
instance Show ShapeRange where
    {-# INLINABLE show #-}
    show = show . minShapeR

-- | Prints all nontrivially constrained suits in rank order (reverse
-- alphabetical) as 'SuitRange's, followed by their (filled) unicode suit
-- symbols, separated by spaces. An invalid 'ShapeMinimal' becomes @\"Null\"@.
instance Show ShapeMinimal where
    {-# INLINABLE show #-}
    show = maybe "Null" showShapeN . unShapeM

showShapeN :: ShapeNum -> String
{-# INLINABLE showShapeN #-}
showShapeN = intercalate " " . filter (\x -> head x /= '?')
           . zipWith (flip (++)) ["♠", "♥", "♦", "♣"]
           . map showSuitN . (\(s, h, d, c) -> [s, h, d, c])

-- | The commutative operation intersects the two ranges. Identity is the
-- universal range.
instance Monoid ShapeRange where
    {-# INLINABLE mempty #-}
    mempty = ShapeRange $ Just $ (e, e, e, e) where e = (SuitInt 0, SuitInt 13)
    {-# INLINABLE mappend #-}
    mappend = (liftN2 unShapeR ShapeRange . _'' join . liftM2) intShapeN

intShapeN :: ShapeNum -> ShapeNum -> Maybe ShapeNum
{-# INLINABLE intShapeN #-}
intShapeN x = (>>= normShapeN) . sequenceT . untuple4
            . (fmap intSuitN (Tuple4 x) <*>) . Tuple4

{-| Constructs, validates, and normalizes a 'ShapeRange'. Normalization takes
the most restrictive bound via the following rules:

* For any suit @X@, let @M3@ be the sum of the max-bounds of all other suits.
Then @X@ must be at least @min' = 13 - M3@ cards or it is impossible to have as
many as 13 total cards. The stricter (greater) of @min'@ and the original
min-bound of @X@ is used.

* For any suit @X@, let @m3@ be the sum of the min-bounds of all other suits.
Then @X@ must be at most @max' = 13 - m3@ cards or it is impossible to satisfy
the other suit minima within 13 total cards. The stricter (lower) of @max'@ and
the original max-bound of @X@ is used.

No other derived constraints exist; this is an idempotent operation, ergo
normalizing. The derived constraints may cause an empty (unsatisfiable)
'ShapeRange' even if all component 'SuitRange's are individually valid.
-}
toShapeR :: ShapeNum -> ShapeRange
{-# INLINABLE toShapeR #-}
toShapeR = ShapeRange . normShapeN

normShapeN :: ShapeNum -> Maybe ShapeNum
{-# INLINABLE normShapeN #-}
normShapeN = sequenceT . untuple4 . fmap valSuitN . Tuple4 . butterfly calc
                where calc ((SuitInt l0, SuitInt h0), (SuitInt l1, SuitInt h1),
                           (SuitInt l2, SuitInt h2)) (SuitInt l, SuitInt h) =
                                (SuitInt $ max l $ 13 - h0 - h1 - h2,
                                 SuitInt $ min h $ 13 - l0 - l1 - l2)

-- | Constructs, validates, and normalizes a 'ShapeMinimal' by constructing a
-- 'ShapeRange' and minimizing it
toShapeM :: ShapeNum -> ShapeMinimal
{-# INLINABLE toShapeM #-}
toShapeM = minShapeR . toShapeR

{-| Converts a 'ShapeRange' into its isomorphic 'ShapeMinimal' form. Given that
the range starts out in 'ShapeRange' form, with maximally restrictive
constraints, normalization relaxes (removes) the constraints and takes the
\"best\" (author-preferred) human-readable form via the following rules:

* All equality constraints are never relaxed.

* If 12 cards are known from min-bounds, all remaining non-equality max-bounds
are relaxed. This corresponds to turning 5♠ 4-5♥ 4-5♦ 1-♣ into 5♠ 4+♥ 4+♦, an
\"add last card\" type of shape. Equivalent to ignoring the immediate next rule.

* Else, if all ranges are within 1 card's length, no constraints are relaxed at
all. This prevents \"remove a card\" type shapes such as 5♠ 4-5♥ 3-4♦ 1-♣ from
being turned into 5♠ 5-♥ 3-♦ 1-♣, deemed harder to read.

* Consider the derived max-bounds listed in 'toShapeR', which depend on the
/min/-bounds. If using the min-bounds alone, a max-bound can be derived, then it
is unnecessary and can be relaxed.

* Consider the derived min-bounds listed in 'toShapeR', which depend on the
(new) max-bounds. If using the max-bounds alone, a min-bound can be derived,
then it is unnecessary and can be relaxed. However, any min-bound which is 4 or
more is not relaxed, as turning 6♠ 4+♥ 2-♦ 1-♣ into 6♠ 2-♦ 1-♣ is deemed to not
improve readability.

Note that while either all max- or all min-bounds can be relaxed simultanously
due to independence, doing both simultaneously is incorrect (try a fully
specific single shape). The choice of relaxing max-bounds first corresponds to
preferring 11+♠ -♦ -♣ over 2-♥ -♦ -♣. These rules are idempotent, ergo
normalizing.
-}
minShapeR :: ShapeRange -> ShapeMinimal
{-# INLINABLE minShapeR #-}
minShapeR = ShapeMinimal . fmap result . unShapeR where
    result r = if (known12 r) || (not $ within1 r) then relax r else r
    known12 ((SuitInt s, _), (SuitInt h, _), (SuitInt d, _), (SuitInt c, _)) =
                                                            s + h + d + c >= 12
    within1 (s, h, d, c) = ch s && ch h && ch d && ch c where
                            ch (SuitInt l, SuitInt h) = h - l <= 1
    relax = butterfly relMin . butterfly relMax
    relMin ((_, SuitInt h1), (_, SuitInt h2), (_, SuitInt h3))
        o@(SuitInt l, h'@(SuitInt h))
        | l == h || l >= 4 = o
        | 13 - h1 - h2 - h3 >= l = (minBound, h')
        | otherwise = o
    relMax ((SuitInt l1, _), (SuitInt l2, _), (SuitInt l3, _))
        o@(l'@(SuitInt l), SuitInt h)
        | l == h = o
        | 13 - l1 - l2 - l3 <= h = (l', maxBound)
        | otherwise = o

-- | Converts a 'ShapeMinimal' to its isomorphic 'ShapeRange' form
expShapeM :: ShapeMinimal -> ShapeRange
{-# INLINABLE expShapeM #-}
expShapeM = ShapeRange . (>>= normShapeN) . unShapeM

-- | Constructs, validates, and normalizes a 'ShapeRange' from 4 'SuitRange's
shapeHand :: SuitRange -> SuitRange -> SuitRange -> SuitRange -> ShapeRange
{-# INLINABLE shapeHand #-}
shapeHand = curryN $ ShapeRange . (>>= normShapeN) . sequenceT
                   . untuple4 . fmap unSuitR . Tuple4

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♠
shapeS :: SuitRange -> ShapeRange
{-# INLINABLE shapeS #-}
shapeS = flip (flip (flip shapeHand e) e) e where e = mempty

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♥
shapeH :: SuitRange -> ShapeRange
{-# INLINABLE shapeH #-}
shapeH = flip (flip (shapeHand e) e) e where e = mempty

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♦
shapeD :: SuitRange -> ShapeRange
{-# INLINABLE shapeD #-}
shapeD = flip (shapeHand e e) e where e = mempty

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♣
shapeC :: SuitRange -> ShapeRange
{-# INLINABLE shapeC #-}
shapeC = shapeHand e e e where e = mempty

