{-|
Module      : Disclosure.Shape
Description : Datatypes for ranges of individual suit lengths and hand shapes
Copyright   : (c) 2016 Jeffrey Tsang
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
module Disclosure.Shape
( SuitNum
, SuitRange
, toSuitR
, unSuitR
, suitLE
, suitGE
, suitEQ
, inclSuitR
, ShapeNum
, ShapeRange
, toShapeR
, unShapeR
, ShapeMinimal
, toShapeM
, unShapeM
, minShapeR
, expShapeM
, shapeHand
, shapeS
, shapeH
, shapeD
, shapeC
, inclShapeR
) where

import Data.List
import qualified Data.Tuple.Homogenous --tuples-homogenous-h98
import Data.Maybe
import qualified Data.Foldable
import Data.Monoid

toList4 = Data.Foldable.toList . Data.Tuple.Homogenous.Tuple4
readSucc y x = [(x, y)]

-- | (Low, high) pair of constraints for a suit length. Trivial\/nonexistent
-- constraints are represented by low = 0 and\/or high = 13.
type SuitNum = (Int, Int)

{-| A validated range of lengths in an unspecified suit, as a boxed 'Maybe'
'SuitNum'.

'SuitNum' ranges (low, high) are valid if 0 ≤ low ≤ high ≤ 13. Invalid and\/or
empty ranges are 'Nothing'.
-}
newtype SuitRange = SuitRange {
    -- | Unboxes a 'SuitRange'
    unSuitR :: Maybe SuitNum } deriving (Eq, Ord)

{-| Pretty-prints a 'SuitRange' according to the following rules in precedence:

* An invalid range becomes @\"Null\"@

* [0, 13] or no constraint becomes @\"?\"@

* [0, 0] or a void becomes @\"-\"@

* [x, x] or an = constraint becomes @\"x\"@

* [0, x] or only a ≤ constraint becomes @\"x-\"@

* [x, 13] or only a ≥ constraint becomes @\"x+\"@

* All other [x, y] become @\"x-y\"@
-}
instance Show SuitRange where
    show (SuitRange Nothing) = "Null"
    show (SuitRange (Just r)) = showSuitN r

showSuitN :: SuitNum -> String
showSuitN (a, b)
    | a == 0 && b == 13 = "?"
    | a == 0 && b == 0 = "-"
    | a == b = show a
    | a == 0 = show b ++ "-"
    | b == 13 = show a ++ "+"
    | otherwise = show a ++ "-" ++ show b

instance Read SuitRange where
    readsPrec _ ('N':'u':'l':'l':xs) = readSucc xs $ SuitRange Nothing  -- "Null"...
    readsPrec _ ('?':xs) = readSucc xs $ mempty                         -- "?"...
    readsPrec _ ('-':xs)
        | (x, z):_ <- reads xs = readSucc z $ suitLE x                  -- "-x"...
        | otherwise = readSucc xs $ suitEQ 0                            -- "-"...
    readsPrec _ xt
        | (x, '-':xs):_ <- reads xt
        , (y, z):_ <- reads xs = readSucc z $ toSuitR (x, y)            -- "x-y"...
        | (x, '-':xs):_ <- reads xt = readSucc xs $ suitLE x            -- "x-"...
        | (x, '+':xs):_ <- reads xt = readSucc xs $ suitGE x            -- "x+"...
        | (x, xs):_ <- reads xt = readSucc xs $ suitEQ x                -- "x"...
    readsPrec _ _ = []

-- | The commutative operation intersects the two ranges. Identity is the
-- universal range.
instance Monoid SuitRange where
    mempty = SuitRange $ Just (0, 13)
    mappend (SuitRange mx) (SuitRange my) = SuitRange $ do
        (xl, xh) <- mx
        (yl, yh) <- my
        let zl = max xl yl
            zh = min xh yh in
            if zl > zh then Nothing else Just (zl, zh)

-- | Constructs and validates a 'SuitRange'
toSuitR :: SuitNum -> SuitRange
toSuitR = SuitRange . normSuitN

normSuitN :: SuitNum -> Maybe SuitNum
normSuitN (l, h)
    | l < 0 || l > 13 || h < 0 || h > 13 || l > h = Nothing
    | otherwise = Just (l, h)

-- | Constructs and validates a 'SuitRange' for ≤@x@
suitLE :: Int -> SuitRange
suitLE x = toSuitR (0, x)

-- | Constructs and validates a 'SuitRange' for ≥@x@
suitGE :: Int -> SuitRange
suitGE x = toSuitR (x, 13)

-- | Constructs and validates a 'SuitRange' for =@x@
suitEQ :: Int -> SuitRange
suitEQ x = toSuitR (x, x)

-- | Tests whether @a@ is included in @b@, using the 'Monoid' of intersection.
-- ≤ ordering in the meet-lattice.
inclSuitR :: SuitRange -> SuitRange -> Bool
inclSuitR a = (== a) . mappend a

-- | (♠,♥,♦,♣) constraints for a range of hand
-- shapes
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

showShapeN :: ShapeNum -> String
showShapeN = intercalate " " . filter (\x -> head x /= '?')
           . zipWith (flip (++)) ["♠", "♥", "♦", "♣"]
           . map showSuitN . toList4

-- | Converts to a 'ShapeMinimal' and pretty-prints it
instance Show ShapeRange where
    show = show . minShapeR

-- | Pretty-prints all nontrivially constrained suits in rank order (reverse
-- alphabetical) as 'SuitRange's, followed by their (filled) unicode suit
-- symbols, separated by spaces. An invalid 'ShapeMinimal' becomes @\"Null\"@.
instance Show ShapeMinimal where
    show (ShapeMinimal Nothing) = "Null"
    show (ShapeMinimal (Just x)) = showShapeN x

-- | __TODO:__ Not implemented
instance Read ShapeRange where
    readsPrec _ _ = []

-- | Parses as a 'ShapeRange' and converts it to 'ShapeMinimal'
instance Read ShapeMinimal where
    readsPrec = (map (\(x, y) -> (minShapeR x, y)) .) . readsPrec

-- | The commutative operation intersects the two ranges. Identity is the
-- universal range.
instance Monoid ShapeRange where
    mempty = ShapeRange $ Just (e, e, e, e) where e = (0, 13)
    mappend (ShapeRange x) (ShapeRange y) = ShapeRange $ do
        (sx, hx, dx, cx) <- x
        (sy, hy, dy, cy) <- y
        let app (lx, hx) (ly, hy) = (max lx ly, min hx hy) in
            normShapeN (app sx sy, app hx hy, app dx dy, app cx cy)

-- | The commutative operation intersects the two ranges. Identity is the
-- universal range. Converts to and from 'ShapeRange' form.
instance Monoid ShapeMinimal where
    mempty = ShapeMinimal $ Just (e, e, e, e) where e = (0, 13)
    mappend x = minShapeR . mappend (expShapeM x) . expShapeM

{-| Constructs, validates, and normalizes a 'ShapeRange'.

Normalization takes the most restrictive bound via the following rules:

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
toShapeR = ShapeRange . normShapeN

normShapeN :: ShapeNum -> Maybe ShapeNum
normShapeN (s, h, d, c) = do
    s2 <- normSuitN $ calc s (h, d, c)
    h2 <- normSuitN $ calc h (s, d, c)
    d2 <- normSuitN $ calc d (s, h, c)
    c2 <- normSuitN $ calc c (s, h, d)
    return (s2, h2, d2, c2)
    where calc (l0, h0) ((l1, h1), (l2, h2), (l3, h3)) = (l, h)
            where l = max l0 (13 - h1 - h2 - h3)
                  h = min h0 (13 - l1 - l2 - l3)

-- | Constructs, validates, and normalizes a 'ShapeMinimal' by constructing a
-- 'ShapeRange' and minimizing it
toShapeM :: ShapeNum -> ShapeMinimal
toShapeM = minShapeR . toShapeR

{-| Converts a 'ShapeRange' into its isomorphic 'ShapeMinimal' form.

Given that the range starts out in 'ShapeRange' form, with maximally restrictive
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
minShapeR (ShapeRange Nothing) = ShapeMinimal Nothing
minShapeR (ShapeRange (Just r)) = ShapeMinimal $ Just $ result r
    where result = if (known12 r) || (not $ within1 r) then relax else id
          known12 = (>= 12) . sum . map fst . toList4
          within1 = all (\(l, h) -> h - l <= 1) . toList4
          relax = calc relMin . calc relMax
          calc f (s, h, d, c) = (f s (h, d, c), f h (s, d, c), f d (s, h, c), f c (s, h, d))
          relMin (l0, h0) ((_, h1), (_, h2), (_, h3))
            | l0 == h0 || l0 >= 4 = (l0, h0)
            | 13 - h1 - h2 - h3 >= l0 = (0, h0)
            | otherwise = (l0, h0)
          relMax (l0, h0) ((l1, _), (l2, _), (l3, _))
            | l0 == h0 = (l0, h0)
            | 13 - l1 - l2 - l3 <= h0 = (l0, 13)
            | otherwise = (l0, h0)

-- | Converts a 'ShapeMinimal' to its isomorphic 'ShapeRange' form
expShapeM :: ShapeMinimal -> ShapeRange
expShapeM = ShapeRange . (=<<) normShapeN . unShapeM

-- | Constructs, validates, and normalizes a 'ShapeRange' from 4 'SuitRange's
shapeHand :: SuitRange -> SuitRange -> SuitRange -> SuitRange -> ShapeRange
shapeHand (SuitRange ms) (SuitRange mh) (SuitRange md) (SuitRange mc) = ShapeRange $ do
    s <- ms; h <- mh; d <- md; c <- mc
    normShapeN (s, h, d, c)

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♠
shapeS :: SuitRange -> ShapeRange
shapeS = ShapeRange . (=<<) (\r -> normShapeN (r, e, e, e)) . unSuitR where e = (0, 13)

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♥
shapeH :: SuitRange -> ShapeRange
shapeH = ShapeRange . (=<<) (\r -> normShapeN (e, r, e, e)) . unSuitR where e = (0, 13)

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♦
shapeD :: SuitRange -> ShapeRange
shapeD = ShapeRange . (=<<) (\r -> normShapeN (e, e, r, e)) . unSuitR where e = (0, 13)

-- | Constructs, validates, and normalizes a 'ShapeRange' with only the
-- specified 'SuitRange' of ♣
shapeC :: SuitRange -> ShapeRange
shapeC = ShapeRange . (=<<) (\r -> normShapeN (e, e, e, r)) . unSuitR where e = (0, 13)

-- | Tests whether @a@ is included in @b@, using the 'Monoid' of intersection.
-- ≤ ordering in the meet-lattice.
inclShapeR :: ShapeRange -> ShapeRange -> Bool
inclShapeR a = (== a) . mappend a

