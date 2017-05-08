{-|
Module      : Disclosure.Constraint.Shape.Internal
Description : Internal datatype definitions and functions
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable
-}
module Disclosure.Constraint.Shape.Internal where

import Data.Maybe
import Data.Tuple.Sequence (sequenceT) --tuple
import Data.Tuple.Homogenous (Tuple4(..)) --tuples-homogenous-h98
import Disclosure.Base.Util
import Disclosure.Base.Range
import Disclosure.Base.Range.Internal (BRange(..))

-- | Utility for doing suit computations with (all other suits, current suit)
butterfly :: ((a, a, a) -> a -> b) -> Tuple4 a -> Tuple4 b
{-# INLINABLE butterfly #-}
butterfly f = Tuple4 . butterfly' f . untuple4
    where butterfly' f (s, h, d, c) = (f (h, d, c) s, f (s, d, c) h,
                                       f (s, h, c) d, f (s, h, d) c)

-- | Newtype wrapper over 'Int' which is bounded to [@0@, @13@]
newtype Suit = Suit {
    -- | Unboxes a 'Suit'
    unSuit :: Int } deriving (Eq, Ord)

-- | Checks and 'Maybe' constructs a 'Suit'
toSuit :: Int -> Maybe Suit
{-# INLINABLE toSuit #-}
toSuit s
    | s < 0 || s > 13 = Nothing
    | otherwise = Just $ Suit s

instance Bounded Suit where
    {-# INLINABLE minBound #-}
    minBound = Suit 0
    {-# INLINABLE maxBound #-}
    maxBound = Suit 13

instance Enum Suit where
    {-# INLINABLE fromEnum #-}
    fromEnum (Suit s) = s
    {-# INLINABLE toEnum #-}
    toEnum = fromJust . toSuit

-- | @0@ becomes "-", else directly as 'Int'
instance Show Suit where
    {-# INLINABLE show #-}
    show (Suit x)
        | x == 0 = "-"
        | otherwise = show x

-- | Directly as 'Int' __TODO__:check
instance Read Suit where
    {-# INLINABLE readsPrec #-}
    readsPrec = _'' (map (\(x, y) -> (Suit x, y))) readsPrec

-- | A validated range of lengths in an unspecified suit
type SuitRange = BRange Suit

-- | Constructs and validates a 'SuitRange'
toSuitRange :: Int -> Int -> Maybe SuitRange
{-# INLINABLE toSuitRange #-}
toSuitRange = liftN2 Suit id $ curry toBRange

-- | Constructs and validates a 'SuitRange' for =@x@
suitEQ :: Int -> Maybe SuitRange
{-# INLINABLE suitEQ #-}
suitEQ = bRangeEQ . Suit

-- | Constructs and validates a 'SuitRange' for ≤@x@
suitLE :: Int -> Maybe SuitRange
{-# INLINABLE suitLE #-}
suitLE = bRangeLE . Suit

-- | Constructs and validates a 'SuitRange' for ≥@x@
suitGE :: Int -> Maybe SuitRange
{-# INLINABLE suitGE #-}
suitGE = bRangeGE . Suit

{-| A validated, nonempty range of hand shapes, as a 4-tuple of 'SuitRange's;
normalization rules are described in 'toShape'.

'Shape' ranges are valid if each of the constituent 'SuitRange's is valid (and
nonempty), and the entire set of constraints is satisfiable. Empty ranges are
invalid.
-}
newtype Shape = Shape {
    -- | Unboxes a 'Shape'
    unShape :: Tuple4 SuitRange } deriving (Eq, Ord)

{-| Constructs, validates, and normalizes a 'Shape'. Normalization takes the
most restrictive bound via the following rules:

* For any suit @X@, let @M3@ be the sum of the max-bounds of all other suits.
Then @X@ must be at least @min' = 13 - M3@ cards or it is impossible to have as
many as 13 total cards. The stricter (greater) of @min'@ and the original
min-bound of @X@ is used.

* For any suit @X@, let @m3@ be the sum of the min-bounds of all other suits.
Then @X@ must be at most @max' = 13 - m3@ cards or it is impossible to satisfy
the other suit minima within 13 total cards. The stricter (lower) of @max'@ and
the original max-bound of @X@ is used.

No other derived constraints exist; this is an idempotent operation, ergo
normalizing. The derived constraints may result in an invalid (unsatisfiable)
'Shape' even if all component 'SuitRange's are individually valid.
-}
toShape :: Tuple4 SuitRange -> Maybe Shape
{-# INLINABLE toShape #-}
toShape = fmap (Shape . Tuple4) . sequenceT . untuple4
        . fmap toBRange . butterfly calc . fmap unBRange
        where calc ((Suit l0, Suit h0), (Suit l1, Suit h1), (Suit l2, Suit h2))
                (Suit l, Suit h) = (Suit $ max l $ 13 - h0 - h1 - h2,
                                    Suit $ min h $ 13 - l0 - l1 - l2)

{-| Converts a 'Shape' into its isomorphic minimal form as an unboxed 'Shape'.
Given that the range starts out in 'Shape' form, with maximally restrictive
constraints, normalization relaxes (removes) the constraints and takes the
\"best\" (author-preferred) human-readable form via the following rules:

* All equality constraints are never relaxed.

* Consider the derived max-bounds listed in 'toShape', which depend on the
/min/-bounds. If using the min-bounds alone, a max-bound can be derived, then it
is unnecessary and can be relaxed.

* Next, consider the derived min-bounds listed in 'toShape', which depend on
the (new) max-bounds. If using the max-bounds alone, a min-bound can be derived,
then it is unnecessary and can be relaxed. However, any min-bound which is 4 or
more is not relaxed, as turning 6♠ 4+♥ 2-♦ 1-♣ into 6♠ 2-♦ 1-♣ is deemed to not
improve readability.

Note that while either all max- or all min-bounds can be relaxed simultanously
due to independence, doing both simultaneously is incorrect (try a fully
specific single shape). The choice of relaxing max-bounds first corresponds to
preferring 11+♠ -♦ -♣ over 2-♥ -♦ -♣. These rules are idempotent, ergo
normalizing.
-}
minShape :: Shape -> Tuple4 SuitRange
{-# INLINABLE minShape #-}
minShape = fmap BRange -- trusted operation
         . butterfly relMin . butterfly relMax . fmap unBRange . unShape
    where relMin ((_, Suit h1), (_, Suit h2), (_, Suit h3))
            o@(Suit l, h'@(Suit h))
            | l == h || l >= 4 = o
            | 13 - h1 - h2 - h3 >= l = (minBound, h')
            | otherwise = o
          relMax ((Suit l1, _), (Suit l2, _), (Suit l3, _))
            o@(l'@(Suit l), Suit h)
            | l == h = o
            | 13 - l1 - l2 - l3 <= h = (l', maxBound)
            | otherwise = o

-- | The commutative operation, which may fail if the result is empty,
-- intersects the two ranges. Identity is the universal range.
instance Monoid' Shape where
    {-# INLINABLE mempty' #-}
    mempty' = Shape $ Tuple4 $ (e, e, e, e) where e = mempty'
    {-# INLINABLE mappend' #-}
    mappend' = liftN2 unShape ((>>= toShape . Tuple4) . sequenceT . untuple4)
               $ applyA2 $ pure mappend'
