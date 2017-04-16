{-|
Module      : Disclosure.Constraint.Strength
Description : Datatypes for ranges of hand strengths
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines default types for hand strength ranges, that is, anything other than
specifically relating to shape, or presence/absence of specific cards.
-}
module Disclosure.Constraint.Strength (
-- * Datatypes and constructors
  Modifier(..)
, StrengthNum(..)
, Strength
, toStrength
, sDenom
, sNumer
, StrengthRange
, toStrengthR
, sRDenom
, sRNumer
-- ** Convenience compound StrengthRange constructors
, toBStrengthR
, toStrengthR'
, toBStrengthR'
, toIntStrR
, toBIntStrR
, toIntStrR'
, toBIntStrR'
-- ** Convenience simple StrengthRange constructors
, strengthEQ
, strengthEQ'
, intStrEQ
, intStrEQ'
, strengthLE
, strengthLE'
, intStrLE
, intStrLE'
, strengthGE
, strengthGE'
, intStrGE
, intStrGE'
, simpStrengthR
, simpStrengthR'
, simpIntStrR
, simpIntStrR'
-- * StrengthRange functions
, expStrengthR
, compStrengthR
, scalStrengthR
, fitStrengthR
, fitStrengthR'
, lcmStrengthR
, lcmStrengthR'
, gcdStrengthR
, colStrengthR
, colStrengthR'
, colStrengthR''
-- * Set algebra operations
, IntersectStrengthR(..)
, UnionStrengthR(..)
-- * Internal unboxed functions
, expStrengthRR
, compStrengthRR
, scalStrengthRR
) where

import Data.Char
import Data.Monoid
import Data.Maybe
import Data.Tuple.Curry --tuple
import Data.Tuple.Homogenous --tuples-homogenous-h98
import Disclosure.Base.Util
import Disclosure.Base.Range
import Disclosure.Base.CompoundRange

-- | An enum of strength modifiers: --, -, =, +, ++
data Modifier = (:--) | (:-) | (:=) | (:+) | (:++)
    deriving (Eq, Ord, Bounded, Enum)

-- | Prints in superscript, ':=' becomes @\"\"@ (no modifier)
instance Show Modifier where
    {-# INLINABLE show #-}
    show (:--) = "⁻⁻"
    show (:-) = "⁻"
    show (:=) = "" -- ⁼
    show (:+) = "⁺"
    show (:++) = "⁺⁺"

-- | A raw fractional strength value, consisting of a fractional numerator with
-- unknown denominator and a modifier
type StrengthNum = (Int, Modifier)

-- | A fractional strength value, as fractional denominator and a 'StrengthNum'.
-- Generally used only for printing.
data Strength = Strength {
    -- | Retrieves the denominator from a 'Strength'
    sDenom :: Int,
    -- | Retrieves the numerator\/'Modifier' from a 'Strength'
    sNumer :: StrengthNum }

-- | Prints (integer part)(numerator in superscript)@&frasl;@(denominator in
-- subscript)(modifier), with integer xor fraction omitted if zero, @\"0\"@ if
-- numerically zero. Numerator is kept less in magnitude than the denominator,
-- with the same sign as the integer part.
instance Show Strength where
    {-# INLINABLE show #-}
    show (Strength d (n, m))
        | i == 0 && f < 0 = "-" ++ show' f ++ "⁄" ++ show_ d ++ show m
        | i == 0 && f > 0 = show' f ++ "⁄" ++ show_ d ++ show m
        | f == 0 = show i ++ show m
        | otherwise = show i ++ show' f ++ "⁄" ++ show_ d ++ show m where
            (i, f) = quotRem n d
            show' = map (("⁰¹²³⁴⁵⁶⁷⁸⁹" !!) . (subtract $ ord '0') . ord) . show
                  . abs
            show_ = map (("₀₁₂₃₄₅₆₇₈₉" !!) . (subtract $ ord '0') . ord) . show

-- | Fractions compared numerically, lexicographically 'Modifier'
instance Eq Strength where
    {-# INLINABLE (==) #-}
    (Strength d (n, m)) == (Strength d' (n', m')) =
        n * d' == n' * d && m == m'

-- | Fractions compared numerically, lexicographically 'Modifier'
instance Ord Strength where
    {-# INLINABLE compare #-}
    compare (Strength d (n, m)) (Strength d' (n', m'))
        = compare (n * d', m) (n' * d, m')

-- | Constructs and normalizes a 'Strength'
toStrength :: Int -- ^ Fraction denominator
           -> StrengthNum -- ^ Fraction numerator and 'Modifier'
           -> Strength
{-# INLINABLE toStrength #-}
toStrength d s@(n, m)
    | d == 0 = error "zero denominator toStrength"
    | d < 0 = Strength (abs d) (negate n, m)
    | otherwise = Strength d s

-- | An unbounded compound range ('CURange') of 'StrengthNum' with a common
-- denominator
data StrengthRange = StrengthRange {
    -- | Retrieves the common denominator of the 'StrengthRange'
    sRDenom :: Int,
    -- | Retrieves the numerator\/'Modifier' range of the 'StrengthRange'
    sRNumer :: CURange StrengthNum }

instance Show StrengthRange where
    {-# INLINABLE show #-}
    show StrengthRange { sRDenom = d, sRNumer = n } =
        show $ unsafeTransCURange (toStrength d) n

-- | Constructs, validates, and normalizes a 'StrengthRange'
toStrengthR :: Int -- ^ Common fraction denominator
            -> [(Maybe StrengthNum, Maybe StrengthNum)]
            -- ^ (lower bound, upper bound) pairs of numerators with 'Modifier'
            -> StrengthRange
{-# INLINABLE toStrengthR #-}
toStrengthR d
    | d <= 0 = error "Nonpositive denominator toStrengthR"
    | otherwise = StrengthRange d . toCURange

-- | Constructs, validates, and normalizes a finite 'StrengthRange'
toBStrengthR :: Int -- ^ Common fraction denominator
             -> [(StrengthNum, StrengthNum)]
             -- ^ (lower bound, upper bound) pairs of numerators with 'Modifier'
             -> StrengthRange
{-# INLINABLE toBStrengthR #-}
toBStrengthR d
    | d <= 0 = error "Nonpositive denominator toBStrengthR"
    | otherwise = StrengthRange d . toCURange'

-- | Constructs, validates, and normalizes a 'StrengthRange' without 'Modifier's
toStrengthR' :: Int -- ^ Common fraction denominator
             -> [(Maybe Int, Maybe Int)]
             -- ^ (lower bound, upper bound) pairs of numerators
             -> StrengthRange
{-# INLINABLE toStrengthR' #-}
toStrengthR' d
    | d <= 0 = error "Nonpositive denominator toStrengthR'"
    | otherwise = StrengthRange d . toCURange . map (\(x, y) -> (s x, s y))
    where s = fmap (flip (,) (:=))

-- | Constructs, validates, and normalizes a finite 'StrengthRange' without
-- 'Modifier's
toBStrengthR' :: Int -- ^ Common fraction denominator
             -> [(Int, Int)] -- ^ (lower bound, upper bound) pairs of numerators
             -> StrengthRange
{-# INLINABLE toBStrengthR' #-}
toBStrengthR' d
    | d <= 0 = error "Nonpositive denominator toStrengthR'"
    | otherwise = StrengthRange d . toCURange . map (\(x, y) -> (s x, s y))
    where s = Just . flip (,) (:=)

-- | Constructs, validates, and normalizes an integral 'StrengthRange'
toIntStrR :: [(Maybe StrengthNum, Maybe StrengthNum)]
          -- ^ (lower bound, upper bound) pairs of strengths with 'Modifier'
          -> StrengthRange
{-# INLINABLE toIntStrR #-}
toIntStrR = toStrengthR 1

-- | Constructs, validates, and normalizes a finite integral 'StrengthRange'
toBIntStrR :: [(StrengthNum, StrengthNum)]
          -- ^ (lower bound, upper bound) pairs of strengths with 'Modifier'
          -> StrengthRange
{-# INLINABLE toBIntStrR #-}
toBIntStrR = toBStrengthR 1

-- | Constructs, validates, and normalizes an integral 'StrengthRange' without
-- 'Modifier's
toIntStrR' :: [(Maybe Int, Maybe Int)]
          -- ^ (lower bound, upper bound) pairs of strengths
          -> StrengthRange
{-# INLINABLE toIntStrR' #-}
toIntStrR' = toStrengthR' 1

-- | Constructs, validates, and normalizes a finite integral 'StrengthRange'
-- without 'Modifier's
toBIntStrR' :: [(Int, Int)] -- ^ (lower bound, upper bound) pairs of strengths
          -> StrengthRange
{-# INLINABLE toBIntStrR' #-}
toBIntStrR' = toBStrengthR' 1

instance Bounded StrengthRange where
    {-# INLINABLE minBound #-}
    minBound = toBIntStrR' []
    {-# INLINABLE maxBound #-}
    maxBound = toIntStrR' [(Nothing, Nothing)]

{-| Scales up the denominator of a 'StrengthRange' by a positive factor. All
intervals are unchanged numerically.

prop> compStrengthR i . expStrengthR i == id
-}
expStrengthR :: Int -> StrengthRange -> StrengthRange
{-# INLINABLE expStrengthR #-}
expStrengthR s o@(StrengthRange d r)
    | s <= 0 = error "Nonpositive scaling factor expStrengthR"
    | s == 1 = o
    | otherwise = StrengthRange (d * s) $ expStrengthRR s r

{-| Scales up the denominator of an unboxed 'StrengthRange' by a positive
factor. All intervals are unchanged numerically.

prop> compStrengthRR i . expStrengthRR i == id
-}
expStrengthRR :: Int -> CURange StrengthNum -> CURange StrengthNum
{-# INLINABLE expStrengthRR #-}
expStrengthRR s
    | s <= 0 = error "Nonpositive scaling factor expStrengthRR"
    | s == 1 = id
    | otherwise = unsafeTransCURange (\(n, m) -> (n * s, m))

{-| Scales down the denominator of a 'StrengthRange' by a positive factor, which
must divide the original denominator. All divisions of numerators are rounded
down, and the resulting intervals are coalesced. Will cause roundoff losses
unless every numerator is divisible by the factor.

prop> compStrengthR i . expStrengthR i == id
-}
compStrengthR :: Int -> StrengthRange -> StrengthRange
{-# INLINABLE compStrengthR #-}
compStrengthR s o@(StrengthRange d r)
    | s <= 0 = error "Nonpositive scaling factor compStrengthR"
    | s == 1 = o
    | mod d s /= 0 = error $ "Scaling factor does not divide original " ++
                             "denominator compStrengthRange"
    | otherwise = StrengthRange (div d s) $ compStrengthRR s r

{-| Scales down the denominator of an unboxed 'StrengthRange' by a positive
factor, which must divide the original denominator. All divisions of numerators
are rounded down, and the resulting intervals are coalesced. Will cause roundoff
losses unless every numerator is divisible by the factor.

prop> compStrengthR i . expStrengthR i == id
-}
compStrengthRR :: Int -> CURange StrengthNum -> CURange StrengthNum
{-# INLINABLE compStrengthRR #-}
compStrengthRR s
    | s <= 0 = error "Nonpositive scaling factor compStrengthRR"
    | s == 1 = id
    | otherwise = colCURange (>=) . unsafeTransCURange (\(n, m) -> (div n s, m))

{-| Scales the denominator of a 'StrengthRange' to a given number, by first
scaling up to the LCM of the number and the original denominator, then down to
the number. When scaling down, all divisions of numerators are rounded down, and
the resulting intervals are coalesced. Will cause roundoff losses unless every
numerator times number divided by original denominator results in an integer.
-}
scalStrengthR :: Int -> StrengthRange -> StrengthRange
{-# INLINABLE scalStrengthR #-}
scalStrengthR s (StrengthRange d r) = StrengthRange s $ scalStrengthRR s d r

{-| Scales the denominator of an unboxed 'StrengthRange' to a given number, by
first scaling up to the LCM of the number and the original denominator, then
down to the number. When scaling down, all divisions of numerators are rounded
down, and the resulting intervals are coalesced. Will cause roundoff losses
unless every numerator times number divided by original denominator results in
an integer.
-}
scalStrengthRR :: Int -> Int -> CURange StrengthNum -> CURange StrengthNum
{-# INLINABLE scalStrengthRR #-}
scalStrengthRR s d
    | s <= 0 = error "Nonpositive denominator scalStrengthRR"
    | s == d = id
    | c == 1 = expStrengthRR e
    | e == 1 = compStrengthRR c
    | otherwise = colCURange (>=)
                . unsafeTransCURange (\(n, m) -> (div (n * e) c, m))
    where l = lcm s d; e = div l d; c = div l s

-- | Scales the denominators of the given 'StrengthRange's to their LCM. All
-- intervals are unchanged numerically.
fitStrengthR :: StrengthRange -> StrengthRange -> (StrengthRange, StrengthRange)
{-# INLINABLE fitStrengthR #-}
fitStrengthR x@(StrengthRange d _) y@(StrengthRange d' _)
    = (lcmStrengthR d' x, lcmStrengthR d y)

-- | Scales the denominator of a 'StrengthRange' to the LCM of the given number
-- and its own denominator. All intervals are unchanged numerically.
lcmStrengthR :: Int -> StrengthRange -> StrengthRange
{-# INLINABLE lcmStrengthR #-}
lcmStrengthR n x@(StrengthRange d _) = expStrengthR (div (lcm n d) d) x

-- | Scales the denominators of the given 'StrengthRange's to their LCM. All
-- intervals are unchanged numerically. Returns the unboxed 'CURange's without
-- denominator.
fitStrengthR' :: StrengthRange -> StrengthRange ->
                 (CURange StrengthNum, CURange StrengthNum)
{-# INLINABLE fitStrengthR' #-}
fitStrengthR' x@(StrengthRange d _) y@(StrengthRange d' _)
    = (lcmStrengthR' d' x, lcmStrengthR' d y)

-- | Scales the denominator of a 'StrengthRange' to the LCM of the given number
-- and its own denominator. All intervals are unchanged numerically. Returns
-- the unboxed 'CURange' without denominator.
lcmStrengthR' :: Int -> StrengthRange -> CURange StrengthNum
{-# INLINABLE lcmStrengthR' #-}
lcmStrengthR' d' (StrengthRange d r) = expStrengthRR (div (lcm d' d) d) r

instance Eq StrengthRange where
    {-# INLINABLE (==) #-}
    (==) = _' (uncurry (==)) . fitStrengthR'

-- | The commutative operation intersects the two ranges. Identity is the
-- universal set.
instance Monoid StrengthRange where
    {-# INLINABLE mempty #-}
    mempty = StrengthRange 1 mempty
    {-# INLINABLE mappend #-}
    mappend x@(StrengthRange d _) y@(StrengthRange d' _) = StrengthRange
        (lcm d d') $ uncurry mappend $ fitStrengthR' x y

-- | Scales down the denominator of a 'StrengthRange' as much as possible
-- without affecting the numerical value of any interval, i.e. by the GCD of
-- all numerators and the denominator.
gcdStrengthR :: StrengthRange -> StrengthRange
{-# INLINABLE gcdStrengthR #-}
gcdStrengthR x@(StrengthRange d r) = compStrengthR (foldr gcd d ns) x
    where ns = map fst $ catMaybes $ uncurry (++) $ unzip $ unCURange r

-- | Collapses a 'StrengthRange' treating strength values within one modifier
-- unit (including @x@ fraction, max modifier and @x+1@ fraction, min modifier)
-- as adjacent.
colStrengthR :: StrengthRange -> StrengthRange
{-# INLINABLE colStrengthR #-}
colStrengthR (StrengthRange d r) = StrengthRange d $ colCURange comp r
          -- can assume x < y
    where comp (n, m) (n', m') = (n == n' && (fromEnum m') - (fromEnum m) <= 1)
            || ((n == n' - 1) && (m == maxBound) && (m' == minBound))

-- | Collapses a 'StrengthRange' treating strength values within 1 fractional
-- unit and same modifier as adjacent.
colStrengthR' :: StrengthRange -> StrengthRange
{-# INLINABLE colStrengthR' #-}
colStrengthR' (StrengthRange d r) = StrengthRange d $ colCURange comp r
          -- can assume x < y
    where comp (n, m) (n', m') = (n == n') || ((n == n' - 1) && (m >= m'))

-- | Collapses a 'StrengthRange' treating strength values within 1 fractional
-- unit as adjacent regardless of modifiers.
colStrengthR'' :: StrengthRange -> StrengthRange
{-# INLINABLE colStrengthR'' #-}
colStrengthR'' (StrengthRange d r) = StrengthRange d $ colCURange comp r
           -- can assume x < y
     where comp (n, m) (n', m') = (n == n') || (n == n' - 1)

-- | Constructs a 'StrengthRange' exactly equal to the strength value with
-- 'Modifier'
strengthEQ :: Int -- ^ Fraction denominator
           -> Int -- ^ Fraction numerator
           -> Modifier -> StrengthRange
{-# INLINABLE strengthEQ #-}
strengthEQ d = _' (StrengthRange d . singURange . uRangeEQ) . (,)

-- | Constructs a 'StrengthRange' exactly equal to the strength value without
-- 'Modifier'
strengthEQ' :: Int -- ^ Fraction denominator
            -> Int -- ^ Fraction numerator
            -> StrengthRange
{-# INLINABLE strengthEQ' #-}
strengthEQ' d = flip (strengthEQ d) (:=)

-- | Constructs a 'StrengthRange' exactly equal to the integral strength value
-- with 'Modifier'
intStrEQ :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrEQ #-}
intStrEQ = strengthEQ 1

-- | Constructs a 'StrengthRange' exactly equal to the integral strength value
-- without 'Modifier'
intStrEQ' :: Int -> StrengthRange
{-# INLINABLE intStrEQ' #-}
intStrEQ' = flip intStrEQ (:=)

-- | Constructs a 'StrengthRange' for ≤ the strength value with 'Modifier'
strengthLE :: Int -- ^ Fraction denominator
           -> Int -- ^ Fraction numerator
           -> Modifier -> StrengthRange
{-# INLINABLE strengthLE #-}
strengthLE d = _' (StrengthRange d . singURange . uRangeLE) . (,)

-- | Constructs a 'StrengthRange' for ≤ the strength value without 'Modifier'
strengthLE' :: Int -- ^ Fraction denominator
            -> Int -- ^ Fraction numerator
            -> StrengthRange
{-# INLINABLE strengthLE' #-}
strengthLE' d = flip (strengthLE d) (:=)

-- | Constructs a 'StrengthRange' for ≤ the integral strength value with
-- 'Modifier'
intStrLE :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrLE #-}
intStrLE = strengthLE 1

-- | Constructs a 'StrengthRange' for ≤ the integral strength value without
-- 'Modifier'
intStrLE' :: Int -> StrengthRange
{-# INLINABLE intStrLE' #-}
intStrLE' = flip intStrLE (:=)

-- | Constructs a 'StrengthRange' for ≥ the strength value with 'Modifier'
strengthGE :: Int -- ^ Fraction denominator
           -> Int -- ^ Fraction numerator
           -> Modifier -> StrengthRange
{-# INLINABLE strengthGE #-}
strengthGE d = _' (StrengthRange d . singURange . uRangeGE) . (,)

-- | Constructs a 'StrengthRange' for ≥ the strength value without 'Modifier'
strengthGE' :: Int -- ^ Fraction denominator
            -> Int -- ^ Fraction numerator
            -> StrengthRange
{-# INLINABLE strengthGE' #-}
strengthGE' d = flip (strengthGE d) (:=)

-- | Constructs a 'StrengthRange' for ≥ the integral strength value with
-- 'Modifier'
intStrGE :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrGE #-}
intStrGE = strengthGE 1

-- | Constructs a 'StrengthRange' for ≥ the integral strength value without
-- 'Modifier'
intStrGE' :: Int -> StrengthRange
{-# INLINABLE intStrGE' #-}
intStrGE' = flip intStrGE (:=)

-- | Constructs and validates a 'StrengthRange' for between the two strength
-- values and modifiers
simpStrengthR :: Int -- ^ Common fraction denominator
              -> Int -- ^ Lower bound numerator
              -> Modifier -- ^ Lower bound modifier
              -> Int -- ^ Upper bound numerator
              -> Modifier -- ^ Upper bound modifier
              -> StrengthRange
{-# INLINABLE simpStrengthR #-}
simpStrengthR d n m n' m' = StrengthRange d $ singURange $
                            toURange' (n, m) (n', m')

-- | Constructs and validates a 'StrengthRange' for between the two strength
-- values without modifiers
simpStrengthR' :: Int -- ^ Common fraction denominator
               -> Int -- ^ Lower bound numerator
               -> Int -- ^ Upper bound numerator
               -> StrengthRange
{-# INLINABLE simpStrengthR' #-}
simpStrengthR' d n n' = simpStrengthR d n (:=) n' (:=)

-- | Constructs and validates a 'StrengthRange' for between the two integral
-- strength values and modifiers
simpIntStrR :: Int -> Modifier -> Int -> Modifier -> StrengthRange
{-# INLINABLE simpIntStrR #-}
simpIntStrR = simpStrengthR 1

-- | Constructs and validates a 'StrengthRange' for between the two integral
-- strength values without modifiers
simpIntStrR' :: Int -> Int -> StrengthRange
{-# INLINABLE simpIntStrR' #-}
simpIntStrR' = simpStrengthR' 1

-- | Newtype wrapper on a 'StrengthRange' whose 'Ord'ering is subset inclusion,
-- for testing of inclusion. Note that 'Eq' is inherited directly and __IS
-- INCONSISTENT__ with 'Ord' on incomparable ranges.
newtype IntersectStrengthR = IntersectStrengthR {
    unIntersectStrengthR :: StrengthRange } deriving Eq

instance Ord IntersectStrengthR where
    {-# INLINABLE compare #-}
    compare (IntersectStrengthR x) (IntersectStrengthR y)
        | x == y = EQ
        | intersection == x = LT
        | intersection == y = GT
        | otherwise = EQ
        where intersection = mappend x y

-- | Newtype wrapper for a 'StrengthRange' whose commutative 'Monoid' is set
-- union, with identity being the empty set.
newtype UnionStrengthR = UnionStrengthR { unUnionStrengthR :: StrengthRange }
    deriving Eq

instance Monoid UnionStrengthR where
    {-# INLINABLE mempty #-}
    mempty = UnionStrengthR minBound
    {-# INLINABLE mappend #-}
    mappend (UnionStrengthR x@(StrengthRange d _))
        (UnionStrengthR y@(StrengthRange d' _)) = UnionStrengthR $ StrengthRange
            (lcm d d') $ unUnionCUR $ uncurry mappend
                       $ untuple2 $ fmap UnionCUR $ Tuple2 $ fitStrengthR' x y

