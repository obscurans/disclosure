{-|
Module      : Disclosure.Constraint.Strength
Description : Datatypes for ranges of hand strengths
Copyright   : (c) 2016 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines default types for hand strength ranges, that is, anything other than
specifically relating to shape, or presence/absence of specific cards.
-}
module Disclosure.Constraint.Strength (
-- * Datatypes and constructors
  Modifier(..)
, Strength
, toStrength
, StrengthRange
, cStrengthR
-- ** Convenience Strength constructors
, toStrength'
, intStr
, intStr'
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
, toStrengthR
, toStrengthR'
, toIntStrR
, toIntStrR'
-- ** Convenience compound StrengthRange constructors
, cBStrengthR
, toCStrengthR
, toCBStrengthR
, toCStrengthR'
, toCBStrengthR'
, toCIntStrR
, toCBIntStrR
, toCIntStrR'
, toCBIntStrR'
-- * Collapsing functions
, colStrengthR
, colStrengthR'
, colIntStrR
, colIntStrR'
-- * Set algebra reexports from Base.CompoundRange
, IntersectCUR(..)
, UnionCUR(..)
) where

import Data.Char
import Data.Tuple.Curry --tuples
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

-- | A mixed fractional strength value, with optional modifier. Fractions are
-- /not/ reduced. Numerator is kept less in magnitude than the denominator, with
-- the same sign as the integer part.
data Strength = Strength Int Int Int Modifier

-- | Prints (integer part)(numerator in superscript)@&frasl;@(denominator in
-- subscript)(modifier), with integer xor fraction omitted if zero, @\"0\"@ if
-- numerically zero.
instance Show Strength where
    {-# INLINABLE show #-}
    show (Strength d i n m)
        | i == 0 && n /= 0 = show' n ++ "⁄" ++ show_ d ++ show m
        | n == 0 = show i ++ show m
        | otherwise = show i ++ show' n ++ "⁄" ++ show_ d ++ show m where
            show' = map (("⁰¹²³⁴⁵⁶⁷⁸⁹" !!) . (subtract $ ord '0') . ord) . show
                  . abs
            show_ = map (("₀₁₂₃₄₅₆₇₈₉" !!) . (subtract $ ord '0') . ord) . show

-- | Mixed fractions compared numerically, lexicographically 'Modifier'
instance Eq Strength where
    (Strength d i n m) == (Strength d' i' n' m') =
        (i * d + n) * d' == (i' * d' + n') * d && m == m'

-- | Mixed fractions compared numerically, lexicographically 'Modifier'
instance Ord Strength where
    compare (Strength d i n m) (Strength d' i' n' m')
        = compare ((i * d + n) * d', m) ((i' * d' + n') * d, m')

-- | Constructs and normalizes a 'Strength'
toStrength :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> Modifier -> Strength
{-# INLINABLE toStrength #-}
toStrength d i n
    | n /= 0 && d == 0 = error "zero denominator toFracStr"
    | n == 0 && d == 0 = Strength 1 i 0
    | otherwise = Strength (abs d) i' n'
    where (i', n') = quotRem ((i * d + n) * signum d) (abs d)

-- | An unbounded compound range of 'Strength'. Instances: 'Bounded', 'Eq',
-- 'Show', 'Monoid'.
type StrengthRange = CURange Strength

-- | Constructs a 'Strength' with no modifier
toStrength' :: Int -- ^ Fraction denominatior
            -> Int -- ^ Integer part
            -> Int -- ^ Fraction numerator
            -> Strength
{-# INLINABLE toStrength' #-}
toStrength' d i = flip (Strength d i) (:=)

-- | Constructs an integral 'Strength'
intStr :: Int -> Modifier -> Strength
{-# INLINABLE intStr #-}
intStr = flip (Strength 1) 0

-- | Constructs an integral 'Strength' with no modifier
intStr' :: Int -> Strength
{-# INLINABLE intStr' #-}
intStr' = flip intStr (:=)

-- | Constructs a 'StrengthRange' for that exact strength value
strengthEQ :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> Modifier -> StrengthRange
{-# INLINABLE strengthEQ #-}
strengthEQ = _''' (singURange . uRangeEQ) . Strength

-- | Constructs a 'StrengthRange' for that exact strength value with no modifier
strengthEQ' :: Int -- ^ Fraction denominator
            -> Int -- ^ Integer part
            -> Int -- ^ Fraction numerator
            -> StrengthRange
{-# INLINABLE strengthEQ' #-}
strengthEQ' = _'' (singURange . uRangeEQ) . toStrength'

-- | Constructs a 'StrengthRange' for that exact integral strength value
intStrEQ :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrEQ #-}
intStrEQ = _' (singURange . uRangeEQ) . intStr

-- | Constructs a 'StrengthRange' for that exact integral strength value with no
-- modifier
intStrEQ' :: Int -> StrengthRange
{-# INLINABLE intStrEQ' #-}
intStrEQ' = singURange . uRangeEQ . intStr'

-- | Constructs a 'StrengthRange' for ≤ that strength value
strengthLE :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> Modifier -> StrengthRange
{-# INLINABLE strengthLE #-}
strengthLE = _''' (singURange . uRangeLE) . Strength

-- | Constructs a 'StrengthRange' for ≤ that strength value with no modifier
strengthLE' :: Int -- ^ Fraction denominator
            -> Int -- ^ Integer part
            -> Int -- ^ Fraction numerator
            -> StrengthRange
{-# INLINABLE strengthLE' #-}
strengthLE' = _'' (singURange . uRangeLE) . toStrength'

-- | Constructs a 'StrengthRange' for ≤ that integral strength value
intStrLE :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrLE #-}
intStrLE = _' (singURange . uRangeLE) . intStr

-- | Constructs a 'StrengthRange' for ≤ that integral strength value with no
-- modifier
intStrLE' :: Int -> StrengthRange
{-# INLINABLE intStrLE' #-}
intStrLE' = singURange . uRangeLE . intStr'

-- | Constructs a 'StrengthRange' for ≥ that strength value
strengthGE :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> Modifier -> StrengthRange
{-# INLINABLE strengthGE #-}
strengthGE = _''' (singURange . uRangeGE) . Strength

-- | Constructs a 'StrengthRange' for ≥ that strength value with no modifier
strengthGE' :: Int -- ^ Fraction denominator
            -> Int -- ^ Integer part
            -> Int -- ^ Fraction numerator
            -> StrengthRange
{-# INLINABLE strengthGE' #-}
strengthGE' = _'' (singURange . uRangeGE) . toStrength'

-- | Constructs a 'StrengthRange' for ≥ that integral strength value
intStrGE :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrGE #-}
intStrGE = _' (singURange . uRangeGE) . intStr

-- | Constructs a 'StrengthRange' for ≥ that integral strength value with no
-- modifier
intStrGE' :: Int -> StrengthRange
{-# INLINABLE intStrGE' #-}
intStrGE' = singURange . uRangeGE . intStr'

-- | Constructs and validates a 'StrengthRange' with strength values
toStrengthR :: Int -- ^ Common fraction denominator
            -> Int -- ^ Integer part, lower bound
            -> Int -- ^ Fraction numerator, lower bound
            -> Modifier
            -> Int -- ^ Integer part, upper bound
            -> Int -- ^ Fraction numerator, upper bound
            -> Modifier -> StrengthRange
{-# INLINABLE toStrengthR #-}
toStrengthR d i n m = _'' (singURange . toURange' (Strength d i n m))
                    . Strength d

-- | Constructs and validates a 'StrengthRange' with strength values without
-- modifier
toStrengthR' :: Int -- ^ Common fraction denominator
             -> Int -- ^ Integer part, lower bound
             -> Int -- ^ Fraction numerator, lower bound
             -> Int -- ^ Integer part, upper bound
             -> Int -- ^ Fraction numerator, upper bound
             -> StrengthRange
{-# INLINABLE toStrengthR' #-}
toStrengthR' d i n = _' (singURange . toURange' (toStrength' d i n))
                   . toStrength' d

-- | Constructs and validates a 'StrengthRange' with integral strength values
toIntStrR :: Int -> Modifier -> Int -> Modifier -> StrengthRange
{-# INLINABLE toIntStrR #-}
toIntStrR i m = _' (singURange . toURange' (intStr i m)) . intStr

-- | Constructs and validates a 'StrengthRange' with integral strength values
-- without modifiers
toIntStrR' :: Int -> Int -> StrengthRange
{-# INLINABLE toIntStrR' #-}
toIntStrR' = liftN2 intStr' singURange toURange'

-- | Constructs and normalizes a 'StrengthRange' with a list of strength
-- intervals
cStrengthR :: [(Maybe Strength, Maybe Strength)]
           -- ^ (lower bound, upper bound) pairs
           -> StrengthRange
{-# INLINABLE cStrengthR #-}
cStrengthR = toCURange

-- | Constructs and normalizes a 'StrengthRange' with a list of finite strength
-- intervals
cBStrengthR :: [(Strength, Strength)] -- ^ (lower bound, upper bound) pairs
            -> StrengthRange
{-# INLINABLE cBStrengthR #-}
cBStrengthR = toCURange . map (\(x, y) -> (Just x, Just y))

-- | Constructs, normalizes, and collapses a 'StrengthRange' with a list of
-- strength values
toCStrengthR :: Int -- ^ Common fraction denominator
             -> [(Maybe (Int, Int, Modifier), Maybe (Int, Int, Modifier))]
             -- ^ (lower bound, upper bound) pairs of (integer part, fraction
             -- numerator, modifier)
             -> StrengthRange
{-# INLINABLE toCStrengthR #-}
toCStrengthR d = colStrengthR . toCURange
               . map (\(x, y) -> (fmap (toS d) x, fmap (toS d) y))
    where toS d = uncurryN $ toStrength d

-- | Constructs, normalizes, and collapses a 'StrengthRange' with a list of
-- finite strength values
toCBStrengthR :: Int -- ^ Common fraction denominator
              -> [((Int, Int, Modifier), (Int, Int, Modifier))]
              -- ^ (lower bound, upper bound) pair of (integer part, fraction
              -- numerator, modifier)
              -> StrengthRange
{-# INLINABLE toCBStrengthR #-}
toCBStrengthR d = colStrengthR . toCURange . map (\(x, y) -> (toS d x, toS d y))
    where toS d = _' Just $ uncurryN $ toStrength d

-- | Constructs, normalizes, and collapses a 'StrengthRange' with a list of
-- strength values without modifier
toCStrengthR' :: Int -- ^ Common fraction denominator
              -> [(Maybe (Int, Int), Maybe (Int, Int))]
              -- ^ (lower bound, upper bound) pairs of (integer part, fraction
              -- numerator)
              -> StrengthRange
{-# INLINABLE toCStrengthR' #-}
toCStrengthR' d = colStrengthR' . toCURange
                . map (\(x, y) -> (fmap (toS d) x, fmap (toS d) y))
    where toS d = uncurry $ toStrength' d

-- | Constructs, normalizes, and collapses a 'StrengthRange' with a list of
-- finite strength values without modifier
toCBStrengthR' :: Int -- ^ Common fraction denominator
               -> [((Int, Int), (Int, Int))]
               -- ^ (lower bound, upper bound) pairs of (integer part, fraction
               -- numerator)
               -> StrengthRange
{-# INLINABLE toCBStrengthR' #-}
toCBStrengthR' d = colStrengthR' . toCURange
                 . map (\(x, y) -> (toS d x, toS d y))
     where toS d = _' Just $ uncurry $ toStrength' d

-- | Constructs, normalizes, and collapses a 'StrengthRange' with a list of
-- integer strength values
toCIntStrR :: [(Maybe (Int, Modifier), Maybe (Int, Modifier))]
           -- ^ (lower bound, upper bound) pairs
           -> StrengthRange
{-# INLINABLE toCIntStrR #-}
toCIntStrR = colIntStrR . toCURange . map (\(x, y) -> (fmap toS x, fmap toS y))
    where toS = uncurry intStr

-- | Constructs, normalizes, and collapses a 'StrengthRange' with a list of
-- finite integer strength values
toCBIntStrR :: [((Int, Modifier), (Int, Modifier))]
            -- ^ (lower bound, upper bound) pairs
            -> StrengthRange
{-# INLINABLE toCBIntStrR #-}
toCBIntStrR = colIntStrR . toCURange . map (\(x, y) -> (toS x, toS y))
    where toS = _' Just $ uncurry intStr

-- | Constructs, normalizes, and collapses a 'StrengthRange' with a list of
-- integer strength values without modifier
toCIntStrR' :: [(Maybe Int, Maybe Int)] -- ^ (lower bound, upper bound) pairs
            -> StrengthRange
{-# INLINABLE toCIntStrR' #-}
toCIntStrR' = colIntStrR' . toCURange
            . map (\(x, y) -> (fmap intStr' x, fmap intStr' y))

-- | Constructs, normalizes, and collapses a 'StrengthRange' with a list of
-- finite integer strength values without modifier
toCBIntStrR' :: [(Int, Int)] -- ^ (lower bound, upper bound) pairs
             -> StrengthRange
{-# INLINABLE toCBIntStrR' #-}
toCBIntStrR' = colIntStrR' . toCURange . map (\(x, y) -> (toS x, toS y))
    where toS = Just . intStr'

-- | Collapses a 'StrengthRange' treating strength values within 1 fractional
-- unit and same modifier as adjacent
colStrengthR :: StrengthRange -> StrengthRange
{-# INLINABLE colStrengthR #-}
colStrengthR = colCURange comp
    where comp (Strength d i n m) (Strength d' i' n' m')
            = diff $ (i * d + n) * d' - (i' * d' + n') * d
            where l = d * d'
                  diff z = not $ (z, m) < (negate l, m') || (z, m) > (l, m')

-- | Collapses a 'StrengthRange' treating strength values within 1 fractional
-- unit as adjacent regardless of modifiers
colStrengthR' :: StrengthRange -> StrengthRange
{-# INLINABLE colStrengthR' #-}
colStrengthR' = colCURange (\(Strength d i n m) (Strength d' i' n' m') ->
                            abs ((i * d + n) * d' - (i' * d' + n') * d) <=
                                min d d')

-- | Collapses a 'StrengthRange' treating strength values within 1 integer and
-- same modifier as adjacent
colIntStrR :: StrengthRange -> StrengthRange
{-# INLINABLE colIntStrR #-}
colIntStrR = colCURange comp
    where comp (Strength d i n m) (Strength d' i' n' m')
            = diff $ (i * d + n) * d' - (i' * d' + n') * d
            where l = d * d'
                  diff z = not $ (z, m) < (negate l, m') || (z, m) > (l, m')

-- | Collapses a 'StrengthRange' treating strength values within 1 integer as
-- adjacent regardless of modifiers
colIntStrR' :: StrengthRange -> StrengthRange
{-# INLINABLE colIntStrR' #-}
colIntStrR' = colCURange (\(Strength d i n m) (Strength d' i' n' m') ->
                          abs ((i * d + n) * d' - (i' * d' + n') * d) <= d * d')

