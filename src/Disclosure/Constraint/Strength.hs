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
-- ** Convenience constructors
, toStrength'
, intStr
, intStr'
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
-- * Set algebra reexports from Base.Range
, IntersectUR(..)
, PUnionUR(..)
) where

import Data.Char
import Disclosure.Base.Util
import Disclosure.Base.Range

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

-- | An unbounded range of 'Strength'. Instances: 'Bounded', 'Eq', 'Ord',
-- 'Show', 'Monoid'.
type StrengthRange = URange Strength

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

-- | Constructs a 'StrengthR' for that exact strength value
strengthEQ :: Int -- ^ Fraction denominator
          -> Int -- ^ Integer part
          -> Int -- ^ Fraction numerator
          -> Modifier -> StrengthRange
{-# INLINABLE strengthEQ #-}
strengthEQ = _''' uRangeEQ . Strength

-- | Constructs a 'StrengthR' for that exact strength value with no modifier
strengthEQ' :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> StrengthRange
{-# INLINABLE strengthEQ' #-}
strengthEQ' = _'' uRangeEQ . toStrength'

-- | Constructs a 'StrengthR' for that exact integral strength value
intStrEQ :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrEQ #-}
intStrEQ = _' uRangeEQ . intStr

-- | Constructs a 'StrengthR' for that exact integral strength value with no
-- modifier
intStrEQ' :: Int -> StrengthRange
{-# INLINABLE intStrEQ' #-}
intStrEQ' = uRangeEQ . intStr'

-- | Constructs a 'StrengthR' for ≤ that strength value
strengthLE :: Int -- ^ Fraction denominator
          -> Int -- ^ Integer part
          -> Int -- ^ Fraction numerator
          -> Modifier -> StrengthRange
{-# INLINABLE strengthLE #-}
strengthLE = _''' uRangeLE . Strength

-- | Constructs a 'StrengthR' for ≤ that strength value with no modifier
strengthLE' :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> StrengthRange
{-# INLINABLE strengthLE' #-}
strengthLE' = _'' uRangeLE . toStrength'

-- | Constructs a 'StrengthR' for ≤ that integral strength value
intStrLE :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrLE #-}
intStrLE = _' uRangeLE . intStr

-- | Constructs a 'StrengthR' for ≤ that integral strength value with no
-- modifier
intStrLE' :: Int -> StrengthRange
{-# INLINABLE intStrLE' #-}
intStrLE' = uRangeLE . intStr'

-- | Constructs a 'StrengthR' for ≥ that strength value
strengthGE :: Int -- ^ Fraction denominator
          -> Int -- ^ Integer part
          -> Int -- ^ Fraction numerator
          -> Modifier -> StrengthRange
{-# INLINABLE strengthGE #-}
strengthGE = _''' uRangeGE . Strength

-- | Constructs a 'StrengthR' for ≥ that strength value with no modifier
strengthGE' :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> StrengthRange
{-# INLINABLE strengthGE' #-}
strengthGE' = _'' uRangeLE . toStrength'

-- | Constructs a 'StrengthR' for ≥ that integral strength value
intStrGE :: Int -> Modifier -> StrengthRange
{-# INLINABLE intStrGE #-}
intStrGE = _' uRangeGE . intStr

-- | Constructs a 'StrengthR' for ≥ that integral strength value with no
-- modifier
intStrGE' :: Int -> StrengthRange
{-# INLINABLE intStrGE' #-}
intStrGE' = uRangeGE . intStr'

-- | Constructs and validates a 'StrengthR' with strength values
toStrengthR :: Int -- ^ Common fraction denominator
           -> Int -- ^ Integer part, lower bound
           -> Int -- ^ Fraction numerator, lower bound
           -> Modifier
           -> Int -- ^ Integer part, upper bound
           -> Int -- ^ Fraction numerator, upper bound
           -> Modifier -> StrengthRange
{-# INLINABLE toStrengthR #-}
toStrengthR d i n m = _'' (toURange' (Strength d i n m)) . Strength d

-- | Constructs and validates a 'StrengthR' with strength values without
-- modifier
toStrengthR' :: Int -- ^ Common fraction denominator
            -> Int -- ^ Integer part, lower bound
            -> Int -- ^ Fraction numerator, lower bound
            -> Int -- ^ Integer part, upper bound
            -> Int -- ^ Fraction numerator, upper bound
            -> StrengthRange
{-# INLINABLE toStrengthR' #-}
toStrengthR' d i n = _' (toURange' (toStrength' d i n)) . toStrength' d

-- | Constructs and validates a 'StrengthR' with integral strength values
toIntStrR :: Int -> Modifier -> Int -> Modifier -> StrengthRange
{-# INLINABLE toIntStrR #-}
toIntStrR i m = _' (toURange' (intStr i m)) . intStr

-- | Constructs and validates a 'StrengthR' with integral strength values
-- without modifiers
toIntStrR' :: Int -> Int -> StrengthRange
{-# INLINABLE toIntStrR' #-}
toIntStrR' = liftN2 intStr' id toURange'

