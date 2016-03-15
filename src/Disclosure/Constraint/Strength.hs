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
, IntStr(..)
, IntStrR
, FracStr
, toFracStr
, FracStrR
-- ** Integral strength convenience constructors
, intStr'
, intStrEQ
, intStrEQ'
, intStrLE
, intStrLE'
, intStrGE
, intStrGE'
, toIntStrR
, toIntStrR'
-- ** Fractional strength convenience constructors
, fracStr'
, fracStr''
, fracStrEQ
, fracStrEQ'
, fracStrEQ''
, fracStrLE
, fracStrLE'
, fracStrLE''
, fracStrGE
, fracStrGE'
, fracStrGE''
, toFracStrR
, toFracStrR'
, toFracStrR''
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

-- | Shows in superscript, @:=@ shows nothing (no modifier)
instance Show Modifier where
    {-# INLINABLE show #-}
    show (:--) = "⁻⁻"
    show (:-) = "⁻"
    show (:=) = "" -- ⁼
    show (:+) = "⁺"
    show (:++) = "⁺⁺"

-- | An integral strength value, with optional modifier
data IntStr = IntStr Int Modifier deriving (Eq, Ord)

instance Show IntStr where
    {-# INLINABLE show #-}
    show (IntStr i m) = show i ++ show m

-- | An unbounded range of 'IntStr'. Instances: 'Bounded', 'Eq', 'Ord', 'Show',
-- 'Monoid'.
type IntStrR = URange IntStr

-- | A mixed fractional strength value, with optional modifier. Fractions are
-- /not/ reduced. Numerator is kept less in magnitude than the denominator, with
-- the same sign as the integer part.
data FracStr = FracStr Int Int Int Modifier deriving Eq

-- | Shows (integer part)(numerator in superscript)(fraction slash)(denominator
-- in subscript)(modifier), with fraction omitted if zero
instance Show FracStr where
    {-# INLINABLE show #-}
    show (FracStr d i n m)
        | i == 0 && n /= 0 = show' n ++ "⁄" ++ show_ d ++ show m
        | n == 0 = show i ++ show m
        | otherwise = show i ++ show' n ++ "⁄" ++ show_ d ++ show m where
            show' = map (("⁰¹²³⁴⁵⁶⁷⁸⁹" !!) . (subtract $ ord '0') . ord) . show
                  . abs
            show_ = map (("₀₁₂₃₄₅₆₇₈₉" !!) . (subtract $ ord '0') . ord) . show

instance Ord FracStr where
    compare (FracStr d i n m) (FracStr d' i' n' m')
        = compare ((i * d + n) * d', m) ((i' * d' + n') * d, m')

-- | Constructs and normalizes a 'FracStr'
toFracStr :: Int -- ^ Fraction denominator
          -> Int -- ^ Integer part
          -> Int -- ^ Fraction numerator
          -> Modifier -> FracStr
{-# INLINABLE toFracStr #-}
toFracStr d i n
    | n /= 0 && d == 0 = error "zero denominator toFracStr"
    | n == 0 && d == 0 = FracStr 1 i 0
    | otherwise = FracStr (abs d) i' n'
    where (i', n') = quotRem ((i * d + n) * signum d) (abs d)

-- | An unbounded range of 'FracStr'. Instances: 'Bounded', 'Eq', 'Ord', 'Show',
-- 'Monoid'.
type FracStrR = URange FracStr

-- | Constructs an 'IntStr' with no modifier
intStr' :: Int -> IntStr
{-# INLINABLE intStr' #-}
intStr' = flip IntStr (:=)

-- | Constructs an 'IntStrR' for that exact strength value
intStrEQ :: Int -> Modifier -> IntStrR
{-# INLINABLE intStrEQ #-}
intStrEQ = _' uRangeEQ . IntStr

-- | Constructs an 'IntStrR' for that exact strength value with no modifier
intStrEQ' :: Int -> IntStrR
{-# INLINABLE intStrEQ' #-}
intStrEQ' = uRangeEQ . intStr'

-- | Constructs an 'IntStrR' for ≤ that strength value
intStrLE :: Int -> Modifier -> IntStrR
{-# INLINABLE intStrLE #-}
intStrLE = _' uRangeLE . IntStr

-- | Constructs an 'IntStrR' for ≤ that strength value with no modifier
intStrLE' :: Int -> IntStrR
{-# INLINABLE intStrLE' #-}
intStrLE' = uRangeLE . intStr'

-- | Constructs an 'IntStrR' for ≥ that strength value
intStrGE :: Int -> Modifier -> IntStrR
{-# INLINABLE intStrGE #-}
intStrGE = _' uRangeGE . IntStr

-- | Constructs an 'IntStrR' for ≥ that strength value with no modifier
intStrGE' :: Int -> IntStrR
{-# INLINABLE intStrGE' #-}
intStrGE' = uRangeGE . intStr'

-- | Constructs and validates an 'IntStrR' with strength values and modifiers
toIntStrR :: Int -> Modifier -> Int -> Modifier -> IntStrR
{-# INLINABLE toIntStrR #-}
toIntStrR i m = _' (toURange' (IntStr i m)) . IntStr

-- | Constructs and validates an 'IntStr' with strength values without modifiers
toIntStrR' :: Int -> Int -> IntStrR
{-# INLINABLE toIntStrR' #-}
toIntStrR' = liftN2 intStr' id toURange'

-- | Constructs a 'FracStr' with no modifier
fracStr' :: Int -- ^ Fraction denominatior
         -> Int -- ^ Integer part
         -> Int -- ^ Fraction numerator
         -> FracStr
{-# INLINABLE fracStr' #-}
fracStr' d i = flip (FracStr d i) (:=)

-- | Constructs a 'FracStr' with no modifier and no fractional part
fracStr'' :: Int -> FracStr
{-# INLINABLE fracStr'' #-}
fracStr'' = flip (flip (FracStr 1) 0) (:=)

-- | Constructs a 'FracStrR' for that exact strength value
fracStrEQ :: Int -- ^ Fraction denominator
          -> Int -- ^ Integer part
          -> Int -- ^ Fraction numerator
          -> Modifier -> FracStrR
{-# INLINABLE fracStrEQ #-}
fracStrEQ = _''' uRangeEQ . FracStr

-- | Constructs a 'FracStrR' for that exact strength value with no modifier
fracStrEQ' :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> FracStrR
{-# INLINABLE fracStrEQ' #-}
fracStrEQ' = _'' uRangeEQ . fracStr'

-- | Constructs a 'FracStrR' for that exact strength value with no modifer and
-- no fractional part
fracStrEQ'' :: Int -> FracStrR
{-# INLINABLE fracStrEQ'' #-}
fracStrEQ'' = uRangeEQ . fracStr''

-- | Constructs a 'FracStrR' for ≤ that strength value
fracStrLE :: Int -- ^ Fraction denominator
          -> Int -- ^ Integer part
          -> Int -- ^ Fraction numerator
          -> Modifier -> FracStrR
{-# INLINABLE fracStrLE #-}
fracStrLE = _''' uRangeLE . FracStr

-- | Constructs a 'FracStrR' for ≤ that strength value with no modifier
fracStrLE' :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> FracStrR
{-# INLINABLE fracStrLE' #-}
fracStrLE' = _'' uRangeLE . fracStr'

-- | Constructs a 'FracStrR' for ≤ that strength value with no modifier and no
-- fractional part
fracStrLE'' :: Int -> FracStrR
{-# INLINABLE fracStrLE'' #-}
fracStrLE'' = uRangeLE . fracStr''

-- | Constructs a 'FracStrR' for ≥ that strength value
fracStrGE :: Int -- ^ Fraction denominator
          -> Int -- ^ Integer part
          -> Int -- ^ Fraction numerator
          -> Modifier -> FracStrR
{-# INLINABLE fracStrGE #-}
fracStrGE = _''' uRangeGE . FracStr

-- | Constructs a 'FracStrR' for ≥ that strength value with no modifier
fracStrGE' :: Int -- ^ Fraction denominator
           -> Int -- ^ Integer part
           -> Int -- ^ Fraction numerator
           -> FracStrR
{-# INLINABLE fracStrGE' #-}
fracStrGE' = _'' uRangeLE . fracStr'

-- | Constructs a 'FracStrR' for ≥ that strength value with no modifier and no
-- fractional part
fracStrGE'' :: Int -> FracStrR
{-# INLINABLE fracStrGE'' #-}
fracStrGE'' = uRangeLE . fracStr''

-- | Constructs and validates a 'FracStrR' with strength values
toFracStrR :: Int -- ^ Common fraction denominator
           -> Int -- ^ Integer part, lower bound
           -> Int -- ^ Fraction numerator, lower bound
           -> Modifier
           -> Int -- ^ Integer part, upper bound
           -> Int -- ^ Fraction numerator, upper bound
           -> Modifier -> FracStrR
{-# INLINABLE toFracStrR #-}
toFracStrR d i n m = _'' (toURange' (FracStr d i n m)) . FracStr d

-- | Constructs and validates a 'FracStrR' with strength values without modifier
toFracStrR' :: Int -- ^ Common fraction denominator
            -> Int -- ^ Integer part, lower bound
            -> Int -- ^ Fraction numerator, lower bound
            -> Int -- ^ Integer part, upper bound
            -> Int -- ^ Fraction numerator, upper bound
            -> FracStrR
{-# INLINABLE toFracStrR' #-}
toFracStrR' d i n = _' (toURange' (fracStr' d i n)) . fracStr' d

-- | Constructs and validates a 'FracStrR' with strength values without modifier
-- nor fractional part
toFracStrR'' :: Int -> Int -> FracStrR
{-# INLINABLE toFracStrR'' #-}
toFracStrR'' = liftN2 fracStr'' id toURange'

