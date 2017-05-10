{-|
Module      : Disclosure.Base.Bid.Internal
Description : Internal datatype definitions and functions
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable
-}
module Disclosure.Base.Bid.Internal where

import Data.Char

-- | A boxed 'Int' for a bid level. Checked to be in [1, 7].
newtype Lv = Lv {
    -- | Unboxes a Lv
    unLv :: Int } deriving (Eq, Ord)

-- | Checks and 'Maybe' constructs a 'Lv'
toLv :: Int -> Maybe Lv
{-# INLINABLE toLv #-}
toLv l
    | l < 1 || l > 7 = Nothing
    | otherwise = Just $ Lv l

-- | 1 ≤ Lv ≤ 7
instance Bounded Lv where
    {-# INLINABLE minBound #-}
    minBound = Lv 1
    {-# INLINABLE maxBound #-}
    maxBound = Lv 7

-- | Natural mapping from [0, 6] -> [1, 7]
instance Enum Lv where
    {-# INLINABLE fromEnum #-}
    fromEnum (Lv l)
        | l < 1 || l > 7 = error "invalid fromEnum Lv"
        | otherwise = l - 1
    {-# INLINABLE toEnum #-}
    toEnum i
        | i < 0 || i > 6 = error "invalid toEnum Lv"
        | otherwise = Lv (i + 1)

-- | Directly as 'Int', checked
instance Read Lv where
    {-# INLINABLE readsPrec #-}
    readsPrec _ [] = []
    readsPrec _ (x:xt)
        | x < '1' || x > '7' = []
        | otherwise = [(Lv (ord x - ord '0'), xt)]
