{-|
Module      : Disclosure.Base.Bid
Description : Datatypes for denominations, bids, and calls
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines four enum types for levels, denominations, bids, and non-bid calls.
-}
module Disclosure.Base.Bid (
-- * Datatypes and constructors
  Lv
, toLv
, unLv
, Denom(..)
, Bid(..)
, Call(..)
) where

import Data.Char
import Disclosure.Base.Bid.Internal

matchCIPre :: String -> String -> Bool
matchCIPre [] _ = True
matchCIPre _ [] = False
matchCIPre (x:xs) (y:ys) = if (toUpper x == toUpper y)
                           then matchCIPre xs ys else False

-- | Directly as 'Int'
instance Show Lv where
    {-# INLINABLE show #-}
    show = show . unLv

-- | Enumeration of the 5 denominations
data Denom = C | D | H | S | NT deriving (Eq, Ord, Bounded, Enum, Show)

-- | Accepts any case-insensitive prefix of the denomination names @\"clubs\"@,
-- @\"diamonds\"@, @\"hearts\"@, @\"spades\"@, @\"notrumps\"@, @\"nt\"@.
-- __TODO:__ allow parses with remainder.
instance Read Denom where
    {-# INLINABLE readsPrec #-}
    readsPrec _ xs
        | [] <- xs = []
        | matchCIPre xs "clubs" = succeed C
        | matchCIPre xs "diamonds" = succeed D
        | matchCIPre xs "hearts" = succeed H
        | matchCIPre xs "spades" = succeed S
        | matchCIPre xs "notrumps" = succeed NT
        | matchCIPre xs "nt" = succeed NT
        | otherwise = []
        where
            succeed x = [(x, [])]

-- | Level and denomination combination for a bid
data Bid = Bid { level :: Lv, denom :: Denom } deriving (Eq, Ord, Bounded)

-- | Natural order, @0@⇔@1C@ through @34@⇔@7NT@
instance Enum Bid where
    {-# INLINABLE fromEnum #-}
    fromEnum (Bid (Lv l) d)
        | l < 1 || l > 7 = error "invalid fromEnum Bid"
        | otherwise = (l - 1) * 5 + (fromEnum d)
    {-# INLINABLE toEnum #-}
    toEnum i
        | i < 0 || i > 34 = error "invalid toEnum Bid"
        | otherwise = Bid (Lv (i `div` 5 + 1)) (toEnum (i `mod` 5))

instance Show Bid where
    {-# INLINABLE show #-}
    show (Bid l d) = show l ++ show d

-- | No space between 'Lv' and 'Denom'.
instance Read Bid where
    {-# INLINABLE readsPrec #-}
    readsPrec x xt = [ (Bid l d, r) | (l, xs) <- readsPrec x xt,
                                      (d, r) <- readsPrec x xs ]

-- | Enumeration of the non-bid calls
data Call = Pass | Dbl | Rdbl deriving (Eq, Bounded, Enum, Show)

-- | Accepts any case-insensitive prefix of the names @\"pass\"@,
-- @\"(re)?double\"@, @\"(re?)?dbl\"@, as well as @\"xx\"@ and @\"x\"@.
-- __TODO:__ allow parses with remainder.
instance Read Call where
    {-# INLINABLE readsPrec #-}
    readsPrec _ x
        | matchCIPre x "pass" = succeed Pass
        | matchCIPre x "double" = succeed Dbl
        | matchCIPre x "dbl" = succeed Dbl
        | matchCIPre x "redouble" = succeed Rdbl
        | matchCIPre x "redbl" = succeed Rdbl
        | matchCIPre x "rdbl" = succeed Rdbl
        | matchCIPre x "xx" = succeed Rdbl
        | matchCIPre x "x" = succeed Dbl
        | otherwise = []
        where succeed y = [(y, [])]
