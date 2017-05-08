{-|
Module      : Disclosure.Base.Util
Description : Internal utility functions
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Internal utility functions
-}
module Disclosure.Base.Util where

import Data.Tuple.Curry (curryN, uncurryN) --tuple
import Data.Tuple.Homogenous (Tuple2(..), Tuple4(..)) --tuples-homogenous-h98
import Control.Monad

-- | A possibly failing 'Monoid', where 'mappend'' returns 'Maybe' @a@ instead
class Monoid' a where
    {-# MINIMAL mempty', mappend' #-}
    mempty' :: a
    mappend' :: a -> a -> Maybe a
    mconcat' :: [a] -> Maybe a
    mconcat' = foldr (_' (=<<) mappend') $ Just mempty'

-- | Modifies a composing function to \"wait for one argument\"
_' :: (b -> c) -> (a -> b) -> a -> c
{-# INLINABLE _' #-}
_' = (.)

-- | Modifies a composing function to \"wait for two arguments\"
_'' :: (c -> d) -> (a -> b -> c) -> a -> b -> d
{-# INLINABLE _'' #-}
_'' = _' . _'

-- | Modifies a composing function to \"wait for three arguments\"
_''' :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
{-# INLINABLE _''' #-}
_''' = _' . _' . _'

-- | Modifies a composing function to \"wait for four arguments\"
_'''' :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
{-# INLINABLE _'''' #-}
_'''' = _' . _' . _' . _'

-- | Modifies a composing function to \"skip one argument\"
__' :: (a -> b) -> (b -> c) -> a -> c
{-# INLINABLE __' #-}
__' = flip (.)

-- | Modifies an 'Applicative' function to be curried normally
applyA2 :: Applicative f => f (a -> b -> c) -> f a -> f b -> f c
{-# INLINABLE applyA2 #-}
applyA2 f a = (f <*> a <*>)

-- | Lifts a 2-function using a un-constructor and an output constructor
liftN2 :: (c -> a) -> (b -> d) -> (a -> a -> b) -> c -> c -> d
{-# INLINABLE liftN2 #-}
liftN2 f g = curry . _' g . __' (untuple2 . fmap f . Tuple2) . uncurry

-- | Lifts a 4-function using a un-constructor and an output constructor
liftN4 :: (c -> a) -> (b -> d) -> (a -> a -> a -> a -> b) ->
          c -> c -> c -> c -> d
{-# INLINABLE liftN4 #-}
liftN4 f g = curryN . _' g . __' (untuple4 . fmap f . Tuple4) . uncurryN

-- | Validates with the given function or flushes to 'Nothing'
toMaybe :: (a -> Bool) -> a -> Maybe a
{-# INLINABLE toMaybe #-}
toMaybe f x = if f x then Just x else Nothing

-- | Newtype wrapper for 'Maybe' @a@ that orders 'Nothing' last, 'Just' normally
newtype NothingLast a = NLast { unNLast :: (Maybe a) } deriving Eq
-- | 'Nothing' is last, 'Just' inherits 'Ord'
instance Ord a => Ord (NothingLast a) where
    {-# INLINABLE compare #-}
    compare (NLast Nothing) (NLast Nothing) = EQ
    compare (NLast Nothing) (NLast _) = GT
    compare (NLast _) (NLast Nothing) = LT
    compare (NLast x) (NLast y) = compare x y

-- | Compares two elements, with first 'Nothing' being -∞, second 'Nothing'
-- being +∞
compareMaybeR :: Ord a => Maybe a -> Maybe a -> Ordering
{-# INLINABLE compareMaybeR #-}
compareMaybeR = _' (maybe LT id) . liftM2 compare

-- | Lifts a binary function into 'Maybe' in an absorptive way: if one argument
-- is 'Nothing' it returns the other, else applies the function
liftAbsorb2 :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
{-# INLINABLE liftAbsorb2 #-}
liftAbsorb2 _ Nothing y = y
liftAbsorb2 _ x@(Just _) Nothing = x
liftAbsorb2 f (Just x) (Just y) = Just $ f x y

-- | Lifts a binary monadic function into 'Maybe' in an absorptive way: if one
-- argument is 'Nothing' it returns the other, else applies the function
liftAbsorbM2 :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
{-# INLINABLE liftAbsorbM2 #-}
liftAbsorbM2 _ Nothing y = y
liftAbsorbM2 _ x@(Just _) Nothing = x
liftAbsorbM2 f (Just x) (Just y) = f x y

-- | Compares using the 'Monoid' operation, which is presumed to be a meet
-- lattice operation.
compareMeet :: (Eq a, Monoid a) => a -> a -> Ordering
{-# INLINABLE compareMeet #-}
compareMeet x y
    | x == y = EQ
    | intersect == x = LT
    | intersect == y = GT
    | otherwise = EQ
    where intersect = mappend x y

-- | Compares using the 'Monoid'' operation, which is presumed to be a meet
-- lattice operation, which failure represents a result of lattice bottom.
compareMeet' :: (Eq a, Monoid' a) => a -> a -> Ordering
{-# INLINABLE compareMeet' #-}
compareMeet' x y
    | x == y = EQ
    | intersect == Just x = LT
    | intersect == Just y = GT
    | otherwise = EQ
    where intersect = mappend' x y

-- | Utility to simplify 'readsPrec' returns
readSucc :: b -> a -> [(a, b)]
{-# INLINABLE readSucc #-}
readSucc y x = [(x, y)]
