{-|
Module      : Disclosure.Base.Util
Description : Internal utility functions
Copyright   : (c) 2016 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Internal utility functions
-}
module Disclosure.Base.Util where

import Data.Tuple.Curry --tuples
import Data.Tuple.Homogenous --tuples-homogenous-h98
import Control.Applicative

-- | Modifies a composing function to \"wait for one argument\"
_' :: (b -> c) -> (a -> b) -> (a -> c)
_' = (.)

-- | Modifies a composing function to \"wait for two arguments\"
_'' :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
_'' = _' . _'

-- | Modifies a composing function to \"skip one argument\"
__' :: (a -> b) -> (b -> c) -> (a -> c)
__' = flip (.)

-- | Modifies an 'Applicative' function to be curried normally
applyA2 :: Applicative f => f (a -> b -> c) -> f a -> f b -> f c
applyA2 f a b = f <*> a <*> b

-- | Lifts a 2-function using a un-constructor and an output constructor
liftN2 :: (c -> a) -> (b -> d) -> (a -> a -> b) -> (c -> c -> d)
liftN2 f g = curry . _' g . __' (untuple2 . fmap f . Tuple2) . uncurry

-- | Lifts a 4-function using a un-constructor and an output constructor
liftN4 :: (c -> a) -> (b -> d) -> (a -> a -> a -> a -> b) -> (c -> c -> c -> c -> d)
liftN4 f g = curryN . _' g . __' (untuple4 . fmap f . Tuple4) . uncurryN

-- | Utility to simplify 'readsPrec' returns
readSucc :: b -> a -> [(a, b)]
readSucc y x = [(x, y)]

-- | Utility for doing suit computations with (all other suits, current suit)
butterfly :: ([a] -> a -> b) -> (a, a, a, a) -> (b, b, b, b)
butterfly f (s, h, d, c) = (f [h,d,c] s, f [s,d,c] h, f [s,h,c] d, f [s,h,d] c)
