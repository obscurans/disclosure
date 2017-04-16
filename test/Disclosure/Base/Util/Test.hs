module Disclosure.Base.Util.Test where

import Data.Maybe
import Disclosure.Test.Util
import Disclosure.Base.Util

tests :: TestTree
tests = testGroup "Disclosure.Base.Util"
    [ testGroup "Monoid'"
        [ testLSI "mconcat'" _mconcat' ]
    , testSI "toMaybe" _toMaybe
    , testMSI2 "NothingLast" _NothingLast
    , testMSI2 "compareMaybeR" _compareMaybeR
    , testGroup "liftAbsorb2"
        [ testMSI2 "const" $ _liftAbsorb2 $ const $ const $ SI 1
        , testMSI2 "sub" $ _liftAbsorb2 (-) ]
    , testGroup "liftAbsorbM2"
        [ testMSI2 "Nothing" $ _liftAbsorbM2 $ const $ const $ Nothing
        , testMSI2 "const" $ _liftAbsorbM2 $ const $ const $ Just $ SI 1
        , testMSI2 "sub'" $ _liftAbsorbM2
                        (\x y -> if odd (x - y) then Nothing else Just (x - y))
        ]
    , testLI2 "compareMeet" _compareMeet
    , testLI2 "compareMeet'" _compareMeet' ]

newtype LSmallint = LSI [Smallint] deriving (Eq, Show)

instance Monoid' LSmallint where
    mempty' = LSI []
    mappend' (LSI [0]) _ = Nothing
    mappend' _ (LSI [0]) = Nothing
    mappend' (LSI x) (LSI y) = Just $ LSI $ x ++ y

_mconcat' :: [Smallint] -> Bool
_mconcat' x = mconcat' (fmap (LSI . (:[])) x) ==
                if any (== 0) x then Nothing else Just $ LSI x

_toMaybe :: Smallint -> Bool
_toMaybe x = toMaybe odd x == if odd x then Just x else Nothing

_NothingLast :: Maybe Smallint -> Maybe Smallint -> Bool
_NothingLast x@Nothing Nothing = compare (NLast x) (NLast x) == EQ
_NothingLast Nothing x@(Just _) = compare (NLast Nothing) (NLast x) == GT
_NothingLast x@(Just _) Nothing = compare (NLast x) (NLast Nothing) == LT
_NothingLast x@(Just w) y@(Just z) = compare (NLast x) (NLast y) == compare w z

_compareMaybeR :: Maybe Smallint -> Maybe Smallint -> Bool
_compareMaybeR Nothing x = compareMaybeR Nothing x == LT
_compareMaybeR x Nothing = compareMaybeR x Nothing == LT
_compareMaybeR x@(Just x') y@(Just y') = compareMaybeR x y == compare x' y'

_liftAbsorb2 :: (Smallint -> Smallint -> Smallint)
             -> Maybe Smallint -> Maybe Smallint -> Bool
_liftAbsorb2 f x@Nothing Nothing = liftAbsorb2 f x x == Nothing
_liftAbsorb2 f Nothing x = liftAbsorb2 f Nothing x == x
_liftAbsorb2 f x Nothing = liftAbsorb2 f x Nothing == x
_liftAbsorb2 f x@(Just x') y@(Just y') = liftAbsorb2 f x y == Just (f x' y')

sub' :: Smallint -> Smallint -> Maybe Smallint
sub' x y = if odd (x - y) then Nothing else Just (x - y)

_liftAbsorbM2 :: (Smallint -> Smallint -> Maybe Smallint)
              -> Maybe Smallint -> Maybe Smallint -> Bool
_liftAbsorbM2 f x@Nothing Nothing = liftAbsorbM2 f x x == Nothing
_liftAbsorbM2 f Nothing x = liftAbsorbM2 f Nothing x == x
_liftAbsorbM2 f x Nothing = liftAbsorbM2 f x Nothing == x
_liftAbsorbM2 f x@(Just x') y@(Just y') = liftAbsorbM2 f x y == f x' y'

_compareMeet :: Latticeint -> Latticeint -> Bool
_compareMeet x y
    | x == y = r == EQ
    | mod y x == 0 = r == LT
    | mod x y == 0 = r == GT
    | otherwise = r == EQ
    where r = compareMeet x y

_compareMeet' :: Latticeint -> Latticeint -> Bool
_compareMeet' x y
    | x == y = r == EQ
    | mod y x == 0 = r == LT
    | mod x y == 0 = r == GT
    | otherwise = r == EQ
    where r = compareMeet' x y
