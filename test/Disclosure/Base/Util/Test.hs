module Disclosure.Base.Util.Test where

import Disclosure.Test.Util
import Disclosure.Base.Util

tests :: TestTree
tests = testGroup "Disclosure.Base.Util"
    [ _'Monoid'
    , _'toMaybe
    , _'NothingLast
    , _'compareMaybeR
    , _'liftAbsorb2
    , _'liftAbsorbM2
    , _'compareMeet
    , _'compareMeet' ]

_'Monoid' = setSCDepth 6 $ testGroup "Monoid'"
    [ sTestProperty "mconcat'" _mconcat' ]

newtype LSmallint = LSI [Smallint] deriving (Eq, Show)

instance Monoid' LSmallint where
    mempty' = LSI []
    mappend' (LSI [0]) _ = Nothing
    mappend' _ (LSI [0]) = Nothing
    mappend' (LSI x) (LSI y) = Just $ LSI $ x ++ y

_mconcat' :: [Smallint] -> Bool
_mconcat' x = mconcat' (fmap (LSI . (:[])) x) ==
                if any (== 0) x then Nothing else Just $ LSI x

_'toMaybe = setSCDepth 15 $ sTestProperty "toMaybe" _toMaybe

_toMaybe :: Smallint -> Bool
_toMaybe x = toMaybe odd x == if odd x then Just x else Nothing

_'NothingLast = setSCDepth 15 $ sTestProperty "NothingLast" _NothingLast

_NothingLast :: Maybe Smallint -> Maybe Smallint -> Bool
_NothingLast x@Nothing Nothing = compare (NLast x) (NLast x) == EQ
_NothingLast Nothing x@(Just _) = compare (NLast Nothing) (NLast x) == GT
_NothingLast x@(Just _) Nothing = compare (NLast x) (NLast Nothing) == LT
_NothingLast x@(Just w) y@(Just z) = compare (NLast x) (NLast y) == compare w z

_'compareMaybeR = setSCDepth 15 $ sTestProperty "compareMaybeR" _compareMaybeR

_compareMaybeR :: Maybe Smallint -> Maybe Smallint -> Bool
_compareMaybeR Nothing x = compareMaybeR Nothing x == LT
_compareMaybeR x Nothing = compareMaybeR x Nothing == LT
_compareMaybeR x@(Just x') y@(Just y') = compareMaybeR x y == compare x' y'

_'liftAbsorb2 = setSCDepth 15 $ sTestProperty "liftAbsorb2" _liftAbsorb2

_liftAbsorb2 :: Maybe Smallint -> Maybe Smallint -> Bool
_liftAbsorb2 x@Nothing Nothing = liftAbsorb2 (-) x x == Nothing
_liftAbsorb2 Nothing x = liftAbsorb2 (-) Nothing x == x
_liftAbsorb2 x Nothing = liftAbsorb2 (-) x Nothing == x
_liftAbsorb2 x@(Just x') y@(Just y') = liftAbsorb2 (-) x y == Just (x' - y')

sub' :: Smallint -> Smallint -> Maybe Smallint
sub' x y = if odd (x - y) then Nothing else Just (x - y)

_'liftAbsorbM2 = setSCDepth 15 $ sTestProperty "liftAbsorbM2" _liftAbsorbM2

_liftAbsorbM2 :: Maybe Smallint -> Maybe Smallint -> Bool
_liftAbsorbM2 x@Nothing Nothing = liftAbsorbM2 sub' x x == Nothing
_liftAbsorbM2 Nothing x = liftAbsorbM2 sub' Nothing x == x
_liftAbsorbM2 x Nothing = liftAbsorbM2 sub' x Nothing == x
_liftAbsorbM2 x@(Just x') y@(Just y') = liftAbsorbM2 sub' x y == sub' x' y'

_'compareMeet = setSCDepth 15 $ sTestProperty "compareMeet" _compareMeet

_compareMeet :: Latticeint -> Latticeint -> Bool
_compareMeet x y
    | x == y = r == EQ
    | mod y x == 0 = r == LT
    | mod x y == 0 = r == GT
    | otherwise = r == EQ
    where r = compareMeet x y

_'compareMeet' = setSCDepth 15 $ sTestProperty "compareMeet'" _compareMeet'

_compareMeet' :: Latticeint -> Latticeint -> Bool
_compareMeet' x y
    | x == y = r == EQ
    | mod y x == 0 = r == LT
    | mod x y == 0 = r == GT
    | otherwise = r == EQ
    where r = compareMeet' x y

