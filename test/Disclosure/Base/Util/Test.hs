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

_'Monoid' = adjSCDepth 1 $ testGroup "Monoid'"
    [ sTestProperty "mconcat'" _mconcat' ]

newtype LSmallint = LSmallint [Smallint] deriving (Eq, Show)

instance Monoid' LSmallint where
    mempty' = LSmallint []
    mappend' (LSmallint [0]) _ = Nothing
    mappend' _ (LSmallint [0]) = Nothing
    mappend' (LSmallint x) (LSmallint y) = Just $ LSmallint $ x ++ y

_mconcat' :: [Smallint] -> Bool
_mconcat' x = mconcat' (fmap (LSmallint . (:[])) x) ==
                if any (== 0) x then Nothing else Just $ LSmallint x

_'toMaybe = adjSCDepth 5 $ sTestProperty "toMaybe" _toMaybe

_toMaybe :: Smallint -> Bool
_toMaybe x = toMaybe odd x == if odd x then Just x else Nothing

_'NothingLast = adjSCDepth 5 $ testGroup "NothingLast"
    [ testCase "both Nothing" _NothingLast0
    , sTestProperty "1 Nothing" _NothingLast1
    , sTestProperty "both Just" _NothingLast2 ]

_NothingLast0 = compare (NLast Nothing) (NLast nothingSmallint) @=? EQ

_NothingLast1 :: Smallint -> Bool
_NothingLast1 x = compare (NLast Nothing) (NLast $ Just x) == GT &&
                  compare (NLast $ Just x) (NLast Nothing) == LT

_NothingLast2 :: Smallint -> Smallint -> Bool
_NothingLast2 x y = compare (NLast $ Just x) (NLast $ Just y) == compare x y

_'compareMaybeR = adjSCDepth 5 $ testGroup "compareMaybeR"
    [ testCase "both Nothing" _compareMaybeR0
    , sTestProperty "1 Nothing" _compareMaybeR1
    , sTestProperty "both Just" _compareMaybeR2 ]

_compareMaybeR0 = compareMaybeR Nothing nothingSmallint @=? LT

_compareMaybeR1 :: Smallint -> Bool
_compareMaybeR1 x = compareMaybeR Nothing (Just x) == LT &&
                    compareMaybeR (Just x) Nothing == LT

_compareMaybeR2 :: Smallint -> Smallint -> Bool
_compareMaybeR2 x y = compareMaybeR (Just x) (Just y) == compare x y

_'liftAbsorb2 = adjSCDepth 5 $ testGroup "liftAbsorb2"
    [ testCase "both Nothing" _liftAbsorb20
    , sTestProperty "1 Nothing" _liftAbsorb21
    , sTestProperty "both Just" _liftAbsorb22 ]

_liftAbsorb20 = liftAbsorb2 (-) Nothing nothingSmallint @=? Nothing

_liftAbsorb21 :: Smallint -> Bool
_liftAbsorb21 x = liftAbsorb2 (-) Nothing (Just x) == Just x &&
                  liftAbsorb2 (-) (Just x) Nothing == Just x

_liftAbsorb22 :: Smallint -> Smallint -> Bool
_liftAbsorb22 x y = liftAbsorb2 (-) (Just x) (Just y) == Just (x - y)

sub' :: Smallint -> Smallint -> Maybe Smallint
sub' x y = if odd (x - y) then Nothing else Just (x - y)

_'liftAbsorbM2 = adjSCDepth 5 $ testGroup "liftAbsorbM2"
    [ testCase "both Nothing" _liftAbsorbM20
    , sTestProperty "1 Nothing" _liftAbsorbM21
    , sTestProperty "both Just" _liftAbsorbM22 ]

_liftAbsorbM20 = liftAbsorbM2 sub' Nothing Nothing @=? Nothing

_liftAbsorbM21 :: Smallint -> Bool
_liftAbsorbM21 x = liftAbsorbM2 sub' Nothing (Just x) == Just x &&
                   liftAbsorbM2 sub' (Just x) Nothing == Just x

_liftAbsorbM22 :: Smallint -> Smallint -> Bool
_liftAbsorbM22 x y = liftAbsorbM2 sub' (Just x) (Just y) == sub' x y

_'compareMeet = adjSCDepth 10 $ sTestProperty "compareMeet" _compareMeet

_compareMeet :: Latticeint -> Latticeint -> Bool
_compareMeet x y
    | x == y = r == EQ
    | mod y x == 0 = r == LT
    | mod x y == 0 = r == GT
    | otherwise = r == EQ
    where r = compareMeet x y

_'compareMeet' = adjSCDepth 10 $ sTestProperty "compareMeet'" _compareMeet'

_compareMeet' :: Latticeint -> Latticeint -> Bool
_compareMeet' x y
    | x == y = r == EQ
    | mod y x == 0 = r == LT
    | mod x y == 0 = r == GT
    | otherwise = r == EQ
    where r = compareMeet' x y

