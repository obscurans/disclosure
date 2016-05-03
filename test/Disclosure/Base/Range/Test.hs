module Disclosure.Base.Range.Test where

import Data.Maybe
import Disclosure.Test.Util
import Disclosure.Base.Util
import Disclosure.Base.Range.Internal
--import Disclosure.Base.Range

tests :: TestTree
tests = testGroup "Disclosure.Base.Range"
    [ _'toURange
    , _'toBRange
    , _'showURange
    , _'showBRange
    , _'ordURange
    , _'ordBRange
    , _'monoid'URange
    , _'monoid'BRange
    , _'punionUR
    , _'punionBR ]

_'toURange = setSCDepth 10 $ sTestProperty "toURange" _toURange

_toURange :: (Maybe Smallint, Maybe Smallint) -> Bool
_toURange z@(x, y)
    | compareMaybeR x y == GT = failure
    | otherwise = success
    where r = toURange z
          failure = r == Nothing
          success = r == Just (URange z)

_'toBRange = setSCDepth 10 $ sTestProperty "toBRange" _toBRange

_toBRange :: (Smallint, Smallint) -> Bool
_toBRange z@(x, y)
    | x < minBound = failure
    | y > maxBound = failure
    | x > y = failure
    | otherwise = success
    where r = toBRange z
          failure = r == Nothing
          success = r == Just (BRange z)

_'showURange = setSCDepth 12 $ testGroup "Show URange"
    [ testCase "universal" _showURange0
    , sTestProperty "singleton" _showURange1
    , sTestProperty "left-infinite" _showURange2
    , sTestProperty "right-infinite" _showURange3
    , sTestProperty "proper" _showURange4 ]

_showURange0 = fmap show (toURange (Nothing, nothingSmallint)) @=? Just "?"

_showURange1 :: Smallint -> Bool
_showURange1 x = fmap show (toURange (Just x, Just x)) == Just (show x)

_showURange2 :: Smallint -> Bool
_showURange2 x = fmap show (toURange (Nothing, Just x)) == Just (show x ++ "-")

_showURange3 :: Smallint -> Bool
_showURange3 x = fmap show (toURange (Just x, Nothing)) == Just (show x ++ "+")

_showURange4 :: SOPSmallint -> Bool
_showURange4 (SOPSmallint (x, y)) = fmap show (toURange (Just x, Just y))
                                == Just (show x ++ "–" ++ show y)

_'showBRange = setSCDepth 12 $ testGroup "Show BRange"
    [ testCase "universal" _showBRange0
    , sTestProperty "singleton" _showBRange1
    , sTestProperty "left-infinite" _showBRange2
    , sTestProperty "right-infinite" _showBRange3
    , sTestProperty "proper" _showBRange4 ]

_showBRange0 = fmap show (toBRange (minBound, maxBound :: BASmallint))
                @=? Just "?"

_showBRange1 :: BASmallint -> Bool
_showBRange1 x = fmap show (toBRange (x, x)) == Just (show x)

_showBRange2 :: BASmallint -> Bool
_showBRange2 x = fmap show (toBRange (minBound, x)) == Just (show x ++ "-")

_showBRange3 :: BASmallint -> Bool
_showBRange3 x = fmap show (toBRange (x, maxBound)) == Just (show x ++ "+")

_showBRange4 :: SOPBASmallint -> Bool
_showBRange4 (SOPBASmallint (x, y)) = fmap show (toBRange (x, y))
                                == Just (show x ++ "–" ++ show y)

_'ordURange = setSCDepth 7 $ testGroup "Ord URange"
    [ sTestProperty "compare" _ordURange ]

toSmallURange :: MOPSmallint -> URange Smallint
toSmallURange (MOPSmallint z) = fromJust $ toURange z

_ordURange :: MOPSmallint -> MOPSmallint -> Bool
_ordURange a@(MOPSmallint (x, y)) b@(MOPSmallint (w, z))
    | compareNL y z == LT = check LT
    | compareNL y z == GT = check GT
    | x < w = check GT
    | x > w = check LT
    | otherwise = check EQ
    where compareNL = liftN2 NLast id compare
          check = (compare (toSmallURange a) (toSmallURange b) ==)

_'ordBRange = setSCDepth 7 $ testGroup "Ord BRange"
    [ sTestProperty "compare" _ordBRange ]

toSmallBRange :: OPBSmallint -> BRange BSmallint
toSmallBRange (OPBSmallint z) = fromJust $ toBRange z

_ordBRange :: OPBSmallint -> OPBSmallint -> Bool
_ordBRange a@(OPBSmallint (x, y)) b@(OPBSmallint (w, z))
    | y < z = check LT
    | y > z = check GT
    | x < w = check GT
    | x > w = check LT
    | otherwise = check EQ
    where check = (compare (toSmallBRange a) (toSmallBRange b) ==)

_'monoid'URange = testGroup "Monoid' URange"
    [ setSCDepth 15 $ sTestProperty "mempty'" _mempty'URange
    , setSCDepth 7 $ sTestProperty "mappend'" _mappend'URange ]

_mempty'URange :: MOPSmallint -> Bool
_mempty'URange x = fromJust (mappend' y mempty') == y &&
                   fromJust (mappend' mempty' y) == y
                   where y = toSmallURange x

_mappend'URange :: MOPSmallint -> MOPSmallint -> Bool
_mappend'URange a@(MOPSmallint (x, y)) b@(MOPSmallint (w, z))
    | compareMaybeR maxl minh == GT = failure
    | otherwise = check $ fromJust $ toURange (maxl, minh)
    where maxl = max x w
          minh = liftN2 NLast unNLast min y z
          a' = toSmallURange a
          b' = toSmallURange b
          failure = isNothing $ mappend' a' b'
          check = (mappend' a' b' ==) . Just

_'monoid'BRange = testGroup "Monoid' BRange"
    [ setSCDepth 15 $ sTestProperty "mempty'" _mempty'BRange
    , setSCDepth 7 $ sTestProperty "mappend'" _mappend'BRange ]

_mempty'BRange :: OPBSmallint -> Bool
_mempty'BRange x = fromJust (mappend' y mempty') == y &&
                   fromJust (mappend' mempty' y) == y
                   where y = toSmallBRange x

_mappend'BRange :: OPBSmallint -> OPBSmallint -> Bool
_mappend'BRange a@(OPBSmallint (x, y)) b@(OPBSmallint (w, z))
    | maxl > minh = failure
    | otherwise = check $ fromJust $ toBRange (maxl, minh)
    where maxl = max x w
          minh = min y z
          a' = toSmallBRange a
          b' = toSmallBRange b
          failure = isNothing $ mappend' a' b'
          check = (mappend' a' b' ==) . Just

_'punionUR = setSCDepth 7 $ sTestProperty "punionUR" _punionUR

_punionUR :: MOPSmallint -> MOPSmallint -> Bool
_punionUR a@(MOPSmallint (x, y)) b@(MOPSmallint (w, z)) =
    punionUR (toSmallURange a) (toSmallURange b) == result
    where result = fromJust $ toURange (min x w, liftN2 NLast unNLast max y z)

_'punionBR = setSCDepth 7 $ sTestProperty "punionBR" _punionBR

_punionBR :: OPBSmallint -> OPBSmallint -> Bool
_punionBR a@(OPBSmallint (x, y)) b@(OPBSmallint (w, z)) =
    punionBR (toSmallBRange a) (toSmallBRange b) == result
    where result = fromJust $ toBRange (min x w, max y z)

