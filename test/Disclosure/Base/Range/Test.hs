module Disclosure.Base.Range.Test where

import Data.Maybe
import Disclosure.Test.Util
import Disclosure.Base.Util
import Disclosure.Base.Range.Internal
import Disclosure.Base.Range

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
    , _'punionBR
    , _'toURange'
    , _'uRangeEQ
    , _'bRangeEQ
    , _'uRangeLE
    , _'bRangeLE
    , _'uRangeGE
    , _'bRangeGE ]

_'toURange = setSCDepth 15 $ sTestProperty "toURange" _toURange

_toURange :: (Maybe Smallint, Maybe Smallint) -> Bool
_toURange z@(x, y)
    | compareMaybeR x y == GT = failure
    | otherwise = success
    where r = toURange z
          failure = r == Nothing
          success = r == Just (URange z)

_'toBRange = setSCDepth 15 $ sTestProperty "toBRange" _toBRange

_toBRange :: (Smallint, Smallint) -> Bool
_toBRange z@(x, y)
    | x < minBound = failure
    | y > maxBound = failure
    | x > y = failure
    | otherwise = success
    where r = toBRange z
          failure = r == Nothing
          success = r == Just (BRange z)

toSURange :: MOPSmallint -> URange Smallint
toSURange (MOPSI z) = fromJust $ toURange z

_'showURange = setSCDepth 20 $ testGroup "Show URange"
    [ sTestProperty "show" _showURange ]

_showURange :: MOPSmallint -> Bool
_showURange x@(MOPSI (Nothing, Nothing)) = show (toSURange x) == "?"
_showURange x@(MOPSI (Nothing, Just y)) = show (toSURange x) == show y ++ "-"
_showURange x@(MOPSI (Just y, Nothing)) = show (toSURange x) == show y ++ "+"
_showURange x@(MOPSI (Just y, Just z))
    | y == z = show (toSURange x) == show y
    | otherwise = show (toSURange x) == show y ++ "–" ++ show z

toSBRange :: OPBSmallint -> BRange BSmallint
toSBRange (OPBSI z) = fromJust $ toBRange z

_'showBRange = setSCDepth 20 $ testGroup "Show BRange"
    [ sTestProperty "show" _showBRange ]

_showBRange :: OPBSmallint -> Bool
_showBRange x@(OPBSI (y, z))
    | y == minBound && z == maxBound = check "?"
    | y == z = check $ show y
    | y == minBound = check $ show z ++ "-"
    | z == maxBound = check $ show y ++ "+"
    | otherwise = check $ show y ++ "–" ++ show z
    where check = (show (toSBRange x) ==)

_'ordURange = setSCDepth 7 $ testGroup "Ord URange"
    [ sTestProperty "compare" _ordURange ]

_ordURange :: MOPSmallint -> MOPSmallint -> Bool
_ordURange a@(MOPSI (x, y)) b@(MOPSI (w, z))
    | compareNL y z == LT = check LT
    | compareNL y z == GT = check GT
    | x < w = check GT
    | x > w = check LT
    | otherwise = check EQ
    where compareNL = liftN2 NLast id compare
          check = (compare (toSURange a) (toSURange b) ==)

_'ordBRange = setSCDepth 7 $ testGroup "Ord BRange"
    [ sTestProperty "compare" _ordBRange ]

_ordBRange :: OPBSmallint -> OPBSmallint -> Bool
_ordBRange a@(OPBSI (x, y)) b@(OPBSI (w, z))
    | y < z = check LT
    | y > z = check GT
    | x < w = check GT
    | x > w = check LT
    | otherwise = check EQ
    where check = (compare (toSBRange a) (toSBRange b) ==)

_'monoid'URange = testGroup "Monoid' URange"
    [ setSCDepth 15 $ sTestProperty "mempty'" _mempty'URange
    , setSCDepth 7 $ sTestProperty "mappend'" _mappend'URange ]

_mempty'URange :: MOPSmallint -> Bool
_mempty'URange x = fromJust (mappend' y mempty') == y &&
                   fromJust (mappend' mempty' y) == y
                   where y = toSURange x

_mappend'URange :: MOPSmallint -> MOPSmallint -> Bool
_mappend'URange a@(MOPSI (x, y)) b@(MOPSI (w, z))
    | compareMaybeR maxl minh == GT = failure
    | otherwise = check $ fromJust $ toURange (maxl, minh)
    where maxl = max x w
          minh = liftN2 NLast unNLast min y z
          a' = toSURange a
          b' = toSURange b
          failure = isNothing $ mappend' a' b'
          check = (mappend' a' b' ==) . Just

_'monoid'BRange = testGroup "Monoid' BRange"
    [ setSCDepth 15 $ sTestProperty "mempty'" _mempty'BRange
    , setSCDepth 7 $ sTestProperty "mappend'" _mappend'BRange ]

_mempty'BRange :: OPBSmallint -> Bool
_mempty'BRange x = fromJust (mappend' y mempty') == y &&
                   fromJust (mappend' mempty' y) == y
                   where y = toSBRange x

_mappend'BRange :: OPBSmallint -> OPBSmallint -> Bool
_mappend'BRange a@(OPBSI (x, y)) b@(OPBSI (w, z))
    | maxl > minh = failure
    | otherwise = check $ fromJust $ toBRange (maxl, minh)
    where maxl = max x w
          minh = min y z
          a' = toSBRange a
          b' = toSBRange b
          failure = isNothing $ mappend' a' b'
          check = (mappend' a' b' ==) . Just

_'punionUR = setSCDepth 7 $ sTestProperty "punionUR" _punionUR

_punionUR :: MOPSmallint -> MOPSmallint -> Bool
_punionUR a@(MOPSI (x, y)) b@(MOPSI (w, z)) =
    punionUR (toSURange a) (toSURange b) == result
    where result = fromJust $ toURange (min x w, liftN2 NLast unNLast max y z)

_'punionBR = setSCDepth 7 $ sTestProperty "punionBR" _punionBR

_punionBR :: OPBSmallint -> OPBSmallint -> Bool
_punionBR a@(OPBSI (x, y)) b@(OPBSI (w, z)) =
    punionBR (toSBRange a) (toSBRange b) == result
    where result = fromJust $ toBRange (min x w, max y z)

_'toURange' = setSCDepth 15 $ sTestProperty "toURange'" _toURange'

_toURange' :: (Smallint, Smallint) -> Bool
_toURange' a@(x, y) = toURange' a == toURange (Just x, Just y)

_'uRangeEQ = setSCDepth 30 $ sTestProperty "uRangeEQ" _uRangeEQ

_uRangeEQ :: Smallint -> Bool
_uRangeEQ x = uRangeEQ x == toURange (Just x, Just x)

_'bRangeEQ = setSCDepth 30 $ sTestProperty "bRangeEQ" _bRangeEQ

_bRangeEQ :: Smallint -> Bool
_bRangeEQ x = bRangeEQ x == toBRange (x, x)

_'uRangeLE = setSCDepth 30 $ sTestProperty "uRangeLE" _uRangeLE

_uRangeLE :: Smallint -> Bool
_uRangeLE x = uRangeLE x == toURange (Nothing, Just x)

_'bRangeLE = setSCDepth 30 $ sTestProperty "bRangeLE" _bRangeLE

_bRangeLE :: Smallint -> Bool
_bRangeLE x = bRangeLE x == toBRange (minBound, x)

_'uRangeGE = setSCDepth 30 $ sTestProperty "uRangeGE" _uRangeGE

_uRangeGE :: Smallint -> Bool
_uRangeGE x = uRangeGE x == toURange (Just x, Nothing)

_'bRangeGE = setSCDepth 30 $ sTestProperty "bRangeGE" _bRangeGE

_bRangeGE :: Smallint -> Bool
_bRangeGE x = bRangeGE x == toBRange (x, maxBound)

