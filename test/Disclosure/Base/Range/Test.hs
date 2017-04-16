module Disclosure.Base.Range.Test where

import Data.Maybe
import Disclosure.Test.Util
import Disclosure.Base.Util
import Disclosure.Base.Range.Internal
import Disclosure.Base.Range

tests :: TestTree
tests = testGroup "Disclosure.Base.Range"
    [ testMSIP "toURange" _toURange
    , testSIP "toBRange" _toBRange
    , testGroup "Show URange"
        [ testMOPSI "show" _showURange ]
    , testGroup "Show BRange"
        [ testOPBSI "show" _showBRange ]
    , testGroup "Ord URange"
        [ testMOPSI2 "compare" _ordURange ]
    , testGroup "Ord BRange"
        [ testOPBSI2 "compare" _ordBRange ]
    , testGroup "Monoid' URange"
        [ testMOPSI "mempty" _mempty'URange
        , testMOPSI2 "mappend'" _mappend'URange ]
    , testGroup "Monoid' BRange"
        [ testOPBSI "mempty" _mempty'BRange
        , testOPBSI2 "mappend'" _mappend'BRange ]
    , testMOPSI2 "punionUR" _punionUR
    , testOPBSI2 "punionBR" _punionBR
    , testSIP "toURange'" _toURange'
    , testSI "urangeEQ" _uRangeEQ
    , testSI "brangeEQ" _bRangeEQ
    , testSI "urangeLE" _uRangeLE
    , testSI "brangeLE" _bRangeLE
    , testSI "urangeGE" _uRangeGE
    , testSI "brangeGE" _bRangeGE ]

_toURange :: (Maybe Smallint, Maybe Smallint) -> Bool
_toURange z@(x, y)
    | compareMaybeR x y == GT = failure
    | otherwise = success
    where r = toURange z
          failure = r == Nothing
          success = r == Just (URange z)

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

_showURange :: MOPSmallint -> Bool
_showURange x@(MOPSI (Nothing, Nothing)) = show (toSURange x) == "?"
_showURange x@(MOPSI (Nothing, Just y)) = show (toSURange x) == show y ++ "-"
_showURange x@(MOPSI (Just y, Nothing)) = show (toSURange x) == show y ++ "+"
_showURange x@(MOPSI (Just y, Just z))
    | y == z = show (toSURange x) == show y
    | otherwise = show (toSURange x) == show y ++ "–" ++ show z

toSBRange :: OPBSmallint -> BRange BSmallint
toSBRange (OPBSI z) = fromJust $ toBRange z

_showBRange :: OPBSmallint -> Bool
_showBRange x@(OPBSI (y, z))
    | y == minBound && z == maxBound = check "?"
    | y == z = check $ show y
    | y == minBound = check $ show z ++ "-"
    | z == maxBound = check $ show y ++ "+"
    | otherwise = check $ show y ++ "–" ++ show z
    where check = (show (toSBRange x) ==)

_ordURange :: MOPSmallint -> MOPSmallint -> Bool
_ordURange a@(MOPSI (x, y)) b@(MOPSI (w, z))
    | compareNL y z == LT = check LT
    | compareNL y z == GT = check GT
    | x < w = check GT
    | x > w = check LT
    | otherwise = check EQ
    where compareNL = liftN2 NLast id compare
          check = (compare (toSURange a) (toSURange b) ==)

_ordBRange :: OPBSmallint -> OPBSmallint -> Bool
_ordBRange a@(OPBSI (x, y)) b@(OPBSI (w, z))
    | y < z = check LT
    | y > z = check GT
    | x < w = check GT
    | x > w = check LT
    | otherwise = check EQ
    where check = (compare (toSBRange a) (toSBRange b) ==)

_mempty'URange :: MOPSmallint -> Bool
_mempty'URange x = maybe False (== y) (mappend' y mempty') &&
                   maybe False (== y) (mappend' mempty' y)
                   where y = toSURange x

_mappend'URange :: MOPSmallint -> MOPSmallint -> Bool
_mappend'URange a@(MOPSI (x, y)) b@(MOPSI (w, z))
    | compareMaybeR maxl minh == GT = failure
    | otherwise = check $ toURange (maxl, minh)
    where maxl = max x w
          minh = liftN2 NLast unNLast min y z
          a' = toSURange a
          b' = toSURange b
          failure = isNothing $ mappend' a' b'
          check = (mappend' a' b' ==)

_mempty'BRange :: OPBSmallint -> Bool
_mempty'BRange x = maybe False (== y) (mappend' y mempty') &&
                   maybe False (== y) (mappend' mempty' y)
                   where y = toSBRange x

_mappend'BRange :: OPBSmallint -> OPBSmallint -> Bool
_mappend'BRange a@(OPBSI (x, y)) b@(OPBSI (w, z))
    | maxl > minh = failure
    | otherwise = check $ toBRange (maxl, minh)
    where maxl = max x w
          minh = min y z
          a' = toSBRange a
          b' = toSBRange b
          failure = isNothing $ mappend' a' b'
          check = (mappend' a' b' ==)

_punionUR :: MOPSmallint -> MOPSmallint -> Bool
_punionUR a@(MOPSI (x, y)) b@(MOPSI (w, z)) =
    maybe False (== punionUR (toSURange a) (toSURange b)) result
    where result = toURange (min x w, liftN2 NLast unNLast max y z)

_punionBR :: OPBSmallint -> OPBSmallint -> Bool
_punionBR a@(OPBSI (x, y)) b@(OPBSI (w, z)) =
    maybe False (== punionBR (toSBRange a) (toSBRange b)) result
    where result = toBRange (min x w, max y z)

_toURange' :: (Smallint, Smallint) -> Bool
_toURange' a@(x, y) = toURange' a == toURange (Just x, Just y)

_uRangeEQ :: Smallint -> Bool
_uRangeEQ x = uRangeEQ x == toURange (Just x, Just x)

_bRangeEQ :: Smallint -> Bool
_bRangeEQ x = bRangeEQ x == toBRange (x, x)

_uRangeLE :: Smallint -> Bool
_uRangeLE x = uRangeLE x == toURange (Nothing, Just x)

_bRangeLE :: Smallint -> Bool
_bRangeLE x = bRangeLE x == toBRange (minBound, x)

_uRangeGE :: Smallint -> Bool
_uRangeGE x = uRangeGE x == toURange (Just x, Nothing)

_bRangeGE :: Smallint -> Bool
_bRangeGE x = bRangeGE x == toBRange (x, maxBound)
