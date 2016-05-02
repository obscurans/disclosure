module Disclosure.Base.Range.Test where

import Disclosure.Test.Util
import Disclosure.Base.Util
import Disclosure.Base.Range.Internal
--import Disclosure.Base.Range

tests :: TestTree
tests = testGroup "Disclosure.Base.Range"
    [ _'toURange
    , _'toBRange
    ]

_'toURange = adjSCDepth 5 $ sTestProperty "toURange" _toURange

_toURange :: (Maybe Smallint, Maybe Smallint) -> Bool
_toURange z@(x, y)
    | compareMaybeR x y == GT = failure
    | otherwise = success
    where r = toURange z
          failure = r == Nothing
          success = r == Just (URange z)

_'toBRange = adjSCDepth 5 $ sTestProperty "toBRange" _toBRange

_toBRange :: (Smallint, Smallint) -> Bool
_toBRange z@(x, y)
    | x < minBound = failure
    | y > maxBound = failure
    | x > y = failure
    | otherwise = success
    where r = toBRange z
          failure = r == Nothing
          success = r == Just (BRange z)

