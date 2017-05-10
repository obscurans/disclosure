module Disclosure.Constraint.Shape.Test where

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Data.Tuple.Sequence (sequenceT) --tuple
import Data.Tuple.Homogenous (Tuple4(..)) --tuples-homogenous-h98
import Disclosure.Test.Util
import Disclosure.Base.Util
import Disclosure.Base.Range
import Disclosure.Base.Range.Internal (BRange(..))
import Disclosure.Constraint.Shape
import Disclosure.Constraint.Shape.Internal

tests :: TestTree
tests = testGroup "Disclosure.Constraint.Shape"
    [ testSI "toSuit" _toSuit
    , testGroup "Show Suit"
        [ testBSI "show" _showSuit ]
    , testSI2 "toSuitRange" _toSuitRange
    , testSI "suitEQ" _suitEQ
    , testSI "suitLE" _suitLE
    , testSI "suitGE" _suitGE
    , testOPBSI4 "toShape" _toShape
    , testOPBSI4 "minShape" _minShape
    , testGroup "Monoid' Shape"
        [ testOPBSI4 "mempty'" _mempty'Shape
        , testOPBSI8 "mappend'" _mappend'Shape ]
    , testOPBSI "shapeS" _shapeS
    , testOPBSI "shapeH" _shapeH
    , testOPBSI "shapeD" _shapeD
    , testOPBSI "shapeC" _shapeC
    , testOPBSI8 "punionShape" _punionShape
    ]

allShapes :: [Tuple4 Int]
allShapes = [ Tuple4 (s, h, d, c) | s <- [0 .. 13], h <- [0 .. 13 - s]
                                  , d <- [0 .. 13 - s - h]
                                  , let c = 13 - s - h - d, c >= 0 ]

inShape :: Tuple4 Int -> Shape -> Bool
inShape = (_' $ __' unShape) inShape'

inShape' :: Tuple4 Int -> Tuple4 SuitRange -> Bool
inShape' = _' and . liftA2 inRange
    where inRange y (BRange (Suit l, Suit h)) = l <= y && y <= h

toRawRange :: OPBSmallint -> Maybe SuitRange
toRawRange (OPBSI (BSI l, BSI h)) = toSuitRange l h

toRawShape :: OPBSmallint -> OPBSmallint -> OPBSmallint -> OPBSmallint
           -> Maybe (Tuple4 SuitRange)
toRawShape = _''' (fmap Tuple4 . sequenceT
                  . untuple4 . fmap toRawRange . Tuple4) . (,,,)

_toSuit :: Smallint -> Bool
_toSuit (SI x)
    | x < 0 = failure
    | x > 13 = failure
    | otherwise = success
    where r = toSuit x
          failure = r == Nothing
          success = r == Just (Suit x)

_showSuit :: BSmallint -> Bool
_showSuit (BSI x)
    | x == 0 = show (Suit x) == "-"
    | otherwise = show (Suit x) == show x

_toSuitRange :: Smallint -> Smallint -> Bool
_toSuitRange (SI x) (SI y) = toSuitRange x y == toBRange (Suit x, Suit y)

_suitEQ :: Smallint -> Bool
_suitEQ (SI x) = suitEQ x == bRangeEQ (Suit x)

_suitLE :: Smallint -> Bool
_suitLE (SI x) = suitLE x == bRangeLE (Suit x)

_suitGE :: Smallint -> Bool
_suitGE (SI x) = suitGE x == bRangeGE (Suit x)

_toShape :: OPBSmallint -> OPBSmallint -> OPBSmallint -> OPBSmallint -> Bool
_toShape = _''' (maybe True toShape') . toRawShape
    where toShape' x = and $ map (\s -> maybe False (inShape s) (toShape x) ==
                                        inShape' s x) allShapes
            where y = fmap unShape $ toShape x

_minShape :: OPBSmallint -> OPBSmallint -> OPBSmallint -> OPBSmallint -> Bool
_minShape = _''' (maybe True minShape') . toRawShape
    where minShape' = maybe True (\x -> and $ map
                                (\s -> inShape' s (minShape x) == inShape s x)
                                allShapes) . toShape

_mempty'Shape :: OPBSmallint -> OPBSmallint -> OPBSmallint -> OPBSmallint
              -> Bool
_mempty'Shape = _''' (maybe True mempty'Shape') . toRawShape
    where mempty'Shape' = maybe True (\x -> and $ map
                            (\s -> let a = inShape s x in
                            a == maybe False (inShape s) (mappend' mempty' x) &&
                            a == maybe False (inShape s) (mappend' x mempty'))
                            allShapes) . toShape

_mappend'Shape :: OPBSmallint -> OPBSmallint -> OPBSmallint -> OPBSmallint
               -> OPBSmallint -> OPBSmallint -> OPBSmallint -> OPBSmallint
               -> Bool
_mappend'Shape a b c d e f g h = fromMaybe True $ liftA2 mappend'Shape'
            (toRawShape a b c d >>= toShape) (toRawShape e f g h >>= toShape)
    where mappend'Shape' x y = and $ map (\s -> (inShape s x && inShape s y) ==
                                          maybe False (inShape s) r) allShapes
            where r = mappend' x y

_shapeS :: OPBSmallint -> Bool
_shapeS = maybe True shapeS' . toRawRange
    where shapeS' x = shapeS x == (toShape $ Tuple4 (x, e, e, e))
          e = mempty'

_shapeH :: OPBSmallint -> Bool
_shapeH = maybe True shapeH' . toRawRange
    where shapeH' x = shapeH x == (toShape $ Tuple4 (e, x, e, e))
          e = mempty'

_shapeD :: OPBSmallint -> Bool
_shapeD = maybe True shapeD' . toRawRange
    where shapeD' x = shapeD x == (toShape $ Tuple4 (e, e, x, e))
          e = mempty'

_shapeC :: OPBSmallint -> Bool
_shapeC = maybe True shapeC' . toRawRange
    where shapeC' x = shapeC x == (toShape $ Tuple4 (e, e, e, x))
          e = mempty'

_punionShape :: OPBSmallint -> OPBSmallint -> OPBSmallint -> OPBSmallint
             -> OPBSmallint -> OPBSmallint -> OPBSmallint -> OPBSmallint -> Bool
_punionShape a b c d e f g h = fromMaybe True $ liftA2 punionShape'
            (toRawShape a b c d >>= toShape) (toRawShape e f g h >>= toShape)
    where punionShape' x y = and $ map (\s -> (not (inShape s x || inShape s y)
                                               || (inShape s r))) allShapes
            where r = punionShape x y         -- ^ (in_x or in_y) => in_r
