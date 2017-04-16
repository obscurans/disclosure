module Disclosure.Base.CompoundRange.Test where

import Data.Maybe
import Data.List
import Control.Monad
import Disclosure.Test.Util
import Disclosure.Base.Util
import Disclosure.Base.Range
import Disclosure.Base.Range.Internal
import Disclosure.Base.CompoundRange
import Disclosure.Base.CompoundRange.Internal

tests :: TestTree
tests = testGroup "Disclosure.Base.CompoundRange"
    [ testMOPSIL "toCURange" _toCURange
    , testOPBSIL "toCBRange" _toCBRange
    , testGroup "Show CURange"
        [ testOLMPSI "show" _showCURange ]
    , testGroup "Show CBRange"
        [ testOLPBSI "show" _showCBRange ]
    , testGroup "Monoid CURange"
        [ testOLMPSI "mempty" _memptyCURange
        , testOLMPSI2 "mappend" _mappendCURange ]
    , testGroup "Monoid CBRange"
        [ testOLPBSI "mempty" _memptyCBRange
        , testOLPBSI2 "mappend" _mappendCBRange ]
    , testMOPSI "liftURange" _liftURange
    , testOPBSI "liftBRange" _liftBRange
    , testMOPSI2 "unionURange" _unionURange
    , testOPBSI2 "unionBRange" _unionBRange
    , testOLMPSI2 "unionCUR" _unionCUR
    , testOLPBSI2 "unionCBR" _unionCBR
    , testOLMPSI "punionCUR" _punionCUR
    , testOLPBSI "punionCBR" _punionCBR
    , testGroup "collapseCUR"
        [ testOLMPSI "null" _collapseCUR_null
        , testOLMPSI "universal" _collapseCUR_universal
        , testOLMPSI "adjacent" _collapseCUR_adjacent ]
    , testGroup "collapseCBR"
        [ testOLPBSI "null" _collapseCBR_null
        , testOLPBSI "universal" _collapseCBR_universal
        , testOLPBSI "adjacent" _collapseCBR_adjacent ]
    , testGroup "transformCUR"
        [ testOLMPSI "id" _transformCUR_id
        , testOLMPSI "const" $ _transformCUR_f (const (1::Double))
        , testOLMPSI "mul2" $ _transformCUR_f $ (*2) . unSI
        , testOLMPSI "quot2" $ _transformCUR_f (`quot` 2) ]
    , testGroup "transformCBR"
        [ testOLPBSI "id" _transformCBR_id
        , testOLPBSI "const" $ _transformCBR_f (const (1::Int))
        , testOLPBSI "mul2" $ _transformCBR_f $ (*2) . unBSI
        , testOLPBSI "quot2" $ _transformCBR_f (`quot` 2) ]
    ]

primaFacieCUR :: Ord a => CURange a -> Bool
primaFacieCUR (CURange x) = primaFacieCUR' x
    where primaFacieCUR' [] = True
          primaFacieCUR' [x] = True
          primaFacieCUR' (URange (_,x2):yr@(URange (y1,_):_))
            | isNothing x2 = False
            | isNothing y1 = False
            | x2 >= y1 = False
            | otherwise = primaFacieCUR' yr

primaFacieCBR :: Ord a => CBRange a -> Bool
primaFacieCBR (CBRange x) = primaFacieCBR' x
    where primaFacieCBR' [] = True
          primaFacieCBR' [x] = True
          primaFacieCBR' (BRange (_,x2):yr@(BRange (y1,_):_))
            | x2 >= y1 = False
            | otherwise = primaFacieCBR' yr

-- Brute force "linear search every range of y in x"
injectCUR :: Ord a => CURange a -> [URange a] -> Bool
injectCUR (CURange x) = injectCUR' x x
    where injectCUR' _ _ [] = True
          injectCUR' _ [] y = False
          injectCUR' z (x:xs) yt@(y:ys)
            | x == y = injectCUR' z z ys
            | intersect == Just y = injectCUR' z z ys
            | intersect == Nothing = injectCUR' z xs yt
            | otherwise = False
            where intersect = mappend' x y

-- Brute force "linear search every range of y in x"
injectCBR :: (Bounded a, Ord a) => CBRange a -> [BRange a] -> Bool
injectCBR (CBRange x) = injectCBR' x x
    where injectCBR' _ _ [] = True
          injectCBR' _ [] y = False
          injectCBR' z (x:xs) yt@(y:ys)
            | x == y = injectCBR' z z ys
            | intersect == Just y = injectCBR' z z ys
            | intersect == Nothing = injectCBR' z xs yt
            | otherwise = False
            where intersect = mappend' x y

-- Brute force "linear search every range of x in y"
surjectCUR :: (Ord a, Enum a) => CURange a -> [URange a] -> Bool
surjectCUR (CURange x) y = surjectCUR' y x y
    where surjectCUR' _ [] _ = True
          surjectCUR' _ x [] = False
          surjectCUR' z xt@(x@(URange (x1, x2)):xs) (y:ys)
            | x == y = surjectCUR' z xs z
            | intersect == Just x = surjectCUR' z xs z
            | intersect == Nothing = surjectCUR' z xt ys
            | Just (URange (i1, i2)) <- intersect =
            let si2 = liftM succ i2
                pi1 = liftM pred i1 in
                if i1 == x1 then surjectCUR' z (URange (si2, x2) : xs) ys
                else if i2 == x2 then surjectCUR' z (URange (x1, pi1) : xs) ys
                else surjectCUR' z (URange (x1, pi1) : URange (si2, x2) : xs) ys
            where intersect = mappend' x y

-- Brute force "linear search every range of x in y"
surjectCBR :: (Bounded a, Ord a, Enum a) => CBRange a -> [BRange a] -> Bool
surjectCBR (CBRange x) y = surjectCBR' y x y
    where surjectCBR' _ [] _ = True
          surjectCBR' _ x [] = False
          surjectCBR' z xt@(x@(BRange (x1, x2)):xs) (y:ys)
            | x == y = surjectCBR' z xs z
            | intersect == Just x = surjectCBR' z xs z
            | intersect == Nothing = surjectCBR' z xt ys
            | Just (BRange (i1, i2)) <- intersect =
            let si2 = succ i2
                pi1 = pred i1 in
                if i1 == x1 then surjectCBR' z (BRange (si2, x2) : xs) ys
                else if i2 == x2 then surjectCBR' z (BRange (x1, pi1) : xs) ys
                else surjectCBR' z (BRange (x1, pi1) : BRange (si2, x2) : xs) ys
            where intersect = mappend' x y

equivalentCUR :: (Ord a, Enum a) => CURange a -> [URange a] -> Bool
equivalentCUR x y = injectCUR x y && surjectCUR x y

equivalentCBR :: (Bounded a, Ord a, Enum a) => CBRange a -> [BRange a] -> Bool
equivalentCBR x y = injectCBR x y && surjectCBR x y

equalCUR :: CURange Smallint -> [MOPSmallint] -> Bool
equalCUR x = (==) $ map (MOPSI . unURange) $ unCURange x

equalCBR :: CBRange BSmallint -> [OPBSmallint] -> Bool
equalCBR x = (==) $ map (OPBSI . unBRange) $ unCBRange x

toRawCUR :: [(Maybe Smallint, Maybe Smallint)] -> [URange Smallint]
toRawCUR = catMaybes . map toURange

toRawCBR :: [(BSmallint, BSmallint)] -> [BRange BSmallint]
toRawCBR = catMaybes . map toBRange

_toCURange :: [MOPSmallint] -> Bool
_toCURange x = primaFacieCUR r && equivalentCUR r c
    where c = toRawCUR $ map unMOPSI x
          r = toCURange c

_toCBRange :: [OPBSmallint] -> Bool
_toCBRange x = primaFacieCBR r && equivalentCBR r c
    where c = toRawCBR $ map unOPBSI x
          r = toCBRange c

_showCURange :: OLMPSmallint -> Bool
_showCURange (OLMPSI x) = _showCURange' $ toCURange $ toRawCUR x
    where _showCURange' y@(CURange r)
            | [] <- r = show y == "Null"
            | [x] <- r = show y == show x
            | otherwise = show y ==
                                "(" ++ (intercalate " or " (fmap show r)) ++ ")"

_showCBRange :: OLPBSmallint -> Bool
_showCBRange (OLPBSI x) = _showCBRange' $ toCBRange $ toRawCBR x
    where _showCBRange' y@(CBRange r)
            | [] <- r = show y == "Null"
            | [x] <- r = show y == show x
            | otherwise = show y ==
                                "(" ++ (intercalate " or " (fmap show r)) ++ ")"

_memptyCURange :: OLMPSmallint -> Bool
_memptyCURange (OLMPSI x) = mappend mempty r == r && mappend r mempty == r
    where r = toCURange $ toRawCUR x

_mappendCURange :: OLMPSmallint -> OLMPSmallint -> Bool
_mappendCURange (OLMPSI x) (OLMPSI y) = primaFacieCUR r &&
                                        surjectCUR r xr && surjectCUR r yr
    where xr = toRawCUR x
          yr = toRawCUR y
          r = mappend (toCURange xr) (toCURange yr)

_memptyCBRange :: OLPBSmallint -> Bool
_memptyCBRange (OLPBSI x) = mappend mempty r == r && mappend r mempty == r
    where r = toCBRange $ toRawCBR x

_mappendCBRange :: OLPBSmallint -> OLPBSmallint -> Bool
_mappendCBRange (OLPBSI x) (OLPBSI y) = primaFacieCBR r &&
                                        surjectCBR r xr && surjectCBR r yr
    where xr = toRawCBR x
          yr = toRawCBR y
          r = mappend (toCBRange xr) (toCBRange yr)

_liftURange :: MOPSmallint -> Bool
_liftURange (MOPSI x) = maybe True (\y -> unCURange (liftURange y) == [y]) r
    where r = toURange x

_liftBRange :: OPBSmallint -> Bool
_liftBRange (OPBSI x) = maybe True (\y -> unCBRange (liftBRange y) == [y]) r
    where r = toBRange x

_unionURange :: MOPSmallint -> MOPSmallint -> Bool
_unionURange (MOPSI x) (MOPSI y) = fromMaybe True $ liftM2 f xr yr
    where xr = toURange x
          yr = toURange y
          f = \x y -> let r = unionURange x y in
                        primaFacieCUR r && equivalentCUR r [x, y]

_unionBRange :: OPBSmallint -> OPBSmallint -> Bool
_unionBRange (OPBSI x) (OPBSI y) = fromMaybe True $ liftM2 f xr yr
    where xr = toBRange x
          yr = toBRange y
          f = \x y -> let r = unionBRange x y in
                        primaFacieCBR r && equivalentCBR r [x, y]

_unionCUR :: OLMPSmallint -> OLMPSmallint -> Bool
_unionCUR (OLMPSI x) (OLMPSI y) = primaFacieCUR r && equivalentCUR r xy
    where xr = toCURange $ toRawCUR x
          yr = toCURange $ toRawCUR y
          r = unionCUR xr yr
          xy = unCURange xr ++ unCURange yr

_unionCBR :: OLPBSmallint -> OLPBSmallint -> Bool
_unionCBR (OLPBSI x) (OLPBSI y) = primaFacieCBR r && equivalentCBR r xy
    where xr = toCBRange $ toRawCBR x
          yr = toCBRange $ toRawCBR y
          r = unionCBR xr yr
          xy = unCBRange xr ++ unCBRange yr

_punionCUR :: OLMPSmallint -> Bool
_punionCUR (OLMPSI x) = injectCUR r y
    where y = toRawCUR x
          u = punionCUR $ toCURange y
          r = maybe (toCURange []) liftURange u

_punionCBR :: OLPBSmallint -> Bool
_punionCBR (OLPBSI x) = injectCBR r y
    where y = toRawCBR x
          u = punionCBR $ toCBRange y
          r = maybe (toCBRange []) liftBRange u

_collapseCUR_null :: OLMPSmallint -> Bool
_collapseCUR_null (OLMPSI x) = primaFacieCUR r && r == y
    where y = toCURange $ toRawCUR x
          r = collapseCUR (const (const False)) y

_collapseCUR_universal :: OLMPSmallint -> Bool
_collapseCUR_universal (OLMPSI x) = primaFacieCUR r && r == p
    where y = toCURange $ toRawCUR x
          r = collapseCUR (const (const True)) y
          p = maybe (toCURange []) liftURange $ punionCUR y

_collapseCUR_adjacent :: OLMPSmallint -> Bool
_collapseCUR_adjacent (OLMPSI x) = primaFacieCUR r && equivalentCUR r (p ++ y)
    where y = toRawCUR x
          r = collapseCUR (\x y -> succ x == y) $ toCURange y
          f = \x y -> let (_, x2) = unURange x
                          (y1, _) = unURange y
                          f2 = \x y -> succ x == y in
                            if fromMaybe False (liftM2 f2 x2 y1)
                            then Just (x2, y1) else Nothing
          p = catMaybes $ map (>>= toURange) $ pure f <*> y <*> y

_collapseCBR_null :: OLPBSmallint -> Bool
_collapseCBR_null (OLPBSI x) = primaFacieCBR r && r == y
    where y = toCBRange $ toRawCBR x
          r = collapseCBR (const (const False)) y

_collapseCBR_universal :: OLPBSmallint -> Bool
_collapseCBR_universal (OLPBSI x) = primaFacieCBR r && r == p
    where y = toCBRange $ toRawCBR x
          r = collapseCBR (const (const True)) y
          p = maybe (toCBRange []) liftBRange $ punionCBR y

_collapseCBR_adjacent :: OLPBSmallint -> Bool
_collapseCBR_adjacent (OLPBSI x) = primaFacieCBR r && equivalentCBR r (p ++ y)
    where y = toRawCBR x
          r = collapseCBR (\x y -> succ x == y) $ toCBRange y
          f = \x y -> let (_, x2) = unBRange x
                          (y1, _) = unBRange y in
                            if succ x2 == y1 then Just (x2, y1) else Nothing
          p = catMaybes $ map (>>= toBRange) $ pure f <*> y <*> y

_transformCUR_id :: OLMPSmallint -> Bool
_transformCUR_id (OLMPSI x) = primaFacieCUR r && r == y
    where y = toCURange $ toRawCUR x
          r = transformCUR id y

_transformCUR_f :: (Ord a, Enum a) => (Smallint -> a) -> OLMPSmallint -> Bool
_transformCUR_f f (OLMPSI x) = primaFacieCUR r && equivalentCUR r p
    where y = toRawCUR x
          r = transformCUR f $ toCURange y
          p = catMaybes $ map (toURange . (\(x, y) -> (fmap f x, fmap f y)) .
                                unURange) y

_transformCBR_id :: OLPBSmallint -> Bool
_transformCBR_id (OLPBSI x) = primaFacieCBR r && r == y
    where y = toCBRange $ toRawCBR x
          r = transformCBR id y

_transformCBR_f :: (Bounded a, Ord a, Enum a) => (BSmallint -> a)
                                              -> OLPBSmallint -> Bool
_transformCBR_f f (OLPBSI x) = primaFacieCBR r && equivalentCBR r p
    where y = toRawCBR x
          r = transformCBR f $ toCBRange y
          p = catMaybes $ map (toBRange . (\(x, y) -> (f x, f y)) . unBRange) y

_transformCBR_const :: OLPBSmallint -> Bool
_transformCBR_const (OLPBSI x) = primaFacieCBR r && equivalentCBR r p
    where y = toRawCBR x
          r = transformCBR (const (1::Int)) $ toCBRange y
          p = catMaybes $ map (toBRange . const (1, 1)) y
