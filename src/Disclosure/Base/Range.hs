{-|
Module      : Disclosure.Base.Range
Description : Datatypes for unbounded and bounded ranges
Copyright   : (c) 2016-2017 Jeffrey Tsang
License     : All rights reserved
Maintainer  : jeffrey.tsang@ieee.org
Portability : portable

Defines types for unbounded and bounded ranges, which are closed intervals over
ordered types, represented by a 2-tuple. For unbounded ranges, interval bounds
are wrapped in 'Maybe', with 'Nothing' representing the appropriate infinity for
half-open or open ranges. For bounded ranges, 'minBound', 'maxBound' are treated
as infinities.
-}
module Disclosure.Base.Range (
-- * Datatypes and constructors
  URange
, BRange
, toURange
, toBRange
, unURange
, unBRange
-- ** Convenience constructors
, toURange'
, uRangeEQ
, bRangeEQ
, uRangeLE
, bRangeLE
, uRangeGE
, bRangeGE
-- * Set algebra operations
, compareMeet'
, punionUR
, punionBR
) where

import Data.Ord
import Data.Tuple
import Control.Monad
import Disclosure.Base.Util
import Disclosure.Base.Range.Internal

-- | Linear extension of the subset inclusion ordering. Compares upper bounds
-- under normal ≤ ordering, lexicographically lower bounds under reversed ≥
-- ordering.
instance Ord a => Ord (URange a) where
    {-# INLINABLE compare #-}
    compare = comparing (fmap Down . swap . fmap NLast . unURange)

-- | Linear extension of the subset inclusion ordering. Compares upper bounds
-- under normal ≤ ordering, lexicographically lower bounds under reversed ≥
-- ordering.
instance Ord a => Ord (BRange a) where
    {-# INLINABLE compare #-}
    compare = comparing (fmap Down . swap . unBRange)

-- | Constructs and validates a 'URange' for [@x@, @y@]
toURange' :: Ord a => (a, a) -> Maybe (URange a)
{-# INLINABLE toURange' #-}
toURange' = uncurry $ liftN2 Just id $ curry toURange

-- | Constructs a 'URange' for [@x@, @x@]
uRangeEQ :: Ord a => a -> Maybe (URange a)
{-# INLINABLE uRangeEQ #-}
uRangeEQ = (join $ curry toURange) . Just

-- | Constructs and validates a 'BRange' for [@x@, @x@]
bRangeEQ :: (Bounded a, Ord a) => a -> Maybe (BRange a)
{-# INLINABLE bRangeEQ #-}
bRangeEQ = join $ curry toBRange

-- | Constructs a 'URange' for (-∞, @x@]
uRangeLE :: Ord a => a -> Maybe (URange a)
{-# INLINABLE uRangeLE #-}
uRangeLE = curry toURange Nothing . Just

-- | Constructs and validates a 'BRange' for [@minBound@, @x@]
bRangeLE :: (Bounded a, Ord a) => a -> Maybe (BRange a)
{-# INLINABLE bRangeLE #-}
bRangeLE = curry toBRange minBound

-- | Constructs a 'URange' for [@x@, ∞)
uRangeGE :: Ord a => a -> Maybe (URange a)
{-# INLINABLE uRangeGE #-}
uRangeGE = flip (curry toURange) Nothing . Just

-- | Constructs and validates a 'BRange' for [@x@, @maxBound@]
bRangeGE :: (Bounded a, Ord a) => a -> Maybe (BRange a)
{-# INLINABLE bRangeGE #-}
bRangeGE = flip (curry toBRange) maxBound
