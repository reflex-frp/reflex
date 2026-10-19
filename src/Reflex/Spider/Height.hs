{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module Reflex.Spider.Height
  (Height
  , pattern ZeroHeight
  , pattern InvalidHeight
#if defined(DEBUG_CYCLES)
  , invalidHeightBeingTraversed
  , isUnmarkedInvalidHeight
#endif
  , pattern ValidHeight
  , zeroHeight
  , invalidHeight
  , maxHeight
  , raiseHeight
  , coincidenceHeight
  , compareHeight
  , heightSlotPassed
  , HeightBag
  , heightBagEmpty
  , heightBagHeight
  , heightBagParentCount
  , heightBagAddParent
  , heightBagRemoveParent
  , heightBagInvalidateParent
  , heightBagRecalculateParent
  , heightBagSuccHeight
  , HeightQueue
  , heightQueueEmpty
  , heightQueueInsert
  , heightQueueNext
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif

--------------------------------------------------------------------------------
-- Height data
--------------------------------------------------------------------------------

newtype Height = Height Int
  deriving (Show)

{-# INLINE unsafeHeightAsInt #-}
unsafeHeightAsInt :: Height -> Int
unsafeHeightAsInt (Height h) = h

pattern ZeroHeight :: Height
pattern ZeroHeight = Height 0

pattern InvalidHeight :: Height
pattern InvalidHeight <- (isInvalidHeight -> True)
  where InvalidHeight = Height (-1000)

{-# INLINE isInvalidHeight #-}
isInvalidHeight :: Height -> Bool
#ifdef DEBUG_CYCLES
isInvalidHeight (Height h) = h == (-1000) || h == (-1001)

isUnmarkedInvalidHeight :: Height -> Bool
isUnmarkedInvalidHeight (Height h) = h == (-1001)
#else
isInvalidHeight (Height h) = h == (-1000)
#endif

pattern ValidHeight :: Height
pattern ValidHeight <- Height ((>= 0) -> True)

pattern UnsafeValidHeight :: Int -> Height
pattern UnsafeValidHeight n <- Height (\ x -> if x >= 0 then Just x else Nothing -> Just n)

{-# INLINE zeroHeight #-}
zeroHeight :: Height
zeroHeight = Height 0

{-# INLINE invalidHeight #-}
invalidHeight :: Height
invalidHeight = Height (-1000)

{-# INLINE maxHeight #-}
maxHeight :: Height
maxHeight = Height maxBound

#ifdef DEBUG_CYCLES

-- | An invalid height that is currently being traversed, e.g. by walkInvalidHeightParents
{-# INLINE invalidHeightBeingTraversed #-}
invalidHeightBeingTraversed :: Height
invalidHeightBeingTraversed = Height (-1001)

#endif

{-# COMPLETE InvalidHeight, ValidHeight #-}
{-# COMPLETE InvalidHeight, UnsafeValidHeight #-}

{-# INLINE coincidenceHeight #-}
coincidenceHeight :: Height -> Height -> Height
coincidenceHeight (UnsafeValidHeight outer) (UnsafeValidHeight inner) = Height (max outer inner)
coincidenceHeight InvalidHeight ValidHeight = invalidHeight
coincidenceHeight ValidHeight InvalidHeight = invalidHeight
coincidenceHeight InvalidHeight InvalidHeight = invalidHeight

{-# INLINE raiseHeight #-}
raiseHeight :: Height -> Height -> Maybe Height
raiseHeight (UnsafeValidHeight from) (UnsafeValidHeight to)
  | to > from = Just (Height to)
  | otherwise = Nothing
raiseHeight from to =
  error $ "raiseHeight was passed special height: " <> show from <> " " <> show to

-- | Safe compare, throw an error if either is special.
{-# INLINE compareHeight #-}
compareHeight :: Height -> Height -> Ordering
compareHeight (UnsafeValidHeight a) (UnsafeValidHeight b) = compare a b
compareHeight a b = error $ "compareHeight: special height(s): " <> show a <> " " <> show b

-- Used in merges
{-# INLINE heightSlotPassed #-}
heightSlotPassed :: Height -> Height -> Bool
heightSlotPassed (UnsafeValidHeight current) (UnsafeValidHeight mine) = current >= mine
heightSlotPassed ValidHeight InvalidHeight = True
heightSlotPassed InvalidHeight _ = error "heightSlotPassed: current height is special"

--------------------------------------------------------------------------------
-- Heightbag
--------------------------------------------------------------------------------

-- | Valid merge parents' heights and the number of parents with invalid
-- heights.
data HeightBag = HeightBag
  { _heightBag_invalidParents :: {-# UNPACK #-} !Int
  , _heightBag_contents :: !(IntMap Word) -- Number of parents in each height bucket
  }
  deriving (Show, Read, Eq, Ord)

{-# INLINE heightBagEmpty #-}
heightBagEmpty :: HeightBag
heightBagEmpty = heightBagVerify $ HeightBag 0 IntMap.empty

{-# INLINE heightBagHeight #-}
heightBagHeight :: HeightBag -> Height
heightBagHeight b@(HeightBag invalidParents _) = case compare invalidParents 0 of
  EQ -> heightBagSuccHeight b
  GT -> invalidHeight
  LT -> error $ "heightBagHeight: more heights than parents: " <> show b

{-# INLINE heightBagParentCount #-}
heightBagParentCount :: HeightBag -> Int
heightBagParentCount (HeightBag invalidParents c) = invalidParents + fromIntegral (sum (IntMap.elems c))

{-# INLINE heightBagAddParent #-}
heightBagAddParent :: Height -> HeightBag -> HeightBag
heightBagAddParent h (HeightBag invalidParents c) = heightBagVerify $ case h of
  ValidHeight -> HeightBag invalidParents (insertHeight h c)
  InvalidHeight -> HeightBag (succ invalidParents) c

{-# INLINE heightBagRemoveParent #-}
heightBagRemoveParent :: Height -> HeightBag -> HeightBag
heightBagRemoveParent h b@(HeightBag invalidParents c) = heightBagVerify $ case h of
  ValidHeight -> HeightBag invalidParents (deleteHeight h b)
  InvalidHeight -> HeightBag (pred invalidParents) c

{-# INLINE heightBagInvalidateParent #-}
heightBagInvalidateParent :: Height -> HeightBag -> HeightBag
heightBagInvalidateParent h b@(HeightBag invalidParents _) = heightBagVerify $
  HeightBag (succ invalidParents) (deleteHeight h b)

{-# INLINE heightBagRecalculateParent #-}
heightBagRecalculateParent :: Height -> HeightBag -> HeightBag
heightBagRecalculateParent h (HeightBag invalidParents c) = heightBagVerify $
  HeightBag (pred invalidParents) (insertHeight h c)

{-# INLINE insertHeight #-}
insertHeight :: Height -> IntMap Word -> IntMap Word
insertHeight (Height h) = IntMap.insertWith (+) h 1

{-# INLINE deleteHeight #-}
deleteHeight :: Height -> HeightBag -> IntMap Word
deleteHeight (Height h) b@(HeightBag _ c) = case IntMap.lookup h c of
  Nothing -> error $ "deleteHeight: Height " <> show h <> " not present in bag " <> show b
  Just old -> case old of
    1 -> IntMap.delete h c
    _ -> IntMap.insert h (pred old) c

{-# INLINE heightBagVerify #-}
heightBagVerify :: HeightBag -> HeightBag
#ifdef DEBUG
heightBagVerify b@(HeightBag invalidParents c) = if
  | invalidParents < 0
    -> error $ "heightBagVerify: more heights than parents: " <> show b
  | unsafeHeightAsInt invalidHeight `IntMap.member` c
    -> error $ "heightBagVerify: contains invalid height: " <> show b
  | otherwise -> b
#else
heightBagVerify = id
#endif

heightBagSuccHeight :: HeightBag -> Height
heightBagSuccHeight (HeightBag _ c) = case IntMap.lookupMax c of
  Just (h, _) -> Height (succ h)
  Nothing -> zeroHeight

--------------------------------------------------------------------------------
-- HeightQueue
--------------------------------------------------------------------------------

newtype HeightQueue a = HeightQueue (IntMap [a])
  deriving (Show, Read, Eq, Ord)

{-# INLINE heightQueueEmpty #-}
heightQueueEmpty :: HeightQueue a
heightQueueEmpty = HeightQueue IntMap.empty

{-# INLINE heightQueueInsert #-}
heightQueueInsert :: Height -> a -> HeightQueue a -> HeightQueue a
heightQueueInsert height a (HeightQueue q) = heightQueueVerifyKey height $ HeightQueue $
  IntMap.insertWith (++) (unsafeHeightAsInt height) [a] q

{-# INLINE heightQueueNext #-}
heightQueueNext :: HeightQueue a -> Maybe (Height, [a], HeightQueue a)
heightQueueNext (HeightQueue q) = case IntMap.minViewWithKey q of
  Just ((h, as), q') -> Just (Height h, as, HeightQueue q')
  Nothing -> Nothing

{-# INLINE heightQueueVerifyKey #-}
heightQueueVerifyKey :: Height -> HeightQueue a -> HeightQueue a
#ifdef DEBUG
heightQueueVerifyKey height q = case height of
  ValidHeight -> q
  _ -> error $ "heightQueueInsert: refusing special height " <> show height
#else
heightQueueVerifyKey _ q = q
#endif
