{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
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
  , heightBagSize
  , heightBagAdd
  , heightBagRemove
  , heightBagFromParents
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

data HeightBag = HeightBag
  { _heightBag_size :: {-# UNPACK #-} !Int
  , _heightBag_contents :: !(IntMap Word) -- Number of excess in each bucket
  }
  deriving (Show, Read, Eq, Ord)

{-# INLINE heightBagEmpty #-}
heightBagEmpty :: HeightBag
heightBagEmpty = heightBagVerify $ HeightBag 0 IntMap.empty

{-# INLINE heightBagSize #-}
heightBagSize :: HeightBag -> Int
heightBagSize = _heightBag_size

{-# INLINE heightBagAdd #-}
heightBagAdd :: Height -> HeightBag -> HeightBag
heightBagAdd (Height h) (HeightBag s c) = heightBagVerify $ HeightBag (succ s) $
  IntMap.insertWith (+) h 1 c

{-# INLINE heightBagRemove #-}
heightBagRemove :: Height -> HeightBag -> HeightBag
heightBagRemove (Height h) b@(HeightBag s c) = heightBagVerify $ case IntMap.lookup h c of
  Nothing -> error $ "heightBagRemove: Height " <> show h <> " not present in bag " <> show b
  Just old -> HeightBag (pred s) $ case old of
    1 -> IntMap.delete h c
    _ -> IntMap.insert h (pred old) c

{-# INLINE heightBagVerify #-}
heightBagVerify :: HeightBag -> HeightBag
#ifdef DEBUG
heightBagVerify b@(HeightBag s c) = if
  | s /= fromIntegral (sum (IntMap.elems c))
    -> error $ "heightBagVerify: size doesn't match: " <> show b
  | unsafeHeightAsInt invalidHeight `IntMap.member` c
    -> error $ "heightBagVerify: contains invalid height: " <> show b
  | otherwise -> b
#else
heightBagVerify = id
#endif

{-# INLINE heightBagFromParents #-}
heightBagFromParents :: [Height] -> (HeightBag, Height)
heightBagFromParents heights =
  let step (!b, !inv) = \case
        h@ValidHeight -> (heightBagAdd h b, inv)
        InvalidHeight     -> (b, True)
      (bag, anyInvalid) = foldl' step (heightBagEmpty, False) heights
      nextHeight = if anyInvalid then invalidHeight else heightBagSuccHeight bag
  in (bag, nextHeight)

heightBagSuccHeight :: HeightBag -> Height
heightBagSuccHeight (HeightBag _ c) = case IntMap.maxViewWithKey c of
  Just ((h, _), _) -> Height (succ h)
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
