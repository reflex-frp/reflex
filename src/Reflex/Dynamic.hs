{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TypeOperators, DeriveDataTypeable, PackageImports, TemplateHaskell, LambdaCase, DataKinds, PolyKinds #-}
module Reflex.Dynamic ( Dynamic -- Abstract so we can preserve the law that the current value is always equal to the most recent update
                      , current
                      , updated
                      , constDyn
                      , holdDyn
                      , nubDyn
                      , count
                      , toggle
                      , switchPromptlyDyn
                      , tagDyn
                      , attachDyn
                      , attachDynWith
                      , attachDynWithMaybe
                      , mapDyn
                      , forDyn
                      , mapDynM
                      , foldDyn
                      , foldDynM
                      , foldDynMaybe
                      , foldDynMaybeM
                      , combineDyn
                      , apDyn
                      , collectDyn
                      , mconcatDyn
                      , distributeDMapOverDyn
                      , joinDyn
                      , joinDynThroughMap
                      , traceDyn
                      , traceDynWith
                      , splitDyn
                      , Demux
                      , demux
                      , getDemuxed
                        -- Things that probably aren't very useful:
                      , HList (..)
                      , FHList (..)
                      , distributeFHListOverDyn
                        -- Unsafe
                      , unsafeDynamic
                      ) where

import Prelude hiding (mapM, mapM_)

import Reflex.Class
import Data.Functor.Misc

import Control.Monad hiding (mapM, mapM_, forM, forM_)
import Control.Monad.Fix
import Control.Monad.Identity hiding (mapM, mapM_, forM, forM_)
import Data.These
import Data.Traversable (mapM, forM)
import Data.Align
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare (GCompare (..), GEq (..), (:~:) (..), GOrdering (..))
import Data.Monoid
--import Data.HList (HList (..), hBuild)

data HList (l::[*]) where
    HNil  :: HList '[]
    HCons :: e -> HList l -> HList (e ': l)

infixr 2 `HCons`

type family HRevApp (l1 :: [k]) (l2 :: [k]) :: [k]
type instance HRevApp '[] l = l
type instance HRevApp (e ': l) l' = HRevApp l (e ': l')

hRevApp :: HList l1 -> HList l2 -> HList (HRevApp l1 l2)
hRevApp HNil l = l
hRevApp (HCons x l) l' = hRevApp l (HCons x l')

hReverse :: HList l -> HList (HRevApp l '[])
hReverse l = hRevApp l HNil

hBuild :: (HBuild' '[] r) => r
hBuild =  hBuild' HNil

class HBuild' l r where
    hBuild' :: HList l -> r

instance (l' ~ HRevApp l '[])
      => HBuild' l (HList l') where
  hBuild' l = hReverse l

instance HBuild' (a ': l) r
      => HBuild' l (a->r) where
  hBuild' l x = hBuild' (HCons x l)

-- | A container for a value that can change over time and allows notifications on changes.
-- Basically a combination of a 'Behavior' and an 'Event', with a rule that the Behavior will
-- change if and only if the Event fires.
--
-- Although @Dynamic@ logically has a 'Functor' instance, currently it can\'t
-- be implemented without potentially requiring the mapped function to be
-- evaluated twice. Instead, you can use 'mapDyn', which runs in a 'MonadHold'
-- instance, to achieve the same result.
data Dynamic t a
  = Dynamic (Behavior t a) (Event t a)

unsafeDynamic :: Behavior t a -> Event t a -> Dynamic t a
unsafeDynamic = Dynamic

-- | Extract the 'Behavior' of a 'Dynamic'.
current :: Dynamic t a -> Behavior t a
current (Dynamic b _) = b

-- | Extract the 'Event' of the 'Dynamic'.
updated :: Dynamic t a -> Event t a
updated (Dynamic _ e) = e

-- | 'Dynamic' with the constant supplied value.
constDyn :: Reflex t => a -> Dynamic t a
constDyn x = Dynamic (constant x) never

-- | Create a 'Dynamic' using the initial value that changes every
-- time the 'Event' occurs.
holdDyn :: MonadHold t m => a -> Event t a -> m (Dynamic t a)
holdDyn v0 e = do
  b <- hold v0 e
  return $ Dynamic b e

-- | Create a new 'Dynamic' that only signals changes if the values
-- actually changed.
nubDyn :: (Reflex t, Eq a) => Dynamic t a -> Dynamic t a
nubDyn d =
  let e' = attachWithMaybe (\x x' -> if x' == x then Nothing else Just x') (current d) (updated d)
  in Dynamic (current d) e' --TODO: Avoid invalidating the outgoing Behavior

{-
instance Reflex t => Functor (Dynamic t) where
  fmap f d =
    let e' = fmap f $ updated d
        eb' = push (\b' -> liftM Just $ constant b') e'
        b0 = fmap f $ current d
    
-}      

-- | Map a function over a 'Dynamic'.
mapDyn :: (Reflex t, MonadHold t m) => (a -> b) -> Dynamic t a -> m (Dynamic t b)
mapDyn f = mapDynM $ return . f

-- | Flipped version of 'mapDyn'.
forDyn :: (Reflex t, MonadHold t m) => Dynamic t a -> (a -> b) -> m (Dynamic t b)
forDyn = flip mapDyn

-- | Map a monadic function over a 'Dynamic'.  The only monadic action that the given function can
-- perform is 'sample'.
{-# INLINE mapDynM #-}
mapDynM :: forall t m a b. (Reflex t, MonadHold t m) => (forall m'. MonadSample t m' => a -> m' b) -> Dynamic t a -> m (Dynamic t b)
mapDynM f d = do
  let e' = push (liftM Just . f :: a -> PushM t (Maybe b)) $ updated d
      eb' = fmap constant e'
      v0 = pull $ f =<< sample (current d)
  bb' :: Behavior t (Behavior t b) <- hold v0 eb'
  let b' = pull $ sample =<< sample bb'
  return $ Dynamic b' e'

-- | Create a 'Dynamic' using the initial value and change it each
-- time the 'Event' occurs using a folding function on the previous
-- value and the value of the 'Event'.
foldDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
foldDyn f = foldDynMaybe $ \o v -> Just $ f o v

-- | Create a 'Dynamic' using the initial value and change it each
-- time the 'Event' occurs using a monadic folding function on the
-- previous value and the value of the 'Event'.
foldDynM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t b) -> b -> Event t a -> m (Dynamic t b)
foldDynM f = foldDynMaybeM $ \o v -> liftM Just $ f o v

foldDynMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> Maybe b) -> b -> Event t a -> m (Dynamic t b)
foldDynMaybe f = foldDynMaybeM $ \o v -> return $ f o v

foldDynMaybeM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe b)) -> b -> Event t a -> m (Dynamic t b)
foldDynMaybeM f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample b'
            f o v
      b' <- hold z e'
  return $ Dynamic b' e'

-- | Create a new 'Dynamic' that counts the occurences of the 'Event'.
count :: (Reflex t, MonadHold t m, MonadFix m, Num b) => Event t a -> m (Dynamic t b)
count e = holdDyn 0 =<< zipListWithEvent const (iterate (+1) 1) e

-- | Create a new 'Dynamic' using the initial value that flips its
-- value every time the 'Event' occurs.
toggle :: (Reflex t, MonadHold t m, MonadFix m) => Bool -> Event t a -> m (Dynamic t Bool)
toggle = foldDyn (const not)

-- | Switches to the new 'Event' whenever it receives one.  Switching
-- occurs *before* the inner 'Event' fires - so if the 'Dynamic' changes and both the old and new
-- inner Events fire simultaneously, the output will fire with the value of the *new* 'Event'.
switchPromptlyDyn :: forall t a. Reflex t => Dynamic t (Event t a) -> Event t a
switchPromptlyDyn de =
  let eLag = switch $ current de
      eCoincidences = coincidence $ updated de
  in leftmost [eCoincidences, eLag]

{-

mergeEventsWith :: Reflex t m => (a -> a -> a) -> Event t a -> Event t a -> m (Event t a)
mergeEventsWith f ea eb = mapE (mergeThese f) =<< alignEvents ea eb

firstE :: (Reflex t m) => [Event t a] -> m (Event t a)
firstE [] = return never
firstE (h:t) = mergeEventsLeftBiased h =<< firstE t

concatEventsWith :: (Reflex t m) => (a -> a -> a) -> [Event t a] -> m (Event t a)
concatEventsWith _ [] = return never
concatEventsWith _ [e] = return e
concatEventsWith f es = mapEM (liftM (foldl1 f . map (\(Const2 _ :=> v) -> v) . DMap.toList) . sequenceDmap) <=< mergeEventDMap $ DMap.fromList $ map (\(k, v) -> WrapArg (Const2 k) :=> v) $ zip [0 :: Int ..] es
--concatEventsWith f (h:t) = mergeEventsWith f h =<< concatEventsWith f t

mconcatE :: (Reflex t m, Monoid a) => [Event t a] -> m (Event t a)
mconcatE = concatEventsWith mappend
-}

-- | Split the 'Dynamic' into two 'Dynamic's, each taking the
-- respective value of the tuple.
splitDyn :: (Reflex t, MonadHold t m) => Dynamic t (a, b) -> m (Dynamic t a, Dynamic t b)
splitDyn d = liftM2 (,) (mapDyn fst d) (mapDyn snd d)

-- | Merge the 'Dynamic' values using their 'Monoid' instance.
mconcatDyn :: forall t m a. (Reflex t, MonadHold t m, Monoid a) => [Dynamic t a] -> m (Dynamic t a)
mconcatDyn es = do
  ddm :: Dynamic t (DMap (Const2 Int a) Identity) <- distributeDMapOverDyn . DMap.fromList . map (\(k, v) -> Const2 k :=> v) $ zip [0 :: Int ..] es
  mapDyn (mconcat . map (\(Const2 _ :=> Identity v) -> v) . DMap.toList) ddm

-- | Create a 'Dynamic' with a 'DMap' of values out of a 'DMap' of
-- Dynamic values.
distributeDMapOverDyn :: forall t m k. (Reflex t, MonadHold t m, GCompare k) => DMap k (Dynamic t) -> m (Dynamic t (DMap k Identity))
distributeDMapOverDyn dm = case DMap.toList dm of
  [] -> return $ constDyn DMap.empty
  [k :=> v] -> mapDyn (DMap.singleton k . Identity) v
  _ -> do
    let edmPre = merge $ rewrapDMap updated dm
        edm :: Event t (DMap k Identity) = flip push edmPre $ \o -> return . Just =<< do
          let f _ = \case
                This origDyn -> sample $ current origDyn
                That _ -> error "distributeDMapOverDyn: should be impossible to have an event occurring that is not present in the original DMap"
                These _ (Identity newVal) -> return newVal
          sequenceDmap $ combineDMapsWithKey f dm (wrapDMap Identity o)
        dm0 :: Behavior t (DMap k Identity) = pull $ do
          liftM DMap.fromList . forM (DMap.toList dm) $ \(k :=> dv) -> liftM ((k :=>) . Identity) . sample $ current dv
    bbdm :: Behavior t (Behavior t (DMap k Identity)) <- hold dm0 $ fmap constant edm
    let bdm = pull $ sample =<< sample bbdm
    return $ Dynamic bdm edm

-- | Merge two 'Dynamic's into a new one using the provided
-- function. The new 'Dynamic' changes its value each time one of the
-- original 'Dynamic's changes its value.
combineDyn :: forall t m a b c. (Reflex t, MonadHold t m) => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> m (Dynamic t c)
combineDyn f da db = do
  let eab = align (updated da) (updated db)
      ec = flip push eab $ \o -> do
        (a, b) <- case o of
          This a -> do
            b <- sample $ current db
            return (a, b)
          That b -> do
            a <- sample $ current da
            return (a, b)
          These a b -> return (a, b)
        return $ Just $ f a b
      c0 :: Behavior t c = pull $ liftM2 f (sample $ current da) (sample $ current db)
  bbc :: Behavior t (Behavior t c) <- hold c0 $ fmap constant ec
  let bc :: Behavior t c = pull $ sample =<< sample bbc
  return $ Dynamic bc ec

-- | A psuedo applicative version of ap for 'Dynamic'. Example useage:
--
-- > do
-- >    person <- Person `mapDyn` dynFirstName
-- >                     `apDyn` dynListName
-- >                     `apDyn` dynAge
-- >                     `apDyn` dynAddress
apDyn :: forall t m a b. (Reflex t, MonadHold t m)
      => m (Dynamic t (a -> b))
      -> Dynamic t a
      -> m (Dynamic t b)
apDyn m a = do
  r <- m
  combineDyn (\f c -> f c) r a

{-
tagInnerDyn :: Reflex t => Event t (Dynamic t a) -> Event t a
tagInnerDyn e =
  let eSlow = push (liftM Just . sample . current) e
      eFast = coincidence $ fmap updated e
  in leftmost [eFast, eSlow]
-}

-- | Join a nested 'Dynamic' into a new 'Dynamic' that has the value
-- of the inner 'Dynamic'.
joinDyn :: forall t a. (Reflex t) => Dynamic t (Dynamic t a) -> Dynamic t a
joinDyn dd =
  let b' = pull $ sample . current =<< sample (current dd)
      eOuter :: Event t a = pushAlways (sample . current) $ updated dd
      eInner :: Event t a = switch $ fmap updated (current dd)
      eBoth :: Event t a = coincidence $ fmap updated (updated dd)
      e' = leftmost [eBoth, eOuter, eInner]
  in Dynamic b' e'

--TODO: Generalize this to functors other than Maps
-- | Combine a 'Dynamic' of a 'Map' of 'Dynamic's into a 'Dynamic'
-- with the current values of the 'Dynamic's in a map.
joinDynThroughMap :: forall t k a. (Reflex t, Ord k) => Dynamic t (Map k (Dynamic t a)) -> Dynamic t (Map k a)
joinDynThroughMap dd =
  let b' = pull $ mapM (sample . current) =<< sample (current dd)
      eOuter :: Event t (Map k a) = pushAlways (mapM (sample . current)) $ updated dd
      eInner :: Event t (Map k a) = attachWith (flip Map.union) b' $ switch $ fmap (mergeMap . fmap updated) (current dd) --Note: the flip is important because Map.union is left-biased
      readNonFiring :: MonadSample t m => These (Dynamic t a) a -> m a
      readNonFiring = \case
        This d -> sample $ current d
        That a -> return a
        These _ a -> return a
      eBoth :: Event t (Map k a) = coincidence $ fmap (\m -> pushAlways (mapM readNonFiring . align m) $ mergeMap $ fmap updated m) (updated dd)
      e' = leftmost [eBoth, eOuter, eInner]
  in Dynamic b' e'

-- | Print the value of the 'Dynamic' on each change and prefix it
-- with the provided string. This should /only/ be used for debugging.
--
-- Note: Just like Debug.Trace.trace, the value will only be shown if something
-- else in the system is depending on it.
traceDyn :: (Reflex t, Show a) => String -> Dynamic t a -> Dynamic t a
traceDyn s = traceDynWith $ \x -> s <> ": " <> show x

-- | Print the result of applying the provided function to the value
-- of the 'Dynamic' on each change. This should /only/ be used for
-- debugging.
--
-- Note: Just like Debug.Trace.trace, the value will only be shown if something
-- else in the system is depending on it.
traceDynWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
traceDynWith f d =
  let e' = traceEventWith f $ updated d
  in Dynamic (current d) e'

-- | Replace the value of the 'Event' with the current value of the 'Dynamic'
-- each time the 'Event' occurs.
--
-- Note: `tagDyn d e` differs from `tag (current d) e` in the case that `e` is firing
-- at the same time that `d` is changing.  With `tagDyn d e`, the *new* value of `d`
-- will replace the value of `e`, whereas with `tag (current d) e`, the *old* value
-- will be used, since the 'Behavior' won't be updated until the end of the frame.
-- Additionally, this means that the output 'Event' may not be used to directly change
-- the input 'Dynamic', because that would mean its value depends on itself.  When creating
-- cyclic data flows, generally `tag (current d) e` is preferred.
tagDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagDyn = attachDynWith const

-- | Attach the current value of the 'Dynamic' to the value of the
-- 'Event' each time it occurs.
--
-- Note: `attachDyn d` is not the same as `attach (current d)`.  See 'tagDyn' for details.
attachDyn :: Reflex t => Dynamic t a -> Event t b -> Event t (a, b)
attachDyn = attachDynWith (,)

-- | Combine the current value of the 'Dynamic' with the value of the
-- 'Event' each time it occurs.
--
-- Note: `attachDynWith f d` is not the same as `attachWith f (current d)`.  See 'tagDyn' for details.
attachDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Event t b -> Event t c
attachDynWith f = attachDynWithMaybe $ \a b -> Just $ f a b

-- | Create a new 'Event' by combining the value at each occurence
-- with the current value of the 'Dynamic' value and possibly
-- filtering if the combining function returns 'Nothing'.
--
-- Note: `attachDynWithMaybe f d` is not the same as `attachWithMaybe f (current d)`.  See 'tagDyn' for details.
attachDynWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Dynamic t a -> Event t b -> Event t c
attachDynWithMaybe f d e =
  let e' = attach (current d) e
  in fforMaybe (align e' $ updated d) $ \case
       This (a, b) -> f a b -- Only the tagging event is firing, so use that
       These (_, b) a -> f a b -- Both events are firing, so use the newer value
       That _ -> Nothing -- The tagging event isn't firing, so don't fire

--------------------------------------------------------------------------------
-- Demux
--------------------------------------------------------------------------------

-- | Represents a time changing value together with an 'EventSelector'
-- that can efficiently detect when the underlying Dynamic has a particular value.
-- This is useful for representing data like the current selection of a long list.
--
-- Semantically,
-- > getDemuxed (demux d) k === mapDyn (== k) d
-- However, the when getDemuxed is used multiple times, the complexity is only /O(log(n))/,
-- rather than /O(n)/ for mapDyn.
data Demux t k = Demux { demuxValue :: Behavior t k
                       , demuxSelector :: EventSelector t (Const2 k Bool)
                       }

-- | Demultiplex an input value to a 'Demux' with many outputs.  At any given time, whichever output is indicated by the given 'Dynamic' will be 'True'.
demux :: (Reflex t, Ord k) => Dynamic t k -> Demux t k
demux k = Demux (current k)
                (fan $ attachWith (\k0 k1 -> if k0 == k1        
                                                then DMap.empty
                                                else DMap.fromList [Const2 k0 :=> Identity False,
                                                                    Const2 k1 :=> Identity True])
                                  (current k) (updated k))

--TODO: The pattern of using hold (sample b0) can be reused in various places as a safe way of building certain kinds of Dynamics; see if we can factor this out
-- | Select a particular output of the 'Demux'; this is equivalent to (but much faster than)
-- mapping over the original 'Dynamic' and checking whether it is equal to the given key.
getDemuxed :: (Reflex t, MonadHold t m, Eq k) => Demux t k -> k -> m (Dynamic t Bool)
getDemuxed d k = do
  let e = select (demuxSelector d) (Const2 k)
  bb <- hold (liftM (==k) $ sample $ demuxValue d) $ fmap return e
  let b = pull $ join $ sample bb
  return $ Dynamic b e

--------------------------------------------------------------------------------
-- collectDyn
--------------------------------------------------------------------------------

--TODO: This whole section is badly in need of cleanup

data FHList f l where
  FHNil :: FHList f '[]
  FHCons :: f e -> FHList f l -> FHList f (e ': l)

instance GEq (HListPtr l) where
  HHeadPtr `geq` HHeadPtr = Just Refl
  HHeadPtr `geq` HTailPtr _ = Nothing
  HTailPtr _ `geq` HHeadPtr = Nothing
  HTailPtr a `geq` HTailPtr b = a `geq` b

instance GCompare (HListPtr l) where -- Warning: This ordering can't change, dmapTo*HList will break
  HHeadPtr `gcompare` HHeadPtr = GEQ
  HHeadPtr `gcompare` HTailPtr _ = GLT
  HTailPtr _ `gcompare` HHeadPtr = GGT
  HTailPtr a `gcompare` HTailPtr b = a `gcompare` b

data HListPtr l a where
  HHeadPtr :: HListPtr (h ': t) h
  HTailPtr :: HListPtr t a -> HListPtr (h ': t) a

fhlistToDMap :: forall (f :: * -> *) l. FHList f l -> DMap (HListPtr l) f
fhlistToDMap = DMap.fromList . go
  where go :: forall l'. FHList f l' -> [DSum (HListPtr l') f]
        go = \case
          FHNil -> []
          FHCons h t -> (HHeadPtr :=> h) : map (\(p :=> v) -> (HTailPtr p) :=> v) (go t)

class RebuildSortedHList l where
  rebuildSortedFHList :: [DSum (HListPtr l) f] -> FHList f l
  rebuildSortedHList :: [DSum (HListPtr l) Identity] -> HList l

instance RebuildSortedHList '[] where
  rebuildSortedFHList l = case l of
    [] -> FHNil
    _ : _ -> error "rebuildSortedFHList{'[]}: empty list expected"
  rebuildSortedHList l = case l of
    [] -> HNil
    _ : _ -> error "rebuildSortedHList{'[]}: empty list expected"

instance RebuildSortedHList t => RebuildSortedHList (h ': t) where
  rebuildSortedFHList l = case l of
    ((HHeadPtr :=> h) : t) -> FHCons h . rebuildSortedFHList . map (\(HTailPtr p :=> v) -> p :=> v) $ t
    _ -> error "rebuildSortedFHList{h':t}: non-empty list with HHeadPtr expected"
  rebuildSortedHList l = case l of
    ((HHeadPtr :=> Identity h) : t) -> HCons h . rebuildSortedHList . map (\(HTailPtr p :=> v) -> p :=> v) $ t
    _ -> error "rebuildSortedHList{h':t}: non-empty list with HHeadPtr expected"

dmapToHList :: forall l. RebuildSortedHList l => DMap (HListPtr l) Identity -> HList l
dmapToHList = rebuildSortedHList . DMap.toList

distributeFHListOverDyn :: forall t m l. (Reflex t, MonadHold t m, RebuildSortedHList l) => FHList (Dynamic t) l -> m (Dynamic t (HList l))
distributeFHListOverDyn l = mapDyn dmapToHList =<< distributeDMapOverDyn (fhlistToDMap l)
{-
distributeFHListOverDyn l = do
  let ec = undefined
      c0 = pull $ sequenceFHList $ natMap (sample . current) l
  bbc <- hold c0 $ fmap constant ec
  let bc = pull $ sample =<< sample bbc
  return $ Dynamic bc ec
-}

class AllAreFunctors (f :: a -> *) (l :: [a]) where
  type FunctorList f l :: [*]
  toFHList :: HList (FunctorList f l) -> FHList f l
  fromFHList :: FHList f l -> HList (FunctorList f l)

instance AllAreFunctors f '[] where
  type FunctorList f '[] = '[]
  toFHList l = case l of
    HNil -> FHNil
    _ -> error "toFHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139
  fromFHList FHNil = HNil

instance AllAreFunctors f t => AllAreFunctors f (h ': t) where
  type FunctorList f (h ': t) = f h ': FunctorList f t
  toFHList l = case l of
    a `HCons` b -> a `FHCons` toFHList b
    _ -> error "toFHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139
  fromFHList (a `FHCons` b) = a `HCons` fromFHList b

collectDyn :: ( RebuildSortedHList (HListElems b)
              , IsHList a, IsHList b
              , AllAreFunctors (Dynamic t) (HListElems b)
              , Reflex t, MonadHold t m
              , HListElems a ~ FunctorList (Dynamic t) (HListElems b)
              ) => a -> m (Dynamic t b)
collectDyn ds =
  mapDyn fromHList =<< distributeFHListOverDyn (toFHList $ toHList ds)

-- Poor man's Generic
class IsHList a where
  type HListElems a :: [*]
  toHList :: a -> HList (HListElems a)
  fromHList :: HList (HListElems a) -> a

instance IsHList (a, b) where
  type HListElems (a, b) = [a, b]
  toHList (a, b) = hBuild a b
  fromHList l = case l of
    a `HCons` b `HCons` HNil -> (a, b)
    _ -> error "fromHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139

instance IsHList (a, b, c, d) where
  type HListElems (a, b, c, d) = [a, b, c, d]
  toHList (a, b, c, d) = hBuild a b c d
  fromHList l = case l of
    a `HCons` b `HCons` c `HCons` d `HCons` HNil -> (a, b, c, d)
    _ -> error "fromHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139

instance IsHList (a, b, c, d, e, f) where
  type HListElems (a, b, c, d, e, f) = [a, b, c, d, e, f]
  toHList (a, b, c, d, e, f) = hBuild a b c d e f
  fromHList l = case l of
    a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil -> (a, b, c, d, e, f)
    _ -> error "fromHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139
