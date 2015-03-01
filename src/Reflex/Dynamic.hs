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
                      , combineDyn
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
                      ) where

import Reflex.Class
import Data.Functor.Misc

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Data.These
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

hReverse l = hRevApp l HNil

hEnd :: HList l -> HList l
hEnd = id

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


data Dynamic t a
  = Dynamic (Behavior t a) (Event t a)

current :: Dynamic t a -> Behavior t a
current (Dynamic b _) = b

updated :: Dynamic t a -> Event t a
updated (Dynamic _ e) = e

constDyn :: Reflex t => a -> Dynamic t a
constDyn x = Dynamic (constant x) never

holdDyn :: MonadHold t m => a -> Event t a -> m (Dynamic t a)
holdDyn v0 e = do
  b <- hold v0 e
  return $ Dynamic b e

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

mapDyn :: (Reflex t, MonadHold t m) => (a -> b) -> Dynamic t a -> m (Dynamic t b)
mapDyn f = mapDynM $ return . f

forDyn :: (Reflex t, MonadHold t m) => Dynamic t a -> (a -> b) -> m (Dynamic t b)
forDyn = flip mapDyn

{-# INLINE mapDynM #-}
mapDynM :: forall t m a b. (Reflex t, MonadHold t m) => (forall m'. MonadSample t m' => a -> m' b) -> Dynamic t a -> m (Dynamic t b)
mapDynM f d = do
  let e' = push (liftM Just . f :: a -> PushM t (Maybe b)) $ updated d
      eb' = fmap constant e'
      v0 = pull $ f =<< sample (current d)
  bb' :: Behavior t (Behavior t b) <- hold v0 eb'
  let b' = pull $ sample =<< sample bb'
  return $ Dynamic b' e'

foldDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
foldDyn f = foldDynM (\o v -> return $ f o v)

foldDynM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t b) -> b -> Event t a -> m (Dynamic t b)
foldDynM f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample b'
            liftM Just $ f o v
      b' <- hold z e'
  return $ Dynamic b' e'

count :: (Reflex t, MonadHold t m, MonadFix m, Num b) => Event t a -> m (Dynamic t b)
count e = holdDyn 0 =<< zipListWithEvent const (iterate (+1) 1) e

toggle :: (Reflex t, MonadHold t m, MonadFix m) => Bool -> Event t a -> m (Dynamic t Bool)
toggle = foldDyn (const not)

-- | Switches to the new event whenever it receives one.  Switching occurs *before* occurring the inner event.
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

splitDyn :: (Reflex t, MonadHold t m) => Dynamic t (a, b) -> m (Dynamic t a, Dynamic t b)
splitDyn d = liftM2 (,) (mapDyn fst d) (mapDyn snd d)

mconcatDyn :: forall t m a. (Reflex t, MonadHold t m, Monoid a) => [Dynamic t a] -> m (Dynamic t a)
mconcatDyn es = do
  ddm :: Dynamic t (DMap (Const2 Int a)) <- distributeDMapOverDyn $ DMap.fromList $ map (\(k, v) -> WrapArg (Const2 k) :=> v) $ zip [0 :: Int ..] es
  mapDyn (mconcat . map (\(Const2 _ :=> v) -> v) . DMap.toList) ddm

distributeDMapOverDyn :: forall t m k. (Reflex t, MonadHold t m, GCompare k) => DMap (WrapArg (Dynamic t) k) -> m (Dynamic t (DMap k))
distributeDMapOverDyn dm = case DMap.toList dm of
  [] -> return $ constDyn DMap.empty
  [WrapArg k :=> v] -> mapDyn (DMap.singleton k) v
  _ -> do
    let edmPre = merge $ rewrapDMap updated dm
        edm :: Event t (DMap k) = flip push edmPre $ \o -> return . Just =<< do
          let f _ = \case
                This origDyn -> sample $ current origDyn
                That _ -> error "distributeDMapOverDyn: should be impossible to have an event occurring that is not present in the original DMap"
                These _ (Identity newVal) -> return newVal
          sequenceDmap $ combineDMapsWithKey f dm (wrapDMap Identity o)
        dm0 :: Behavior t (DMap k) = pull $ do
          liftM DMap.fromList $ forM (DMap.toList dm) $ \(WrapArg k :=> dv) -> liftM (k :=>) $ sample $ current dv
    bbdm :: Behavior t (Behavior t (DMap k)) <- hold dm0 $ fmap constant edm
    let bdm = pull $ sample =<< sample bbdm
    return $ Dynamic bdm edm

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

{-
tagInnerDyn :: Reflex t => Event t (Dynamic t a) -> Event t a
tagInnerDyn e =
  let eSlow = push (liftM Just . sample . current) e
      eFast = coincidence $ fmap updated e
  in leftmost [eFast, eSlow]
-}

joinDyn :: forall t a. (Reflex t) => Dynamic t (Dynamic t a) -> Dynamic t a
joinDyn dd =
  let b' = pull $ sample . current =<< sample (current dd)
      eOuter :: Event t a = pushAlways (sample . current) $ updated dd
      eInner :: Event t a = switch $ fmap updated (current dd)
      eBoth :: Event t a = coincidence $ fmap updated (updated dd)
      e' = leftmost [eBoth, eOuter, eInner]
  in Dynamic b' e'

--TODO: Generalize this to functors other than Maps
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

traceDyn :: (Reflex t, Show a) => String -> Dynamic t a -> Dynamic t a
traceDyn s = traceDynWith $ \x -> s <> ": " <> show x

traceDynWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
traceDynWith f d =
  let e' = traceEventWith f $ updated d
  in Dynamic (current d) e'

tagDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagDyn = attachDynWith const

attachDyn :: Reflex t => Dynamic t a -> Event t b -> Event t (a, b)
attachDyn = attachDynWith (,)

attachDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Event t b -> Event t c
attachDynWith f = attachDynWithMaybe $ \a b -> Just $ f a b

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

data Demux t k = Demux { demuxValue :: Behavior t k
                       , demuxSelector :: EventSelector t (Const2 k Bool)
                       }

demux :: (Reflex t, Ord k) => Dynamic t k -> Demux t k
demux k = Demux (current k) (fan $ attachWith (\k0 k1 -> if k0 == k1 then DMap.empty else DMap.fromList [Const2 k0 :=> False, Const2 k1 :=> True]) (current k) (updated k))

--TODO: The pattern of using hold (sample b0) can be reused in various places as a safe way of building certain kinds of Dynamics; see if we can factor this out
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

natMap :: (forall a. f a -> g a) -> FHList f l -> FHList g l
natMap f = \case
  FHNil -> FHNil
  h `FHCons` t -> f h `FHCons` natMap f t

sequenceFHList :: Monad m => FHList m l -> m (HList l)
sequenceFHList = \case
  FHNil -> return HNil
  h `FHCons` t -> do
    hResult <- h
    tResult <- sequenceFHList t
    return $ hResult `HCons` tResult

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

fhlistToDMap :: forall f l. FHList f l -> DMap (WrapArg f (HListPtr l))
fhlistToDMap = DMap.fromList . go
  where go :: forall l'. FHList f l' -> [DSum (WrapArg f (HListPtr l'))]
        go = \case
          FHNil -> []
          FHCons h t -> (WrapArg HHeadPtr :=> h) : map (\(WrapArg p :=> v) -> WrapArg (HTailPtr p) :=> v) (go t)

class RebuildSortedHList l where
  rebuildSortedFHList :: [DSum (WrapArg f (HListPtr l))] -> FHList f l
  rebuildSortedHList :: [DSum (HListPtr l)] -> HList l

instance RebuildSortedHList '[] where
  rebuildSortedFHList [] = FHNil
  rebuildSortedHList [] = HNil

instance RebuildSortedHList t => RebuildSortedHList (h ': t) where
  rebuildSortedFHList ((WrapArg HHeadPtr :=> h) : t) = FHCons h $ rebuildSortedFHList $ map (\(WrapArg (HTailPtr p) :=> v) -> WrapArg p :=> v) t
  rebuildSortedHList ((HHeadPtr :=> h) : t) = HCons h $ rebuildSortedHList $ map (\(HTailPtr p :=> v) -> p :=> v) t


dmapToFHList :: forall f l. RebuildSortedHList l => DMap (WrapArg f (HListPtr l)) -> FHList f l
dmapToFHList = rebuildSortedFHList . DMap.toList

dmapToHList :: forall f l. RebuildSortedHList l => DMap (HListPtr l) -> HList l
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
  toFHList HNil = FHNil
  fromFHList FHNil = HNil

instance AllAreFunctors f t => AllAreFunctors f (h ': t) where
  type FunctorList f (h ': t) = f h ': FunctorList f t
  toFHList (a `HCons` b) = a `FHCons` toFHList b
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
  fromHList (a `HCons` b `HCons` HNil) = (a, b)

instance IsHList (a, b, c, d) where
  type HListElems (a, b, c, d) = [a, b, c, d]
  toHList (a, b, c, d) = hBuild a b c d
  fromHList (a `HCons` b `HCons` c `HCons` d `HCons` HNil) = (a, b, c, d)

instance IsHList (a, b, c, d, e, f) where
  type HListElems (a, b, c, d, e, f) = [a, b, c, d, e, f]
  toHList (a, b, c, d, e, f) = hBuild a b c d e f
  fromHList (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil) = (a, b, c, d, e, f)
