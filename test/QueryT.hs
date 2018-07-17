{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Align
import Data.AppendMap () -- for the Align instance
import qualified Data.AppendMap as AMap
import Data.Foldable
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import Data.Semigroup
import Data.These

import Reflex
import Reflex.Patch.MapWithMove
import Test.Run

newtype MyQuery = MyQuery SelectedCount
  deriving (Show, Read, Eq, Ord, Monoid, Semigroup, Additive, Group)

instance Query MyQuery where
  type QueryResult MyQuery = ()
  crop _ _ = ()

instance (Ord k, Query a, Eq (QueryResult a)) => Query (Selector k a) where
  type QueryResult (Selector k a) = Selector k (QueryResult a)
  crop q r = undefined

newtype Selector k a = Selector { unSelector :: MonoidalMap k a }
  deriving (Show, Read, Eq, Ord, Functor)

instance (Ord k, Eq a, Monoid a) => Semigroup (Selector k a) where
  (Selector a) <> (Selector b) = Selector $ fmapMaybe id $ f a b
    where
      f = alignWith $ \case
        This x -> Just x
        That y -> Just y
        These x y ->
          let z = x `mappend` y
          in if z == mempty then Nothing else Just z

instance (Ord k, Eq a, Monoid a) => Monoid (Selector k a) where
  mempty = Selector AMap.empty
  mappend = (<>)

instance (Eq a, Ord k, Group a) => Group (Selector k a) where
  negateG = fmap negateG

instance (Eq a, Ord k, Group a) => Additive (Selector k a)

main :: IO ()
main = do
  [[0], [1], [1], [0]] <- fmap (map (AMap.keys . unSelector . fst) . concat) $
    runApp (testQueryT testRunWithReplace) () $ map (Just . That) $
      [ That (), This (), That () ]
  result <-
    runApp (testQueryT testRunWithReplace2) () $ map (Just . That) $
      join $ replicate 5 [ That (), This $ This (), This $ That () ]
  mapM_ print result
  [[0], [1], [1], [0]] <- fmap (map (AMap.keys . unSelector . fst) . concat) $
    runApp (testQueryT testSequenceDMapWithAdjust) () $ map (Just . That) $
      [ That (), This (), That () ]
  [[0], [1], [1], [0]] <- fmap (map (AMap.keys . unSelector . fst) . concat) $
    runApp (testQueryT testSequenceDMapWithAdjustWithMove) () $ map (Just . That) $
      [ That (), This (), That () ]
  return ()

testQueryT
  :: ( Reflex t
     , MonadFix m
     , Group q
     , Additive q
     , Monoid (QueryResult q)
     --, PerformEvent t m
     --, MonadIO (Performable m)
     , Show q
     )
  => (Event t a -> Event t b -> QueryT t q m ())
  -> AppIn t () (These a b)
  -> m (AppOut t q q)
testQueryT w (AppIn _ pulse) = do
  let replace = fmapMaybe (^? here) pulse
      increment = fmapMaybe (^? there) pulse
  (_, q, b) <- runQueryTrace (w replace increment) $ pure mempty
  --performEvent_ $ ffor patch $ \p -> liftIO $ putStrLn $ "PATCH: " ++ show p
  let qDyn = incrementalToDynamic q
  return $ AppOut
    { _appOut_behavior = b
    , _appOut_event = updated qDyn
    }

runQueryTrace :: (MonadFix m, Additive q, Group q, Show q, Reflex t) => QueryT t q m a -> Dynamic t (QueryResult q) -> m (a, Incremental t (AdditivePatch q), Behavior t q)
runQueryTrace (QueryT a) qr = do
  ((r, bs), es) <- runReaderT (runEventWriterT (runStateT a mempty)) qr
  let b = (foldlM (\b c -> (b <>) <$> sample c) mempty bs)
  return (r, unsafeBuildIncremental b (fmapCheap AdditivePatch $ traceEvent "PATCH" es), pull b)

--queryDynTrace :: (Reflex t, Monad m, MonadQuery t q m) => Dynamic t q -> m (Dynamic t (QueryResult q))
--queryDynTrace q = do
--  tellQueryDynTrace q
--  zipDynWith crop q <$> askQueryResult
--
--tellQueryDynTrace :: (Reflex t, MonadQuery t q m) => Dynamic t q -> m ()
--tellQueryDynTrace d = tellQueryIncrementalTrace $ unsafeBuildIncremental (sample (current d)) $ attachWith (\old new -> AdditivePatch $ new ~~ old) (current d) (updated d)
--
--tellQueryIncrementalTrace q = do
--  QueryT (modify (currentIncremental q:))
--  QueryT (lift (tellEvent (fmapCheap unAdditivePatch (updatedIncremental q))))

runWithReplaceQueryT
  :: forall t q m a b
  . ( Reflex t
    , MonadFix m
    , Group q
    , Additive q
    , Query q
    , MonadHold t m
    , Adjustable t m
    )
  => QueryT t q m a
  -> Event t (QueryT t q m b)
  -> QueryT t q m (a, Event t b)
runWithReplaceQueryT (QueryT a0) a' = do
  bs <- QueryT get
  ((r0, bs0), r') <- QueryT $ lift $ runWithReplace (runStateT a0 bs) $ fmapCheap (flip runStateT bs . unQueryT) a'
  let sampleBs :: forall m'. MonadSample t m' => [Behavior t q] -> m' q
      sampleBs = foldlM (\b a -> (b <>) <$> sample a) mempty
      bs' = fmapCheap snd $ r'
  bbs <- hold bs0 bs'
  let patches = flip pushAlwaysCheap bs' $ \newBs -> do
        oldBs <- sample bbs
        (~~) <$> sampleBs newBs <*> sampleBs oldBs
  QueryT $ modify $ (:) $ pull $ sampleBs =<< sample bbs
  --QueryT $ put $ pure $ pull $ sampleBs =<< sample bbs
  QueryT $ lift $ tellEvent patches
  --QueryT $ lift $ tellEvent $ flip pushAlwaysCheap bs' $ \newBs -> sampleBs newBs
  return (r0, fmapCheap fst r')

testRunWithReplace :: ( Reflex t
                      , Adjustable t m
                      , MonadHold t m
                      , MonadFix m
                      , MonadQuery t (Selector Int MyQuery) m)
                   => Event t ()
                   -> Event t ()
                   -> m ()
testRunWithReplace replace increment = do
  let w = do
        n <- count increment
        queryDyn $ zipDynWith (\x y -> Selector (AMap.singleton (x :: Int) y)) n $ pure $ MyQuery $ SelectedCount 1
  _ <- runWithReplace w $ w <$ replace
  return ()

testRunWithReplace2
  :: forall t m
  .  ( Reflex t
     , MonadFix m
     , MonadHold t m
     , Adjustable t m
     --, MonadQuery t MyQuery m
     )
  => Event t (These () ())
  -> Event t ()
  -> QueryT t MyQuery m ()
testRunWithReplace2 pb tick = do
  let pbOuter = fmapMaybe (^? here) pb
  let pbInner = fmapMaybe (^? there) pb
  void $ runWithReplaceQueryT (return ()) $ ffor tick $ const $ do
    void $ runWithReplaceQueryT (return ()) $ ffor (leftmost [pbOuter, tick]) $ const $ do
      void $ runWithReplaceQueryT (return ()) $ ffor (leftmost [pbInner, tick]) $ const $ do
        --void $ queryDyn $ pure $ MyQuery $ SelectedCount 1
        let q :: Incremental t (AdditivePatch MyQuery) = unsafeBuildIncremental (pure $ MyQuery $ SelectedCount 1 :: PullM t MyQuery) never
        QueryT (modify (currentIncremental q:))
        --QueryT (lift (tellEvent (fmapCheap unAdditivePatch (updatedIncremental q))))

testSequenceDMapWithAdjust :: ( Reflex t
                              , Adjustable t m
                              , MonadHold t m
                              , MonadFix m
                              , MonadQuery t (Selector Int MyQuery) m)
                           => Event t ()
                           -> Event t ()
                           -> m ()
testSequenceDMapWithAdjust replace increment = do
  _ <- listHoldWithKey (Map.singleton () ()) (Map.singleton () (Just ()) <$ replace) $ \_ _ -> do
    n <- count increment
    queryDyn $ zipDynWith (\x y -> Selector (AMap.singleton (x :: Int) y)) n $ pure $ MyQuery $ SelectedCount 1
  return ()

testSequenceDMapWithAdjustWithMove :: ( Reflex t
                                      , Adjustable t m
                                      , MonadHold t m
                                      , MonadFix m
                                      , MonadQuery t (Selector Int MyQuery) m)
                                   => Event t ()
                                   -> Event t ()
                                   -> m ()
testSequenceDMapWithAdjustWithMove replace increment = do
  _ <- listHoldWithKeyWithMove (Map.singleton () ()) (Map.singleton () (Just ()) <$ replace) $ \_ _ -> do
    n <- count increment
    queryDyn $ zipDynWith (\x y -> Selector (AMap.singleton (x :: Int) y)) n $ pure $ MyQuery $ SelectedCount 1
  return ()

-- scam it out to test traverseDMapWithAdjustWithMove
listHoldWithKeyWithMove :: forall t m k v a. (Ord k, MonadHold t m, Adjustable t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))
listHoldWithKeyWithMove m0 m' f = do
  (n0, n') <- mapMapWithAdjustWithMove f m0 $ ffor m' $ PatchMapWithMove . Map.map (\v -> NodeInfo (maybe From_Delete From_Insert v) Nothing)
  incrementalToDynamic <$> holdIncremental n0 n'
-- -}
