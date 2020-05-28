{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Host.Headless
import Reflex.Network
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.These (These (..))
import Data.Either (Either (..), partitionEithers)
import Control.Monad.Fix (MonadFix)
import Control.Monad (void)
import Control.Concurrent
import Control.Monad.IO.Class

main :: IO ()
main = runHeadlessApp $ do
  (e, eTrigger) <- newTriggerEvent
  mainWidget e
  liftIO $ forkIO $ do
    threadDelay 1000000
    eTrigger ()
  pure never

mainWidget :: (MonadFix m, PostBuild t m, MonadHold t m, NotReady t m, Adjustable t m) => Event t () -> m ()
mainWidget e = do
  rec let (appChangeEvents, _) = fanThese $ partitionEithersNE <$> stateChanges
      dragDyn <- holdDyn () $ NonEmpty.last <$> appChangeEvents
      (_, stateChanges' :: Event t (NonEmpty (Either () ()))) <-
        runEventWriterT
          (do
            tellEvent . fmap (:| [])
               . fmap (\_ -> Right ()) $ traceEventWith (const "Creating Task2") e
            void $ networkView $ (void $ networkView $ pure () <$ dragDyn) <$ pure ()
            void $ networkView $ (void $ networkView $ pure () <$ dragDyn) <$ pure ()
            void $ networkView $ (void $ networkView $ pure () <$ dragDyn) <$ pure ()
            void $ networkView $ (void $ networkView $ pure () <$ dragDyn) <$ pure ()
            void $ networkView $ (void $ networkView $ pure () <$ dragDyn) <$ pure ()
            void $ networkView $ (void $ networkView $ pure () <$ dragDyn) <$ pure ()
            void $ networkView $ (void $ networkView $ pure () <$ dragDyn) <$ pure ()
          )
      stateChanges <- pure $ traceEventWith (const "StateChange") stateChanges'
  pure ()

partitionEithersNE :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)
partitionEithersNE (x :| xs) = case (x, ls, rs) of
  (Left  y, ys    , []    ) -> This (y :| ys)
  (Left  y, ys    , z : zs) -> These (y :| ys) (z :| zs)
  (Right z, []    , zs    ) -> That (z :| zs)
  (Right z, y : ys, zs    ) -> These (y :| ys) (z :| zs)
  where (ls, rs) = partitionEithers xs
