{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Reflex.Dynamic.TH (qDyn, unqDyn, mkDyn) where

import Reflex.Dynamic

import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote
import Data.Data
import Control.Monad.State
import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Meta.Syntax.Translate as Hs
import Data.Monoid
import Data.Generics

-- | Quote a Dynamic expression.  Within the quoted expression, you can use '$(unqDyn [| x |])' to refer to any expression 'x' of type 'Dynamic t a'; the unquoted result will be of type 'a'
qDyn :: Q Exp -> Q Exp
qDyn qe = do
  e <- qe
  let f :: forall d. Data d => d -> StateT [(Name, Exp)] Q d
      f d = case eqT of
        Just (Refl :: d :~: Exp)
          | AppE (VarE m) eInner <- d
          , m == 'unqMarker
          -> do n <- lift $ newName "dynamicQuotedExpressionVariable"
                modify ((n, eInner):)
                return $ VarE n
        _ -> gmapM f d
  (e', exprsReversed) <- runStateT (gmapM f e) []
  let exprs = reverse exprsReversed
      arg = foldr (\a b -> ConE 'FHCons `AppE` a `AppE` b) (ConE 'FHNil) $ map snd exprs
      param = foldr (\a b -> ConP 'HCons [VarP a, b]) (ConP 'HNil []) $ map fst exprs
  [| mapDyn $(return $ LamE [param] e') =<< distributeFHListOverDyn $(return arg) |]

unqDyn :: Q Exp -> Q Exp
unqDyn e = [| unqMarker $e |]

-- | This type represents an occurrence of unqDyn before it has been processed by qDyn.  If you see it in a type error, it probably means that unqDyn has been used outside of a qDyn context.
data UnqDyn

-- unqMarker must not be exported; it is used only as a way of smuggling data from unqDyn to qDyn
--TODO: It would be much nicer if the TH AST was extensible to support this kind of thing without trickery
unqMarker :: a -> UnqDyn
unqMarker = error "An unqDyn expression was used outside of a qDyn expression"

mkDyn :: QuasiQuoter
mkDyn = QuasiQuoter
  { quoteExp = mkDynExp
  , quotePat = error "mkDyn: pattern splices are not supported"
  , quoteType = error "mkDyn: type splices are not supported"
  , quoteDec = error "mkDyn: declaration splices are not supported"
  }

mkDynExp :: String -> Q Exp
mkDynExp s = case Hs.parseExpWithMode (Hs.defaultParseMode { Hs.extensions = [ Hs.EnableExtension Hs.TemplateHaskell ] }) s of
  Hs.ParseFailed (Hs.SrcLoc _ l c) err -> fail $ "mkDyn:" <> show l <> ":" <> show c <> ": " <> err
  Hs.ParseOk e -> qDyn $ return $ everywhere (id `extT` reinstateUnqDyn) $ Hs.toExp $ everywhere (id `extT` antiE) e
    where TH.Name (TH.OccName occName) (TH.NameG _ _ (TH.ModName modName)) = 'unqMarker
#if MIN_VERSION_haskell_src_exts(1,18,0)
          antiE :: Hs.Exp Hs.SrcSpanInfo -> Hs.Exp Hs.SrcSpanInfo
          antiE x = case x of
            Hs.SpliceExp l se ->
              Hs.App l (Hs.Var l $ Hs.Qual l (Hs.ModuleName l modName) (Hs.Ident l occName)) $ case se of
                Hs.IdSplice l2 v -> Hs.Var l2 $ Hs.UnQual l2 $ Hs.Ident l2 v
                Hs.ParenSplice _ ps -> ps
            _ -> x
#else
          antiE x = case x of
            Hs.SpliceExp se ->
              Hs.App (Hs.Var $ Hs.Qual (Hs.ModuleName modName) (Hs.Ident occName)) $ case se of
                Hs.IdSplice v -> Hs.Var $ Hs.UnQual $ Hs.Ident v
                Hs.ParenSplice ps -> ps
            _ -> x
#endif
          reinstateUnqDyn (TH.Name (TH.OccName occName') (TH.NameQ (TH.ModName modName')))
            | modName == modName' && occName == occName' = 'unqMarker
          reinstateUnqDyn x = x
