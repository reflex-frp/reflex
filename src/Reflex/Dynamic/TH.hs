{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeOperators, GADTs, EmptyDataDecls #-}
module Reflex.Dynamic.TH (qDyn, unqDyn) where

import Reflex.Dynamic

import Language.Haskell.TH
import Data.Data
import Control.Monad.State

-- | Quote a Dynamic expression.  Within the quoted expression, you can use '$(unqDyn [| x |])' to refer to any expression 'x' of type 'Dynamic t a'; the unquoted result will be of type 'a'
qDyn :: Q Exp -> Q Exp
qDyn qe = do
  e <- qe
  let f :: forall d. Data d => d -> StateT [(Name, Exp)] Q d
      f d = case eqT of
        Just (Refl :: d :~: Exp)
          | AppE (VarE m) eInner <- d
          , m == 'unqMarker
          -> do n <- lift $ newName "dyn"
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
