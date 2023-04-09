{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Core.ParticalCompos where

import Control.Arrow
-- import qualified Control.Category as Cat
import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
-- import Control.Invertible.Monoidal.Free as Inv

{-
import Data.Bifunctor.Functor
import Data.Bifunctor.Product
import Data.Bifunctor.Sum
-}

{-
import Data.Profunctor
import Data.Profunctor.Composition
import Data.Proxy
-}

import Control.Core.Composition
import Control.Monad
import Control.Monad.Co
import Control.Monad.Trans
import Control.Monad.Trans.Adjoint as M
import Data.Bitraversable
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

flipA :: (Adjunction f g, Monad m) => f (m a) -> m (f a)
flipA fma = do
  a <- extractL fma
  return $ fmap (const a) fma

(##$) ::
  (Adjunction f1 g1, Adjunction f2 g2, Monad m) =>
  (f1 (m a)) ->
  (f2 (m b)) ->
  m ((f2 :.: f1) (a, b))
(##$) f1 f2 = do
  f1' <- flipA f1
  f2' <- flipA f2
  return $ Comp1 (fmap (\b -> fmap (\a -> (a, b)) f1') f2')

(+*$) ::
  (Adjunction f1 g1, Adjunction f2 g2, Monad m) =>
  (f1 (m a)) ->
  (f2 (m b)) ->
  m [(f1 :+: f2) (a, b)]
(+*$) = undefined

{-
partWAdjG :: (Adjunction f g, Adjunction f1 g1, Adjunction f2 g2, Comonad w) =>
    (forall x y z. (x -> y -> z) -> W.AdjointT f g w x -> W.AdjointT f g w y -> W.AdjointT f g w z) ->
    W.AdjointT f g w (f1 a) ->
    W.AdjointT f g w (f2 a) ->
    W.AdjointT f g w ((f2 :.: f1) a)
  compAdjComonad f (W.AdjointT a1) (W.AdjointT a2) =
-}
