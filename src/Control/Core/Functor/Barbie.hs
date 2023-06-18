{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Core.Functor.Barbie where

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

bCompAdjM ::
  (ApplicativeB b, Adjunction f1 g1, Adjunction f2 g2, Monad m) =>
  (forall a. (a, a) -> M.AdjointT (f2 :.: f1) (g1 :.: g2) m a) ->
  b (M.AdjointT f1 g1 m) ->
  b (M.AdjointT f2 g2 m) ->
  b (M.AdjointT (f2 :.: f1) (g1 :.: g2) m)
bCompAdjM f b1 b2 = bmap (\(Pair adj1 adj2) -> (adj1 $## adj2) >>= f) $ bprod b1 b2

bCombAdjM ::
  (ApplicativeB b, Adjunction f1 g1, Adjunction f2 g2, Monad m) =>
  (forall a. (a, a) -> M.AdjointT (f2 :.: f1) (g1 :.: g2) m a) ->
  b (M.AdjointT f1 g1 m) ->
  b (M.AdjointT f2 g2 m) ->
  b (M.AdjointT (f1 :+: f2) (g1 :*: g2) m)
bCombAdjM f b1 b2 = bmap (\(Pair adj1 adj2) -> (adj1 $+* adj2) >>= f) $ bprod b1 b2
