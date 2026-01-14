{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Data.Geometry.Intersection where

-- import qualified Control.Category as Cat

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

import Control.Applicative
import Control.Arrow
import Control.Base.Comonad
import Control.Base.Linear.Metric
import Control.Core.Biparam
import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Core.Composition
import Control.Monad
import Control.Monad.Co
import Control.Monad.Reader as R
import Control.Monad.Trans
import Control.Monad.Trans.Adjoint as M
import Data.Bitraversable
import Data.Bool
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import Linear
import Prelude as Pre

type AdjInRadiusF t a = Env (t a) :.: Env a

type AdjInRadiusG t a = Reader a :.: Reader (t a)

coadjInRadius :: (Comonad w, Metric t, Floating a, Ord a) => W.AdjointT (AdjInRadiusF t a) (AdjInRadiusG t a) w (t a) -> Bool
coadjInRadius w = distance (coask wt) (extract w) < (coask wa)
  where
    (wa, wt) = unCompSysAdjComonad w

type AdjInProjectedF t a = Env (t a) :.: Env (t a)

type AdjInProjectedG t a = Reader (t a) :.: Reader (t a)

coadjInProject :: (Comonad w, Metric t, Floating a, Ord a) => W.AdjointT (AdjInProjectedF t a) (AdjInProjectedG t a) w (t a) -> Bool
coadjInProject w = (inProject $ extend coadjProject wt1) && (inProject $ extend coadjProject wt2)
  where
    inProject :: (Comonad w, Metric t, Floating a, Ord a) => W.AdjointT (Env (t a)) (Reader (t a)) w (t a) -> Bool
    inProject wtp = coadjqd wtp < (quadrance $ coask wtp)
    (wt1, wt2) = unCompSysAdjComonad w

{-
coadjIntersectProject ::
  (Comonad w, Metric t, Floating a, f) =>
  W.AdjointT (AdjInProjectedF t a) (AdjInProjectedG t a) w (f (t a)) ->
  W.AdjointT (AdjInProjectedF t a) (AdjInProjectedG t a) w (f (t a)) ->
  Bool
coadjIntersectProject w1 w2 = undefined
-}
--  where
--    ww1 = fmap (const $ extract w2) w1
