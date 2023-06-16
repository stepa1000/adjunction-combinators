{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Data where

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
import Data.Dynamic
import Data.Function
import Data.Functor.Adjunction
import Data.Graph.Inductive
import Data.HashMap.Lazy
import Data.List.NonEmpty as NE
import Data.Profunctor.Strong
import Data.Vector as V
import GHC.Generics
import Prelude as Pre

type WAdjData f g a = W.AdjointT f g NonEmpty a

type WAdjDynamic f g = WAdjData f g Dynamic

unionAdjData :: (Adjunction f1 g1, Adjunction f2 g2) => WAdjData f1 g1 a -> WAdjData f2 g2 a -> WAdjData (f2 :.: f1) (g1 :.: g2) a
unionAdjData d1 d2 = hoistWAdj undefined $ d1 @## d2

type MAdjData f g a = M.AdjointT f g Vector a

type MAdjDynamic f g = MAdjData f g Dynamic

type GAdjData f g a = Gr (WAdjData f g a) (MAdjData f g a)

type GAdjDynamic f g = GAdjData f g Dynamic

data FullAdjData f g a
  = FWAdjData (WAdjData f g (FullAdjData f g a))
  | FMAdjData (MAdjData f g (FullAdjData f g a))
  | FGAdjData (GAdjData f g (FullAdjData f g a))
  | FAValue a

type FullAdjDynamic f g = FullAdjData f g Dynamic

type Causes f g w a b = CokleisliAdj f g w (WAdjData f g a) (WAdjData f g b)

type Investigation f g m a b = KleisliAdj f g m (MAdjData f g a) (MAdjData f g b)

-- | Causality is triggered only by bijection of comonadic data into a monadic.
type CausalRelation f g w m a = Gr (Causes f g w a a) (Investigation f g m a a)

-- Gr (Causes f g w (MAdjData f g a) (MAdjData f g a)) (Consequences f g m () () )
