{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Linear.Metric where

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
import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Monad
import Control.Monad.Co
import Control.Monad.Trans
import Control.Monad.Trans.Adjoint as M
import Data.Base.Comonad
import Data.Bitraversable
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import Linear
import Prelude as Pre

coadjDot :: (Comonad w, Metric t, Num a) => W.AdjointT (Env (t a)) (Reader (t a)) w (t a) -> t a
coadjDot = coadjBiparam dot

coadjqd :: (Comonad w, Metric t, Num a) => W.AdjointT (Env (t a)) (Reader (t a)) w (t a) -> a
coadjqd = coadjBiparam qd

coadjDistance :: (Comonad w, Metric t, Floating a) => W.AdjointT (Env (t a)) (Reader (t a)) w (t a) -> a
coadjDistance = coadjBiparam distance
