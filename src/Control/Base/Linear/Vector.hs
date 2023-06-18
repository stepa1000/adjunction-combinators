{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Linear.Vector where

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

coadjAdd :: (Num a, Comonad w, Additive f) => W.AdjointT (Env (f a)) (Reader (f a)) w (f a) -> f a
coadjAdd = coadjBiparam (^+^)

adjAdd :: (Num a, Additive f, Monad m) => f a -> M.AdjointT (Env (f a)) (Reader (f a)) m ()
adjAdd = adjBiparam (^+^)

coadjDiffV :: (Num a, Comonad w, Additive f) => W.AdjointT (Env (f a)) (Reader (f a)) w (f a) -> f a
coadjDiffV = coadjBiparam (^-^)

adjDiffV :: (Num a, Additive f, Monad m) => f a -> M.AdjointT (Env (f a)) (Reader (f a)) m ()
adjDiffV = adjBiparam (^-^)

coadjLerp :: (Num a, Comonad w, Additive f) => a -> W.AdjointT (Env (f a)) (Reader (f a)) w (f a) -> f a
coadjLerp l = coadjBiparam (lerp l)

adjLerp :: (Num a, Additive f, Monad m) => a -> f a -> M.AdjointT (Env (f a)) (Reader (f a)) m ()
adjLerp l = adjBiparam (lerp l)

coadjLiftU2 :: (Comonad w, Additive f) => (a -> a -> a) -> W.AdjointT (Env (f a)) (Reader (f a)) w (f a) -> f a
coadjLiftU2 f = coadjBiparam (liftU2 f)

adjLiftU2 :: (Additive f, Monad m) => (a -> a -> a) -> f a -> M.AdjointT (Env (f a)) (Reader (f a)) m ()
adjLiftU2 f = adjBiparam (liftU2 f)

coadjLiftI2 :: (Comonad w, Additive f) => (a -> b -> c) -> W.AdjointT (Env (f a)) (Reader (f a)) w (f b) -> f c
coadjLiftI2 f = coadjBiparam (liftI2 f)
