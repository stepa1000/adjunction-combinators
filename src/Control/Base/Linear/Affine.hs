{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Linear.Affine where

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

coadjDiff :: (Comonad w, Num a, Affine t) => W.AdjointT (Env (t a)) (Reader (t a)) w (t a) -> Diff t a
coadjDiff = coadjBiparam (.-.)

adjAddPoint :: (Monad m, Num a, Affine t) => Diff t a -> M.AdjointT (Env (t a)) (Reader (t a)) m ()
adjAddPoint = adjBiparam (\d v -> v .+^ d)

adjSubPoint :: (Monad m, Num a, Affine t) => Diff t a -> M.AdjointT (Env (t a)) (Reader (t a)) m ()
adjSubPoint = adjBiparam (\d v -> v .-^ d)

coadjqdA :: (Comonad w, Num a, Affine t) => W.AdjointT (Env (t a)) (Reader (t a)) w (t a) -> a
coadjqdA = coadjBiparam qdA

coadjDistanceA :: (Comonad w, Num a, Affine t) => W.AdjointT (Env (t a)) (Reader (t a)) w (t a) -> a
coadjDistanceA = coadjBiparam distanceA
