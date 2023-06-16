{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Prelude.Data.Bool where

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
import Data.Bool
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

coadjAnd :: Comonad w => W.AdjointT (Env Bool) (Reader Bool) w Bool -> Bool
coadjAnd wb = coask wb && extract wb

adjAnd :: Monad m => Bool -> M.AdjointT (Env Bool) (Reader Bool) m ()
adjAnd = adjBiparam (&&)

coadjOr :: Comonad w => W.AdjointT (Env Bool) (Reader Bool) w Bool -> Bool
coadjOr wb = coask wb || extract wb

adjOr :: Monad m => Bool -> M.AdjointT (Env Bool) (Reader Bool) m ()
adjOr = adjBiparam (||)

coadjBool :: Comonad w => Bool -> W.AdjointT (Env a) (Reader a) w a -> a
coadjBool b = coadjBiparam (\a d -> bool a d b)

adjBool :: Monad m => Bool -> a -> M.AdjointT (Env a) (Reader a) m ()
adjBool b = adjBiparam (\a d -> bool a d b)

coadjBoolE :: Comonad w => a -> W.AdjointT (Env Bool) (Reader Bool) w a -> a
coadjBoolE b = coadjBiparam (\a d -> bool b d a)

adjBoolE :: Monad m => a -> a -> M.AdjointT (Env Bool) (Reader Bool) m ()
adjBoolE b = adjBiparam (\a d -> bool b a d)
