{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ** Cofree
--
--  uses for working with recursion in adjoint.
module Control.Base.Control.Comonad.Cofree where

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
import Control.Core.Biparam
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Monad
import Control.Monad.Co
import Control.Monad.Reader as R
import Control.Monad.Trans
import Control.Monad.Trans.Adjoint as M
import Data.Bitraversable
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

adjState_extract :: Monad m => (a -> m a) -> M.AdjointT (Env (Cofree g a)) (Reader (Cofree g a)) m ()
adjState_extract f = adjState $ \c -> (\a -> ((), a)) <$> _extract f c

adjS_unwrap :: Monad m => (g (Cofree g a) -> m (g (Cofree g a))) -> M.AdjointT (Env (Cofree g a)) (Reader (Cofree g a)) m ()
adjS_unwrap f = adjState $ \c -> (\a -> ((), a)) <$> _unwrap f c

adjStelescoped ::
  Monad m =>
  [(Cofree g a -> m (Cofree g a)) -> g (Cofree g a) -> m (g (Cofree g a))] ->
  (a -> m a) ->
  M.AdjointT (Env (Cofree g a)) (Reader (Cofree g a)) m ()
adjStelescoped f g = adjState $ \c -> (\a -> ((), a)) <$> telescoped f g c

adjSshoots :: (Monad m, Traversable g) => (a -> m a) -> M.AdjointT (Env (Cofree g a)) (Reader (Cofree g a)) m ()
adjSshoots f = adjState $ \c -> (\a -> ((), a)) <$> shoots f c

adjSleaves :: (Monad m, Traversable g) => (a -> m a) -> M.AdjointT (Env (Cofree g a)) (Reader (Cofree g a)) m ()
adjSleaves f = adjState $ \c -> (\a -> ((), a)) <$> leaves f c
