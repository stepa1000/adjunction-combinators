{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Prelude.Data.Ord where

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
import Control.Base.Prelude.Control.Biparam
import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Monad
import Control.Monad.Co
import Control.Monad.Reader as R
import Control.Monad.Trans
import Control.Monad.Trans.Adjoint as M
import Data.Bitraversable
import Data.Bool
import Data.CoAndKleisli
import Data.Eq
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Ord
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

coadjEq :: (Eq a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> Bool
coadjEq = coadjBiparam (==)

coadjCompare :: (Ord a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> Ordering
coadjCompare = coadjBiparam compare

coadjMax :: (Ord a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> a
coadjMax = coadjBiparam max

adjMax :: (Ord a, Monad m) => a -> M.AdjointT (Env a) (Reader a) m ()
adjMax = adjBiparam max

coadjMin :: (Ord a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> a
coadjMin = coadjBiparam min

adjMin :: (Ord a, Monad m) => a -> M.AdjointT (Env a) (Reader a) m ()
adjMin = adjBiparam min
