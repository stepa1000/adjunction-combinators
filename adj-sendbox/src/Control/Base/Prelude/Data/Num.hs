{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Prelude.Data.Num where

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
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import GHC.Real
import Prelude as Pre

coadjPlus :: (Num a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> a
coadjPlus = coadjBiparam (+)

adjPlus :: (Num a, Monad m) => a -> M.AdjointT (Env a) (Reader a) m ()
adjPlus = adjBiparam (+)

coadjMinus :: (Num a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> a
coadjMinus = coadjBiparam (-)

adjMinus :: (Num a, Monad m) => a -> M.AdjointT (Env a) (Reader a) m ()
adjMinus = adjBiparam (-)

-- GHC.Real

coadjquot :: (Integral a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> a
coadjquot = coadjBiparam quot

adjquot :: (Integral a, Monad m) => a -> M.AdjointT (Env a) (Reader a) m ()
adjquot = adjBiparam quot

coadjrem :: (Integral a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> a
coadjrem = coadjBiparam rem

adjrem :: (Integral a, Monad m) => a -> M.AdjointT (Env a) (Reader a) m ()
adjrem = adjBiparam rem

coadjdiv :: (Integral a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> a
coadjdiv = coadjBiparam div

adjdiv :: (Integral a, Monad m) => a -> M.AdjointT (Env a) (Reader a) m ()
adjdiv = adjBiparam div

coadjmod :: (Integral a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> a
coadjmod = coadjBiparam mod

adjmod :: (Integral a, Monad m) => a -> M.AdjointT (Env a) (Reader a) m ()
adjmod = adjBiparam mod

coadjQuotRem :: (Integral a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> (a, a)
coadjQuotRem = coadjBiparam quotRem

coadjDivMod :: (Integral a, Comonad w) => W.AdjointT (Env a) (Reader a) w a -> (a, a)
coadjDivMod = coadjBiparam divMod
