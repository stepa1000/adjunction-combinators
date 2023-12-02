{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Core.Biparam where

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
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

coadjBiparam :: Comonad w => (a -> c -> b) -> W.AdjointT (Env a) (Reader a) w c -> b
coadjBiparam biparam wa = biparam (coask wa) (extract wa)

adjBiparam :: Monad m => (b -> a -> a) -> b -> M.AdjointT (Env a) (Reader a) m ()
adjBiparam biparam a = adjModify Identity (biparam a)

adjState :: Monad m => (a -> m (c, a)) -> M.AdjointT (Env a) (Reader a) m c
adjState s = do
  a <- adjGetEnv
  (c, a2) <- lift $ s a
  adjSetEnv a2 (pure c)
  
runAdjT :: Monad m => a -> M.AdjointT (Env a) (Reader a) m b -> m (a,b)
runAdjT a (M.AdjointT rme) = runEnv <$> runReader rme a

runAdjTfst :: (Monad m, Functor f, Functor g) 
  => a -> M.AdjointT (Env a :.: f) (g :.: Reader a) m b -> M.AdjointT f g m (a,b)
runAdjTfst a (M.AdjointT rme) = M.AdjointT $ (fmap . fmap) ((\(x,f)->(\y->(x,y)) <$> f) . runEnv . unComp1) $ fmap (`runReader` a) $ unComp1 rme

runAdjTsnd :: (Monad m, Functor f, Functor g) 
  => a -> M.AdjointT (f :.: Env a) (Reader a :.: g) m b -> M.AdjointT f g m (a,b)
runAdjTsnd a (M.AdjointT rme) = M.AdjointT $ (fmap . fmap) (fmap runEnv . unComp1) $ (`runReader` a) $ unComp1 rme
