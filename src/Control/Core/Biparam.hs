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
import Data.Distributive
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
runAdjTfst a (M.AdjointT rme) = M.AdjointT $ 
  (fmap . fmap) ((\(x,f)->(\y->(x,y)) <$> f) . runEnv . unComp1) $ 
  fmap (`runReader` a) $ 
  unComp1 rme

subAdjSnd :: 
  ( Monad m, Functor f, Functor g ,Functor f1, Functor g1,
    Adjunction f g, Adjunction f1 g1
  ) 
  => 
  M.AdjointT (f1 :.: f) (g :.: g1) m b -> 
  M.AdjointT f1 g1 (M.AdjointT f g m) b
subAdjSnd (M.AdjointT rme) =
  M.AdjointT $
  fmap M.AdjointT $
  (fmap . fmap . fmap) ((\f1f-> fmap (\_->fmap (extractL) f1f) $ extractL f1f) . unComp1) $
  distribute $
  unComp1 rme

subAdjFst :: 
  ( Monad m, Functor f, Functor g ,Functor f1, Functor g1,
    Adjunction f g, Adjunction f1 g1
  ) 
  => 
  M.AdjointT (f1 :.: f) (g :.: g1) m b -> 
  M.AdjointT f g (M.AdjointT f1 g1 m) b
subAdjFst (M.AdjointT rme) =
  M.AdjointT $
  fmap M.AdjointT $
  (fmap . fmap . fmap) unComp1 $
  unComp1 rme

runAdjTsnd :: (Monad m, Functor f, Functor g) 
  => a -> M.AdjointT (f :.: Env a) (Reader a :.: g) m b -> M.AdjointT f g m (a,b)
runAdjTsnd a (M.AdjointT rme) = M.AdjointT $ (fmap . fmap) (fmap runEnv . unComp1) $ (`runReader` a) $ unComp1 rme

createCoadj :: Comonad w => w a -> W.AdjointT (Env a) (Reader a) w ()
createCoadj wa = W.AdjointT $ env (extract wa) $ fmap (const (return ())) wa
