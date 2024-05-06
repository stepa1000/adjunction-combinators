{-# LANGUAGE RankNTypes #-}

module Control.Base.Comonad where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Env
import Control.Core.Composition
import Control.Monad
import Control.Monad.Free
import Control.Monad.Reader as R
import Control.Monad.Trans.Adjoint as M
import Data.CoAndKleisli
import Data.Functor.Adjunction

coask :: (Adjunction f g, Comonad w, Monad g) => W.AdjointT (EnvT e f) (ReaderT e g) w a -> e
coask = extract . mapWAdj ((>> R.ask) . extract)

adjEnv :: Comonad w => e -> w () -> W.AdjointT (Env e) (Reader e) w ()
adjEnv e w = W.AdjointT $ env e $ fmap (pure) w

adjSetEnv :: (Adjunction f g, Monad m) => e -> f a -> M.AdjointT (EnvT e f) (ReaderT e g) m a
adjSetEnv e wa = mapMAdj (const $ return $ EnvT e wa) (return ())

adjSetEnvId :: (Monad m) => e -> M.AdjointT (Env e) (Reader e) m ()
adjSetEnvId e = adjSetEnv e (return ())

adjGetEnv :: (Adjunction f g, Monad m) => M.AdjointT (EnvT e f) (ReaderT e g) m e
adjGetEnv = mapMAdj (\(EnvT e wa) -> return $ EnvT e ((const e) <$> wa)) (return ())

adjModify :: (Adjunction f g, Monad m) => (forall a. a -> f a) -> (e -> e) -> M.AdjointT (EnvT e f) (ReaderT e g) m ()
adjModify point e = do
  a <- adjGetEnv
  adjSetEnv (e a) (point ())
  
{-
coadjFreeTrapez :: Comonad w => W.AdjointT (Free (Env a)) (Cofree (Reader a)) w b -> [w b]
coadjFreeTrapez (W.AdjointT fwg) = f calc fwg
  where
    f :: (Env a (w (Cofree (Reader a) b)) -> [w b]) -> Free (Env a) (w (Cofree (Reader a) b)) -> [w b]
    f g (Free ff) = (\(a, b) -> a ++ b) $ first g $ extract $ fmap ((\(W.Adjoint a)->  )  . flipWM . fmap (f g) . W.AdjointT) ff
    f g (Pure a) = g a
    calc :: Env a (w (Cofree (Reader a) b)) -> [w b]
    calc envWCoFreeReader = flipWM $ extend (foldMap (\r -> [runReader r $ _b b]) . extract) wCoFreeReader
      where
        (b, wCoFreeReader) = runEnv $ envWCoFreeReader

coadjFreeExtract :: Comonad w => W.AdjointT (Free (Env a)) (Cofree (Reader a)) w b -> [b]
coadjFreeExtract = fmap extract . coadjFree
-}
