module Data.Base.Comonad where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Env
import Control.Core.Composition
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Trans.Adjoint as M

coask :: (Adjunction f g, Comonad w) => W.AdjointT (EnvT e f) (ReaderT e g) w a -> e
coask = mapWAdj ((ask <<) . extract)

adjSetEnv :: (Adjunction f g, Monad m) => e -> w a -> M.AdjointT (EnvT e f) (ReaderT e g) m a
adjSetEnv e wa = mapMAdj (const $ return $ EnvT e wa) (return ())

adjGetEnv :: (Adjunction f g, Monad m) => M.AdjointT (EnvT e f) (ReaderT e g) m e
adjGetEnv = mapMAdj (\(EnvT e wa) -> return $ EnvT e ((const e) <$> wa)) (return ())

adjModify :: (Adjunction f g, Monad m) => (forall a. a -> f a) -> (e -> e) -> M.AdjointT (EnvT e f) (ReaderT e g) m ()
adjModify point e = do
  a <- adjGetEnv
  adjSetEnv (e a) (point ())

coadjFree :: Comonad w => W.AdjointT (Free (Env a)) (Cofree (Reader a)) w b -> [w b]
coadjFree (W.AdjointT fwg) = iterA calc fwg
  where
    calc envWCoFreeReader = flipWM $ extend (foldMap (\r -> [runReader r b]) . extract) wCoFreeReader
      where
        (b, wCoFreeReader) = runEnv envWCoFreeReader

coadjFreeExtract :: Comonad w => W.AdjointT (Free (Env a)) (Cofree (Reader a)) w b -> [b]
coadjFreeExtract = fmap extract . coadjFree
