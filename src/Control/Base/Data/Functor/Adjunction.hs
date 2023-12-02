{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Data.Functor.Adjunction where

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
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

coadjZipR :: (Adjunction f u, Comonad w) => W.AdjointT (Env (u a)) (Reader (u a)) w (u a) -> u (a, a)
coadjZipR = coadjBiparam (\a b -> zipR (a, b))

adjZipR :: (Adjunction f u, Semigroup a, Monad m) => u a -> M.AdjointT (Env (u a)) (Reader (u a)) m ()
adjZipR = adjBiparam (\a b -> (\(c, d) -> c <> d) <$> zipR (a, b))

adjStateZipR :: (Adjunction f u, Monad m) => u b -> M.AdjointT (Env (u a)) (Reader (u a)) m (u (b, a))
adjStateZipR ub = adjState (\ua -> return $ (zipR (ub, ua), ua))

coadjZapWithAdjunction :: (Adjunction f u, Comonad w) => (a -> b -> c) -> W.AdjointT (Env (u a)) (Reader (u a)) w (f b) -> c
coadjZapWithAdjunction f = coadjBiparam (zapWithAdjunction f)
