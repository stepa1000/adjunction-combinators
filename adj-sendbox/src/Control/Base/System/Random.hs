{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.System.Random where

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
import System.Random
import Prelude as Pre

coadjRandomR :: (Comonad w, RandomGen g, Random a) => W.AdjointT (Env (a, a)) (Reader (a, a)) w g -> (a, g)
coadjRandomR = coadjBiparam randomR

adjSRandomR :: (Monad m, RandomGen g, Random a) => (a, a) -> M.AdjointT (Env g) (Reader g) m a
adjSRandomR p =
  adjState
    ( \g -> return $ randomR p g
    )

adjSRandom :: (Monad m, RandomGen g, Random a) => M.AdjointT (Env g) (Reader g) m a
adjSRandom =
  adjState
    ( \g -> return $ random g
    )
