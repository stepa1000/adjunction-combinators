{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.AI.BPANN where

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

import AI.BPANN
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
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import System.Random
import Prelude as Pre

coadjCreateRandomNetwork :: Comonad w => W.AdjointT (Env Int) (Reader Int) w [Int] -> Network
coadjCreateRandomNetwork = coadjBiparam createRandomNetwork

adjSCreateRandomNetwork :: (Monad m, MonadIO m) => [Int] -> M.AdjointT (Env Int) (Reader Int) m Network
adjSCreateRandomNetwork ll =
  adjState
    ( \s -> do
        s2 <- randomIO
        return $ (createRandomNetwork s ll, s2)
    )

coadjCalculate :: Comonad w => W.AdjointT (Env Network) (Reader Network) w [Double] -> [Double]
coadjCalculate = coadjBiparam calculate
