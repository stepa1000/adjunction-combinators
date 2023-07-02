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
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import System.Random
import Prelude as Pre

-- | Left adjoint for initial network monad.
type AdjInitNetworkF = Env Int :.: Env Network

-- | Right adjoint for initial network monad.
type AdjInitNetworkG = Reader Network :.: Env Int

-- *** Wrap of network fuctions to comonadic and monadic adjoints.

-- | The layer can be generated randomly by key or configured by the composition
-- of the comonads separately. Free adjoint will allow you to generated different network at once
-- with different keus and different layers, but with the same input/output layers, or vice versa.
coadjCreateRandomNetwork :: Comonad w => W.AdjointT (Env Int) (Reader Int) w [Int] -> Network
coadjCreateRandomNetwork = coadjBiparam createRandomNetwork

-- | The monadic net generator only uses the seed and updates.
adjSCreateRandomNetwork :: (Monad m, MonadIO m) => [Int] -> M.AdjointT (Env Int) (Reader Int) m Network
adjSCreateRandomNetwork ll =
  adjState
    ( \s -> do
        s2 <- randomIO
        return $ (createRandomNetwork s ll, s2)
    )

-- | Data can be configured by the composition of comonads. Which allow you to calculate the
-- incoming data through Trace, Free a lot of networks, and then compare and use it to train
-- the classifier.
coadjCalculate :: Comonad w => W.AdjointT (Env Network) (Reader Network) w [Double] -> [Double]
coadjCalculate = coadjBiparam calculate

-- | network calculation without change.
adjSCalculate :: Monad m => [Double] -> M.AdjointT (Env Network) (Reader Network) m [Double]
adjSCalculate dataList = adjState $ \net -> return (calculate net dataList, net)

-- | network calculation from data. Network calculate several data at once,
-- according to Free.
adjSReCalculate :: Monad m => Network -> M.AdjointT (Env [Double]) (Reader [Double]) m ()
adjSReCalculate net = adjState $ \dataList -> return ((), calculate net dataList)
