{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Data.Graph where

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
import Data.Graph.Inductive as Gr
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

coadjMatch :: (Comonad w, Graph gr) => W.AdjointT (Env (gr a b)) (Reader (gr a b)) w Node -> Decomp gr a b
coadjMatch = coadjBiparam (\gr node -> match node gr)

coadjMkGraph :: (Comonad w, Graph gr) => W.AdjointT (Env [LNode a]) (Reader [LNode a]) w [LEdge b] -> gr a b
coadjMkGraph = coadjBiparam mkGraph

adjMerge :: (Monad m, DynGraph gr) => Context a b -> M.AdjointT (Env (gr a b)) (Reader (gr a b)) m ()
adjMerge = adjBiparam (Gr.&)
