{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Data.Game.Damage.Direct where

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
import Control.Base.Data.Tag
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
import Data.Coerce
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

newtype DirectDamage a = DirectDamage {unDirectDamage :: a} deriving (Num)

newtype DirectHealth a = DirectHealth {unDirectHealth :: a} deriving (Num)

adjSDamage :: (Num a, Monad m) => DirectDamage a -> M.AdjointT (Env (DirectHealth a)) (Reader (DirectHealth a)) m ()
adjSDamage dd = adjState $ \dh -> return $ ((), dh - (coerce dd))
