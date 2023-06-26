{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Prelude.Control where

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
import Control.Base.Prelude.Data.Ord
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
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game
import Prelude as Pre

data EventF f
  = EventKeyF (KeyF f) (f KeyState) (f Modifiers) (f (Float, Float))
  | EventMotionF (f (Float, Float))
  | EventResizeF (f (Int, Int))

data KeyF f
  = CharF (f Char)
  | SpecialKeyF (f SpecialKey)
  | MouseButtonF (f MouseButton)

coadjEventKeyChar :: Comonad w => EventF (W.AdjointT (Env Char) (Reader Char) w) -> Bool
coadjEventKeyChar (EventKeyF (CharF w) _ _ _) = coadjEq w
coadjEventKeyChar _ = False

adjEvent :: Monad m => EventF (M.AdjointT (Env Bool) (Reader Bool) m) -> M.AdjointT (Env Bool) (Reader Bool) m (EventF (M.AdjointT (Env Bool) (Reader Bool) m))
adjEvent = undefined -- btraverse
