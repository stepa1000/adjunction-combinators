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

coadjThickCircle :: Comonad w => W.AdjointT (Env Float) (Reader Float) w Float -> Picture
coadjThickCircle = coadjBiparam ThickCircle

coadjColor :: Comonad w => W.AdjointT (Env Picture) (Reader Picture) w Color -> Picture
coadjColor = coadjBiparam (\p c -> Color c p)

adjColor :: Monad m => Color -> M.AdjointT (Env Picture) (Reader Picture) m ()
adjColor = adjBiparam Color

coadjTranslate :: Comonad w => W.AdjointT (Env Picture) (Reader Picture) w (Float, Float) -> Picture
coadjTranslate = coadjBiparam (\p (x, y) -> Translate x y p)

adjTranslate :: Monad m => (Float, Float) -> M.AdjointT (Env Picture) (Reader Picture) m ()
adjTranslate = adjBiparam (\(x, y) p -> Translate x y p)

coadjRotate :: Comonad w => W.AdjointT (Env Picture) (Reader Picture) w Float -> Picture
coadjRotate = coadjBiparam (\pic r -> Rotate r pic)

adjRotate :: Monad m => Float -> M.AdjointT (Env Picture) (Reader Picture) m ()
adjRotate = adjBiparam Rotate
