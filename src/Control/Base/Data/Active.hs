{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Data.Active where

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
import Data.Active
import Data.Base.Comonad
import Data.Bitraversable
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Functor.Identity
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

coadjMkEra :: Comonad w => W.AdjointT (Env (Time a)) (Reader (Time a)) w (Time a) -> Era a
coadjMkEra = coadjBiparam mkEra

adjShiftDynamic :: Monad m => Duration Rational -> M.AdjointT (Env (Dynamic a)) (Reader (Dynamic a)) m ()
adjShiftDynamic = adjBiparam shiftDynamic

coadjMkActiveStart :: Comonad w => (Time Rational -> a) -> W.AdjointT (Env (Time Rational)) (Reader (Time Rational)) w (Time Rational) -> Active a
coadjMkActiveStart f = coadjBiparam (\a b -> mkActive a b f)

coadjRunActive :: Comonad w => W.AdjointT (Env (Time a)) (Reader (Time a)) w (Active a) -> a
coadjRunActive = coadjBiparam (\a b -> runActive b a)

adjSetEra :: Monad m => Era Rational -> M.AdjointT (Env (Active a)) (Reader (Active a)) m ()
adjSetEra = adjBiparam setEra

adjAtTime :: Monad m => Time Rational -> M.AdjointT (Env (Active a)) (Reader (Active a)) m ()
adjAtTime = adjBiparam atTime
