{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Core.Generic where

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
import Control.Core.Composition
import Control.Monad
import Control.Monad.Co
import Control.Monad.Trans
import Control.Monad.Trans.Adjoint as M
import Data.Either.Extra
import Data.Function
import Data.Functor.Adjunction
import Data.Maybe
import Data.Profunctor.Strong
import Data.Typeable
import GHC.Generics
import Prelude as Pre

{-
liftAdj ::
  (Adjunction f g, Adjunction fg gg, Monad m, Typeable f, Typeable g, Typeable fg, Typeable gg) =>
  M.AdjointT f g m a ->
  M.AdjointT fg gg m a
liftAdj
-}

{-
class (Adjunction f g, Adjunction fg gg) => LiftAdj f g fg gg where
  liftAdj :: (Monad m, Typeable m, Typeable a, Monoid a) => M.AdjointT f g m a -> M.AdjointT fg gg m a

instance
  (Adjunction f g, Adjunction (fg1 :.: fg2) (gg1 :.: gg2), Typeable f, Typeable g, Typeable fg1, Typeable gg1, Typeable fg2, Typeable gg2) =>
  LiftAdj f g (fg1 :.: fg2) (gg1 :.: gg2)
  where
  liftAdj m = fromJust $ cast m <|> return ((\(x, y) -> x <> y) <$> (liftAdj m $## liftAdj m))

instance
  (Adjunction f g, Adjunction fg1 gg1, Adjunction fg2 gg2, Typeable f, Typeable g, Typeable fg1, Typeable gg1, Typeable fg2, Typeable gg2) =>
  LiftAdj f g (fg2 :+: fg1) (gg1 :*: gg2)
  where
  liftAdj m = fromJust $ cast m <|> return (fromEither <$> (liftAdj m $+* liftAdj m))

instance (Adjunction f g, Adjunction fg gg, Typeable f, Typeable g, Typeable fg, Typeable gg) => LiftAdj f g fg gg where
  liftAdj m = fromJust $ cast m <|> (return mempty)
-}

{-  default liftAdj ::
    (Generic1 fg, Generic1 gg, LiftAdj' f g fg gg, Monad m, Typeable f, Typeable g, Typeable fg, Typeable gg) =>
    M.AdjointT f g m a ->
    M.AdjointT fg gg m a
  liftAdj = liftAdj'

class (Adjunction f g, Adjunction fg gg) => LiftAdj' f g fg gg where
  liftAdj' ::
    (Generic1 fg, Generic1 gg, LiftAdj' f g fg gg, Monad m, Typeable f, Typeable g, Typeable fg, Typeable gg) =>
    M.AdjointT f g m a ->
    M.AdjointT fg gg m a

instance (Adjunction f g, Adjunction fg1 gg1, Adjunction fg2 gg2) => LiftAdj' f g (fg1 :.: fg2) (gg1 :.: gg2) where
  liftAdj' m = liftAdj' m $## liftAdj' m
-}
