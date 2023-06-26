{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Base.Prelude.Control.Biparam where

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
import Language.Haskell.TH
import Prelude as Pre

coadjDecFunD :: Comonad w => W.AdjointT (Env Name) (Reader Name) w [Clause] -> Dec
coadjDecFunD = coadjBiparam FunD

type AdjDataDF = ((((Env [DerivClause] :.: Env [Con]) :.: Env (Maybe Kind)) :.: Env [TyVarBndr ()]) :.: Cxt)
type AdjDataDG = (Reader Cxt :.: (Reader [TyVarBndr ()] :.: (Reader (Maybe Kind) :.: (Reader [Con] :.: Reader [DerivClause]) ) ) )

coadjDecDataD :: Comonad w => W.AdjointT (Env Name :.: AdjDataDF) (AdjDataDG :.: Reader Name) w () -> Dec
coadjDecDataD =  DataD (coask cxtW) (coask nameW) (coask mKindW) (coask lConW) (coask lDerivClauseW)
  where
    ((cxtW, (tyVarBndrW, (mKindW, (lConW, lDerivClauseW)))), nameW)

type AdjNewtypeDF = ((((Env [DerivClause] :.: Env Con) :.: Env (Maybe Kind)) :.: Env [TyVarBndr ()]) :.: Cxt)
type AdjNewtypeDG = (Reader Cxt :.: (Reader [TyVarBndr ()] :.: (Reader (Maybe Kind) :.: (Reader Con :.: Reader [DerivClause]) ) ) )

coadjDecDataD :: Comonad w => W.AdjointT (Env Name :.: AdjDataDF) (AdjDataDG :.: Reader Name) w () -> Dec
coadjDecDataD = NewtypeD (coask cxtW) (coask nameW) (coask mKindW) (coask lConW) (coask lDerivClauseW)
  where
    ((cxtW, (tyVarBndrW, (mKindW, (lConW, lDerivClauseW)))), nameW)

-- type AdjTySynDF = 
-- type AdjTySynDG = 

coadjTySynD :: 
