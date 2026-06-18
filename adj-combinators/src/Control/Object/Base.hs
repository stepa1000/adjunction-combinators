{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Object.Base where

import Control.Arrow
-- import qualified Control.Category as Cat
import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
-- import Control.Invertible.Monoidal.Free as Inv
import Control.Monad
-- import Control.Monad.Co
import Control.Monad.Trans
import Control.Monad.Trans.Adjoint as M
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
import Control.Comonad.Cofree
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Env
import Control.Monad
import Control.Monad.Free
import Control.Monad.Reader as R
import Control.Monad.Trans.Adjoint as M
import Data.Bitraversable
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
-- import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

import Control.Core.Composition

type Object wo wv = Cofree wo (wv ()) 

type AdjObject' wo wv fo go fv gv = Object (W.AdjointT fo go wo) (W.AdjointT fv gv wv)

type AdjObject w fo go fv gv = Object (W.AdjointT fo go w) (W.AdjointT fv gv w)

adjObjComp :: 
   ( ComonadApply w
   , Adjunction fo1 go1, Adjunction fv1 gv1
   , Adjunction fo2 go2, Adjunction fv2 gv2) =>
   AdjObject w fo1 go1 fv1 gv1 ->
   AdjObject w fo2 go2 fv2 gv2 ->
   AdjObject w (fo2 :.: fo1) (go1 :.: go2) (fv2 :.: fv1) (gv1 :.: gv2)
adjObjComp (wv1 :< wo1) (wv2 :< wo2) = (wv1 @## wv2) :< (fmap (\(x,y)->adjObjComp x y) $ wo1 @## wo2)

adjObjComb ::  
   ( Comonad w
   , Adjunction fo1 go1, Adjunction fv1 gv1
   , Adjunction fo2 go2, Adjunction fv2 gv2) =>
   AdjObject w fo1 go1 fv1 gv1 ->
   AdjObject w fo2 go2 fv2 gv2 ->
   [AdjObject w (fo1 :+: fo2) (go1 :*: go2) (fv1 :+: fv2) (gv1 :*: gv2)]
adjObjComb (wv1 :< wo1) (wv2 :< wo2) = do
   wv <- (wv1 @+* wv2) 
   wo <- (wo1 @+* wo2)
   return $ (void wv) :< (fmap (const $ adjObjComb (extract wo1) (extract wo2)) wo)
{-
adjObjComb ::  
   ( Comonad w
   , Adjunction fo1 go1, Adjunction fv1 gv1
   , Adjunction fo2 go2, Adjunction fv2 gv2) =>
   AdjObject w fo1 go1 fv1 gv1 ->
   AdjObject w fo2 go2 fv2 gv2 ->
   AdjObject w (Free (fo1 :+: fo2)) (Cofree (go1 :*: go2)) (Free (fv1 :+: fv2)) (Cofree (gv1 :*: gv2))
adjObjComb (wv1 :< wo1) (wv2 :< wo2) = let
   wv = freelyW (wv1 @+* wv2) 
   wo = freelyW (wo1 @+* wo2)
   in (void wv) :< (fmap (const $ adjObjComb (extract wo1) (extract wo2)) wo)
-}
adjObjDay :: 
   ( Comonad w
   , Adjunction fo1 go1, Adjunction fv1 gv1
   , Adjunction fo2 go2, Adjunction fv2 gv2) =>
   AdjObject w fo1 go1 fv1 gv1 ->
   AdjObject w fo2 go2 fv2 gv2 ->
   AdjObject w (Day fo1 fo2) (Day go1 go2) (Day fv1 fv2) (Day gv1 gv2)
adjObjDay (wv1 :< wo1) (wv2 :< wo2) = let
   wv = adjDayW wv1 wv2
   wo = adjDayW wo1 wo2
   in wv :< (fmap (const $ adjObjDay (extract wo1) (extract wo2)) wo)

-- MAPS
--
mapAOW :: 
   (w x -> w2 x) ->
   AdjObject w fo go fv gv ->
   AdjObject w2 fo go fv gv
mapAOW f (a :< b) = (hoistWAdj f a) :< (fmap (mapAOW f) $ hoistWAdj f b)

-- New module needed
--
Par1Objetc :: 
   w () -> 
   AdjObject w Par1 Par1 Par1 Par1 
Par1Object w = unfold (\wx-> (W.AdjointT $ Par1 $ fmap Par1 w , W.AdjointT $ Par1 $ fmap (Par1 . Par1Object) w ) ) w

idObjetc :: 
   w () -> 
   AdjObject w Identity Identity Identity Identity 
idObject w = unfold (\wx-> (W.AdjointT $ Identity $ fmap Identity w , W.AdjointT $ Identity $ fmap (Identity . idObject) w ) ) w

vuObject ::
   w () -> 
   AdjObject w V1 U1 V1 U1
vuObject w = unfold (\wx-> (V1 , V1 ) ) w

viewObject :: 
   (W.AdjointT fo go w () -> (W.AdjointT fv gv w (), W.AdjointT fo go w ())) ->
   W.AdjointT fo go w () ->
   AdjObject w fo go fv gv ->
   --AdjObject w fo go (fv2 :.: fv) (gv :.: gv2)
viewObject f w = unfold ((\(x,y)->(x,dublicate y)) . f) wo

--data SystemF fv gv fc gc

type System m w fo go fv gv = FreeT (W.AdjointT fv gv w) m (AdjObject w fo go fv gv)


