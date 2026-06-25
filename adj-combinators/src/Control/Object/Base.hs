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

 

adjObjCombE ::  
   ( Comonad w
   , Adjunction fo1 go1, Adjunction fv1 gv1
   , Adjunction fo2 go2, Adjunction fv2 gv2) =>
   [Bool] ->
   (AdjObject w fo1 go1 fv1 gv1) ->
   (AdjObject w fo2 go2 fv2 gv2) ->
   AdjObject w (fo1 :+: fo2) (go1 :*: go2) (fv1 :+: fv2) (gv1 :*: gv2)
adjObjCombE [] (wv1 :< wo1) w2 = let
   wv = (adjCombW $ Left wv1) 
   wo = (adjCombW $ Left wo1)
   in (void wv) :< (fmap (const $ adjObjCombE [] (extract wo1) w2) wo)
adjObjCombE (True:lb) (wv1 :< wo1) w2 = let
   wv = (adjCombW $ Left wv1) 
   wo = (adjCombW $ Left wo1)
   in (void wv) :< (fmap (const $ adjObjCombE lb (extract wo1) w2 ) wo)
adjObjCombE (False:lb) w1 (wv2 :< wo2) = let
   wv = (adjCombW $ Right wv2) 
   wo = (adjCombW $ Right wo2)
   in (void wv) :< (fmap (const $ adjObjCombE lb w1 (extract wo2)) wo)

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
par1Objetc :: 
   w () -> 
   AdjObject w Par1 Par1 Par1 Par1 
par1Object w = unfold (\wx-> (W.AdjointT $ Par1 $ fmap Par1 w , W.AdjointT $ Par1 $ fmap (Par1 . Par1Object) w ) ) w

idObjetc :: 
   w () -> 
   AdjObject w Identity Identity Identity Identity 
idObject w = unfold (\wx-> (W.AdjointT $ Identity $ fmap Identity w , W.AdjointT $ Identity $ fmap (Identity . idObject) w ) ) w

vuObject ::
   w () -> 
   AdjObject w V1 U1 V1 U1
vuObject w = unfold (\wx-> (V1 , V1 ) ) w

viewObject :: (Comonad w, Adjunction fo go, Adjunction fv gv)
   (W.AdjointT fo go w () -> (W.AdjointT fv gv w (), W.AdjointT fo go w ())) ->
   W.AdjointT fo go w () ->
   AdjObject w fo go fv gv ->
   --AdjObject w fo go (fv2 :.: fv) (gv :.: gv2)
viewObject f w = unfold ((\(x,y)->(x,dublicate y)) . f) w

reviewObject :: (Comonad w, Adjunction fo go, Adjunction fv gv)
   ((W.AdjointT fv gv w (), W.AdjointT fo go w ()) -> (W.AdjointT fv gv w (), W.AdjointT fo go w ())) ->
   AdjObject w fo go fv gv ->
   AdjObject w fo go fv gv ->
   --AdjObject w fo go (fv2 :.: fv) (gv :.: gv2)
reviewObject f (a :< w) = ((\(x,y)->(x :< fmap (const $ reviewObject f $ extract w) y)) $ f (a, (void w)))

upviewObject :: (Comonad w, Adjunction fo go, Adjunction fv gv)
   ((W.AdjointT fv gv w (), W.AdjointT fo go w ()) -> (W.AdjointT fv gv w (), W.AdjointT fo go w ())) ->
   AdjObject w fo go fv gv ->
   AdjObject w fo go fv gv ->
   --AdjObject w fo go (fv2 :.: fv) (gv :.: gv2)
upviewObject f (a :< w) = viewObject (\z-> f (a,z)) (void w)--((\(x,y)->(x :< fmap (const $ viewObject (\z-> f ()) $ extract w) y)) $ f (a, (void w)))

--data SystemF fv gv fc gc

type System m w fo go fv gv = FreeT (W.AdjointT fv gv w) m (AdjObject w fo go fv gv)

returnSys :: (AdjObject w fo go fv gv) -> System m w fo go fv gv
returnSys (a :< o) = FreeT $ return $ Free $ fmap (const $ returnSys $ extract o) a

viewSystem :: (Comonad w, Monad m 
   , Adjunction fo go, Adjunction fv gv)
   (W.AdjointT fo go w () -> m (W.AdjointT fv gv w (), W.AdjointT fo go w ()))
   W.AdjointT fo go w () -> 
   System m w fo go fv gv
viewSystem f w = unfoldM (FreeT . fmap (\(x,y)-> Free $ fmap (const $ return (x,y)) x) . (fmap (\(x,y)->(x,dublicate y))) . f) w

reviewSystem :: 
   ( Comonad w, Monad m 
   , Adjunction fo go, Adjunction fv gv)
   ((W.AdjointT fv gv w (), W.AdjointT fo go w ()) -> m (W.AdjointT fv gv w (), W.AdjointT fo go w ()))
   AdjObject w fo go fv gv ->
   System m w fo go fv gv
reviewSystem f (a :< w) = (join $ fmap (\(x,y)-> sequence (x :< sequence fmap (const $ reviewSystem f $ extract w) y)) $ f (a, (void w)))

upviewSystem :: 
   ( Comonad w, Monad m 
   , Adjunction fo go, Adjunction fv gv)
   ((W.AdjointT fv gv w (), W.AdjointT fo go w ()) -> m (W.AdjointT fv gv w (), W.AdjointT fo go w ()))
   AdjObject w fo go fv gv ->
   System m w fo go fv gv
upviewSystem f (a :< w) = viewSystem (\z-> f (a,z)) (void w)

extractSystem ::(Comonad w, Monad m 
   , Adjunction fo go, Adjunction fv gv) =>
   System m w fo go fv gv -> m (AdjObject w fo go fv gv)
extractSysteme (FreeT msys) = do
   sys <- msys
   case sys of
      (Pure a) -> return a
      (Free fb) -> extractSysteme $ extrac fb

systemComp :: 
   ( ComonadApply w, Monad m
   , Adjunction fo1 go1, Adjunction fv1 gv1
   , Adjunction fo2 go2, Adjunction fv2 gv2) =>
   (m x -> m y -> m (x,y))
   System m w fo1 go1 fv1 gv1 ->
   System m w fo2 go2 fv2 gv2 ->
   System m w (fo2 :.: fo1) (go1 :.: go2) (fv2 :.: fv1) (gv1 :.: gv2)
systemComp f (FreeT msys1) (FreeT msys2) = FreeT $ do
   (sys1,sys2) <- f msys1 msys2
   case (sys1,sys2) of
      (Pure a, Pure b) -> retrun $ Pure $ adjObjComp a b
      (Free fa, Free fb) -> do
         retrun $ Free $ fmap (\(x,y)-> systemComp f x y) $ adjObjComp fa fb
      (Free fa, Pure b) -> do
         a $ extractSysteme $ extract fa
         retrun $ Pure $ adjObjComp a b
      (Pure a, Free fb) -> do
         b $ extractSysteme $ extract fb
         retrun $ Pure $ adjObjComp a b

systemComb' :: 
   ( ComonadApply w, Monad m
   , Adjunction fo1 go1, Adjunction fv1 gv1
   , Adjunction fo2 go2, Adjunction fv2 gv2) =>
   [Bool] -> 
   (m x -> m y -> m (Either x y))
   System m w fo1 go1 fv1 gv1 ->
   System m w fo2 go2 fv2 gv2 ->
   System m w (fo1 :+: fo2) (go1 :*: go2) (fv1 :+: fv2) (gv1 :*: gv2)
systemComb' lb f s1@(FreeT msys1) s2@(FreeT msys2) = FreeT $ do
   esys <- f msys1 msys2
   case esys of
      (Left (Free fx)) -> return $ Free $ fmap (\x-> systemComb' (True:lb) x s2) $ adjCombW $ Left fx
      (Right (Free fy)) -> return $ Free $ fmap (\y-> systemComb' (False:lb) s1 y) $ adjCombW $ Right fy
      (Left (Pure x)) -> do
         y <- extractSysteme s2
         return $ Pure $ adjObjCombE (reverse lb) x y
      (Right (Pure y)) -> do
         x <- extractSysteme s1
	 return $ Pure $ adjObjCombE (reverse lb) x y
      
systemComb = systemComb' []

systemDay :: 
   ( ComonadApply w, Monad m
   , Adjunction fo1 go1, Adjunction fv1 gv1
   , Adjunction fo2 go2, Adjunction fv2 gv2) =>
   (m x -> m y -> m (x, y))
   System m w fo1 go1 fv1 gv1 ->
   System m w fo2 go2 fv2 gv2 ->
   System m w (Day fo1 fo2) (Day go1 go2) (Day fv1 fv2) (Day gv1 gv2)
systemDay f (FreeT msys1) (FreeT msys2) = FreeT $ do
   (sys1,sys2) <- f msys1 msys2
   case (sys1,sys2) of
      (Pure a, Pure b) -> retrun $ Pure $ adjObjDay a b
      (Free fa, Free fb) -> do
         retrun $ Free $ fmap (\(x,y)-> systemComp f x y) $ adjObjDay fa fb
      (Free fa, Pure b) -> do
         a $ extractSysteme $ extract fa
         retrun $ Pure $ adjObjDay a b
      (Pure a, Free fb) -> do
         b $ extractSysteme $ extract fb
         retrun $ Pure $ adjObjDay a b

-- Classes

class (Adjunction f g, ComonadApply w) => AdjHas f g w e where
   adjGet :: W.AdjointT f g w b -> e
   -- adjGet = coask
   adjSet :: w e -> W.AdjointT f g w ()

adjSetE :: AdjHas f g w e => e -> w b -> W.AdjointT f g w ()
adjSetE e w = adjSet $ fmap (const e) w

instance AdjHas (EnvT e Identity) (ReaderT e Identity) w e where
   adjGet = coask
   adjSet we = void $ adjEnv (extract we) we

instance (AdjHas f1 g1 w a, AdjHas f2 g2 w b) => AdjHas (f2 :.: f1) (g1 :.: g2) w (a, b) where
   adjGet = (\(x,y)-> (adjGet x, adjGet y)) . unCompSysAdjComonad
   adjSet we = void $ (adjSet $ fmap fst we) @## (adjSet $ fmap snd we)

instance (AdjHas f1 g1 w a, AdjHas f2 g2 w b) => AdjHas (f1 :+: f2) (g1 :*: g2) w (Either a b) where
   adjGet = f . adjWunSumProd
      where
         f (Left w) = Left $ adjGet w
	 f (Right w) = Right $ adjGet w
   adjSet we = void $ f $ extract we
      where
         f (Left w) = adjCombW $ Left $ adjSet $ fmap (\(Left a) -> a) we
         f (Right w) = adjCombW $ Right $ adjSet $ fmap (\(Right a) -> a) we
	 
instance (AdjHas f1 g1 w a, AdjHas f2 g2 w b) => AdjHas (Day f1 f2) (Day g1 g2) w (a, b) where
   adjGet = (\(x,y)-> (adjGet x, adjGet y)) . adjUnDay
   adjSet we = void $ adjDayW (adjSet $ fmap fst we) (adjSet $ fmap snd we)


-- newtype TimeTick = TimeTick Integer

newtype DeltaTick p = DeltaTick Integer deriving
 
newtype Tick p = Tick Integer deriving

type HasTick p f g w = AdjHas f g w (Tick p)-- (EnvT (Tick p) Identity) (ReaderT (Tick p) Identity) w (Tick p)

type HasDeltaTick p f g w = AdjHas f g w (DeltaTick p)--(EnvT (DeltaTick p) Identity) (ReaderT (DeltaTick p) Identity) w (DeltaTick p)

type HasDTT p fo1 go1 fo2 go2 fv gv w = (HasTick p fo1 go1 w, HasTick p fv gv w, HasDeltaTick p fo2 go2 w)

type HasDTT2 p f g w = AdjHas f g w (DeltaTick p, Tick p)

initTick :: (HasTick p fo1 go1 w, HasTick p fv gv w, HasDeltaTick p fo2 go2 w) => Proxu p -> w Integer -> AdjObject w (fo2 :.: fo1) (go1 :.: go2) fv gv
initTick (Proxy :: Proxy p) wi = viewObject (\w-> f w ) (adjSet $ fmap (\i-> (DeltaTick @p i , Tick @p 0) ) wi)
   where 
      f w = let
         w' = lower w
         (DeltaTick dt, Tick t) = adjGet w
	 in if t >= dt then (adjSet $ fmap (const (Tick 0)) w', adjSet $ fmap (const (DeltaTick dt, Tick 0)) w' ) 
	    else (adjSet $ fmap (const (Tick $ t + 1)) w', adjSet $ fmap (const (DeltaTick dt, Tick $ t + 1)) w' )  

type Updater a = Updater (a -> a) deriving

type UpdaterType a = UpdaterType (TypeRep (Update a)) deriving

type HasUpdater a f g w = AdjHas f g w (Updater a)

type HasUpdateType a f g w = AdjHas f g w (UpdaterType a)

type HasUT a fo go fv gv w = (Typeable a, HasUpdater a fo go w, HasUpdateType a fv gv w)

initUpdater :: (HasUpdater a fo go w, HasUpdateType a f g w) => w (a -> a) -> AdjObject w fo go fv gv
initUpdater (we :: w (a -> a)) = viewObject f (adjSet $ fmap (\e-> Updater e) wa)
   where
      f w = let
         (Update up) = adjGet w
	 in (adjSetE (typeOf $ Update up) (lower w) , adjSetE (Update up) (lower w))

appUpdater :: 
   ( HasUpdater a fo go w, HasUpdateType a fv gv w
   , AdjHas fo2 go2 w a, Adjunction fv2 gv2) => 
   AdjObject w fo go fv gv -> AdjObject w fo2 go2 fv2 gv2 ->
   AdjObject w (fo2 :.: fo) (go :.: go2) (fv2 :.: fv) (gv :.: gv2)
appUpdater w1 w2 = upviewObject f $ adjObjComp w1 w2
   where
      f (wa,wo) = let
         (Update up, a) = adjGet wo
	 in (wa , adjSetE (Update up, up a) (lower wo))

appUpdaterReV :: 
   ( HasUpdater a fo go w, HasUpdateType a fv gv w
   , AdjHas fo2 go2 w a, Adjunction fv2 gv2) => 
   AdjObject w fo go fv gv -> AdjObject w fo2 go2 fv2 gv2 ->
   AdjObject w (fo2 :.: fo) (go :.: go2) (fv2 :.: fv) (gv :.: gv2)
appUpdaterRev w1 w2 = reviewObject f $ adjObjComp w1 w2
   where
      f (wa,wo) = let
         (Update up, a) = adjGet wo
	 in (wa , adjSetE (Update up, up a) (lower wo))
