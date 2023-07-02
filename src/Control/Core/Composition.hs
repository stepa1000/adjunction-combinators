{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Core.Composition where

import Control.Arrow
-- import qualified Control.Category as Cat
import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
-- import Control.Invertible.Monoidal.Free as Inv
import Control.Monad
import Control.Monad.Co
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

import Data.Bitraversable
import Data.CoAndKleisli
import Data.Function
import Data.Functor.Adjunction
import Data.Profunctor.Strong
import GHC.Generics
import Prelude as Pre

-- Combinators for composition and combination adjunctions types or Object and Subject

runMAdj :: Adjunction f g => (a -> M.AdjointT f g m b) -> f a -> m (f b)
runMAdj = rightAdjunct . (M.runAdjointT .)

mapMAdj :: (Adjunction f g, Monad m) => (f a -> m (f b)) -> M.AdjointT f g m a -> M.AdjointT f g m b
mapMAdj f (M.AdjointT gmfa) = M.AdjointT $ fmap (>>= f) gmfa

runWAdj :: Adjunction f g => (W.AdjointT f g w a -> b) -> w (g a) -> g b
runWAdj = leftAdjunct . (. W.AdjointT)

mapWAdj :: (Adjunction f g, Comonad w) => (w (g a) -> g b) -> W.AdjointT f g w a -> W.AdjointT f g w b
mapWAdj f (W.AdjointT fmga) = W.AdjointT $ fmap (extend f) fmga

hoistMAdj :: (Adjunction f g, Monad m) => (m (g a) -> m2 (g b)) -> M.AdjointT f g m a -> M.AdjointT f g m2 b
hoistMAdj = undefined

hoistWAdj :: (Adjunction f g, Comonad w) => (w (g a) -> w2 (g b)) -> W.AdjointT f g w a -> W.AdjointT f g w2 b
hoistWAdj f (W.AdjointT fmga) = W.AdjointT $ fmap f fmga

adjReturnL :: (Adjunction f1 g1, Adjunction f2 g2, Monad m) => M.AdjointT f1 g1 m a -> M.AdjointT (f2 :.: f1) (g1 :.: g2) m a
adjReturnL m = fst <$> (m $## (return ()))

-- for Monad, Comonad

($##) :: (Adjunction f1 g1, Adjunction f2 g2, Monad m) => M.AdjointT f1 g1 m a -> M.AdjointT f2 g2 m b -> M.AdjointT (f2 :.: f1) (g1 :.: g2) m (a, b)
($##) (M.AdjointT a1) (M.AdjointT a2) =
  M.AdjointT $
    (fmap . fmap) Comp1 $
      Comp1 $
        fmap (\x -> fmap ((>>= (\y -> (\y2 -> ((\z -> (\x2 -> (x2, z)) <$> y2) <$> y)) <$> x))) a2) a1

($+*) :: (Adjunction f1 g1, Adjunction f2 g2, Monad m) => M.AdjointT f1 g1 m a -> M.AdjointT f2 g2 m b -> M.AdjointT (f1 :+: f2) (g1 :*: g2) m (Either a b)
($+*) (M.AdjointT a1) (M.AdjointT a2) =
  M.AdjointT $
    ((fmap . fmap) (L1 . fmap Left) a1)
      :*: ((fmap . fmap) (R1 . fmap Right) a2)

(@##) :: (Adjunction f1 g1, Adjunction f2 g2, ComonadApply w) => W.AdjointT f1 g1 w a -> W.AdjointT f2 g2 w b -> W.AdjointT (f2 :.: f1) (g1 :.: g2) w (a, b)
(@##) (W.AdjointT a1) (W.AdjointT a2) =
  W.AdjointT $
    (fmap . fmap) Comp1 $
      Comp1 $
        fmap (\x -> fmap (\y -> liftW2 (\t h -> fmap (\a -> fmap (\b -> (a, b)) t) h) x y) a1) a2

compAdjComonad ::
  (Adjunction f1 g1, Adjunction f2 g2, Comonad w) =>
  (forall x y z. (x -> y -> z) -> w x -> w y -> w z) ->
  W.AdjointT f1 g1 w a ->
  W.AdjointT f2 g2 w b ->
  W.AdjointT (f2 :.: f1) (g1 :.: g2) w (a, b)
compAdjComonad f (W.AdjointT a1) (W.AdjointT a2) =
  W.AdjointT $
    (fmap . fmap) Comp1 $
      Comp1 $
        fmap (\x -> fmap (\y -> f (\t h -> fmap (\a -> fmap (\b -> (a, b)) t) h) x y) a1) a2

unCompSysAdjComonad ::
  (Adjunction f1 g1, Adjunction f2 g2, Comonad w) =>
  W.AdjointT (f2 :.: f1) (g1 :.: g2) w a ->
  (W.AdjointT f1 g1 w a, W.AdjointT f2 g2 w a)
unCompSysAdjComonad wa@(W.AdjointT a1) =
  ( \x ->
      ( W.AdjointT $ (fmap . fmap) ((() &) . leftAdjunct) $ (fmap . fmap) (const (extract wa) .) $ extractL $ (fmap . fmap . fmap) (rightAdjunct . const) x,
        W.AdjointT $ fmap ((\y -> fmap (const y) $ lower wa) . counit) $ (fmap . fmap) extract x
      )
  )
    $ unComp1
    $ (fmap . fmap) unComp1 a1

adjWunSumProd ::
  (Adjunction f1 g1, Adjunction f2 g2, Comonad w) =>
  W.AdjointT (f1 :+: f2) (g1 :*: g2) w a ->
  Either (W.AdjointT f1 g1 w a) (W.AdjointT f2 g2 w a)
adjWunSumProd wa@(W.AdjointT w1)
  | unLR w1 =
      Left $
        W.AdjointT $
          (fmap . fmap) (\(g1 :*: _) -> g1) $
            (\(L1 w) -> w) w1 -- (Left a)
adjWunSumProd wa@(W.AdjointT w1)
  | True =
      Right $
        W.AdjointT $
          (fmap . fmap) (\(_ :*: g1) -> g1) $
            (\(R1 w) -> w) w1 -- (Right a)

unLR (L1 _) = True
unLR (R1 _) = False

(@+*) ::
  (Adjunction f1 g1, Adjunction f2 g2, Comonad w) =>
  W.AdjointT f1 g1 w a ->
  W.AdjointT f2 g2 w b ->
  [W.AdjointT (f1 :+: f2) (g1 :*: g2) w (Either a b)]
(@+*) (W.AdjointT a1) (W.AdjointT a2) =
  [W.AdjointT $ ((fmap . fmap) (\g -> (fmap Left g) :*: (fmap Right $ extract $ extractL $ a2))) $ L1 a1]
    ++ [W.AdjointT $ (((fmap . fmap) (\g2 -> (fmap Left $ extract $ extractL a1) :*: (fmap Right g2)) . R1) a2)]

-- CoT

type AdjCoT fw gw w m a = CoT (W.AdjointT fw gw w) m a

combineAdjCoT ::
  (Adjunction fw1 gw1, Adjunction fw2 gw2, Comonad w, Monad m) =>
  AdjCoT fw1 gw1 w m a ->
  AdjCoT fw2 gw2 w m a ->
  AdjCoT (fw2 :.: fw1) (gw1 :.: gw2) w m a
combineAdjCoT (CoT a) (CoT b) =
  CoT $
    unCompSysAdjComonad
      >>> (a) *** (b)
      >>> (\(mr1, mr2) -> mr1 >> mr2)

-- for Kleisli, Cokleisli

type KleisliAdj f g m a b = Kleisli (M.AdjointT f g m) a b

type CokleisliAdj f g w a b = Cokleisli (W.AdjointT f g w) a b

type CoAndKleisliAdj fw gw w fm gm m a b = CoAndKleisli (W.AdjointT fw gw w) (M.AdjointT fm gm m) a b

($$##) ::
  (Adjunction f1 g1, Adjunction f2 g2, Monad m) =>
  KleisliAdj f1 g1 m a a2 ->
  KleisliAdj f2 g2 m b b2 ->
  KleisliAdj (f2 :.: f1) (g1 :.: g2) m (a, b) (a2, b2)
($$##) (Kleisli a1) (Kleisli a2) = Kleisli $ (\(x, y) -> (a1 x) $## (a2 y))

($$+*) ::
  ( MonadPlus m,
    Traversable f1,
    Traversable f2,
    Adjunction f1 g1,
    Adjunction f2 g2
  ) =>
  KleisliAdj f1 g1 m a a2 ->
  KleisliAdj f2 g2 m b b2 ->
  KleisliAdj (f1 :+: f2) (g1 :*: g2) m (Either a b) (Either a2 b2)
($$+*) (Kleisli a1) (Kleisli a2) =
  Kleisli $
    (\x -> (a1 x) $+* (lift mzero)) ||| (\y -> (lift mzero) $+* (a2 y))

(@@##) ::
  (Adjunction f1 g1, Adjunction f2 g2, Comonad w) =>
  CokleisliAdj f1 g1 w a a2 ->
  CokleisliAdj f2 g2 w b b2 ->
  CokleisliAdj (f2 :.: f1) (g1 :.: g2) w (a, b) (a2, b2)
(@@##) (Cokleisli a1) (Cokleisli a2) =
  Cokleisli $
    ((\(x, y) -> ((a1 $ fmap fst x), (a2 $ fmap snd y))) . unCompSysAdjComonad)

(@@+*) ::
  (Adjunction f1 g1, Adjunction f2 g2, Comonad w) =>
  CokleisliAdj f1 g1 w a a2 ->
  CokleisliAdj f2 g2 w a b2 ->
  CokleisliAdj (f1 :+: f2) (g1 :*: g2) w a (Either a2 b2)
(@@+*) (Cokleisli a1) (Cokleisli a2) =
  Cokleisli $
    f
      . (\x -> adjWunSumProd x)
  where
    f (Left w) = Left $ a1 w
    f (Right w) = Right $ a2 w

(@$$##) ::
  ( Adjunction fw1 gw1,
    Adjunction fm1 gm1,
    Adjunction fm2 gm2,
    Comonad w,
    Monad m
  ) =>
  CoAndKleisliAdj fw1 gw1 w fm1 gm1 m a1 b1 ->
  CoAndKleisliAdj fw1 gw1 w fm2 gm2 m a2 b2 ->
  CoAndKleisliAdj fw1 gw1 w (fm2 :.: fm1) (gm1 :.: gm2) m (a1, a2) (b1, b2)
(@$$##) (CoAndKleisli a1) (CoAndKleisli a2) =
  CoAndKleisli $ (\a -> a . (\x -> (fmap fst x, fmap snd x))) $ runKleisli $ (Kleisli a1) $$## (Kleisli a2)

(@$$+*) ::
  ( Adjunction fw1 gw1,
    Adjunction fm1 gm1,
    Adjunction fm2 gm2,
    Comonad w,
    Monad m,
    MonadPlus m,
    Traversable fm1,
    Traversable fm2
  ) =>
  CoAndKleisliAdj fw1 gw1 w fm1 gm1 m a1 b1 ->
  CoAndKleisliAdj fw1 gw1 w fm2 gm2 m a2 b2 ->
  CoAndKleisliAdj fw1 gw1 w (fm1 :+: fm2) (gm1 :*: gm2) m (Either a1 a2) (Either b1 b2)
(@$$+*) (CoAndKleisli a1) (CoAndKleisli a2) =
  CoAndKleisli $ \wa -> case extract wa of
    (Left x) -> (a1 $ fmap (const x) wa) $+* (lift mzero)
    (Right y) -> (lift mzero) $+* (a2 $ fmap (const y) wa)

(@@$##) ::
  ( Adjunction fw1 gw1,
    Adjunction fm1 gm1,
    Adjunction fw2 gw2,
    Comonad w,
    Monad m
  ) =>
  CoAndKleisliAdj fw1 gw1 w fm1 gm1 m a1 b1 ->
  CoAndKleisliAdj fw2 gw2 w fm1 gm1 m a2 b2 ->
  CoAndKleisliAdj (fw2 :.: fw1) (gw1 :.: gw2) w fm1 gm1 m (a1, a2) (b1, b2)
(@@$##) (CoAndKleisli a1) (CoAndKleisli a2) =
  CoAndKleisli $ (\a -> (\(m1, m2) -> m1 >>= (\x -> m2 >>= (\y -> return (x, y)))) . a) $ runCokleisli $ (Cokleisli a1) @@## (Cokleisli a2)

(@@$+*) ::
  ( Adjunction fw1 gw1,
    Adjunction fm1 gm1,
    Adjunction fw2 gw2,
    Comonad w,
    Monad m
  ) =>
  CoAndKleisliAdj fw1 gw1 w fm1 gm1 m a b1 ->
  CoAndKleisliAdj fw2 gw2 w fm1 gm1 m a b2 ->
  CoAndKleisliAdj (fw1 :+: fw2) (gw1 :*: gw2) w fm1 gm1 m a (Either b1 b2)
(@@$+*) (CoAndKleisli a1) (CoAndKleisli a2) =
  CoAndKleisli $ (\a -> bisequence . a) $ runCokleisli $ (Cokleisli a1) @@+* (Cokleisli a2)

(@$##) ::
  ( Adjunction fw1 gw1,
    Adjunction fm1 gm1,
    Adjunction fm2 gm2,
    Adjunction fw2 gw2,
    Comonad w,
    Monad m
  ) =>
  CoAndKleisliAdj fw1 gw1 w fm1 gm1 m a1 b1 ->
  CoAndKleisliAdj fw2 gw2 w fm2 gm2 m a2 b2 ->
  CoAndKleisliAdj (fw2 :.: fw1) (gw1 :.: gw2) w (fm2 :.: fm1) (gm1 :.: gm2) m (a1, a2) (b1, b2)
(@$##) (CoAndKleisli a1) (CoAndKleisli a2) =
  CoAndKleisli $ (\(wa1, wa2) -> (a1 $ fmap fst wa1) $## (a2 $ fmap snd wa2)) . unCompSysAdjComonad

(@$+*) ::
  ( Adjunction fw1 gw1,
    Adjunction fm1 gm1,
    Adjunction fm2 gm2,
    Adjunction fw2 gw2,
    Comonad w,
    Monad m
  ) =>
  CoAndKleisliAdj fw1 gw1 w fm1 gm1 m a1 b1 ->
  CoAndKleisliAdj fw2 gw2 w fm2 gm2 m a2 b2 ->
  CoAndKleisliAdj (fw1 :+: fw2) (gw1 :*: gw2) w (fm2 :.: fm1) (gm1 :.: gm2) m (a1, a2) (b1, b2)
(@$+*) (CoAndKleisli a1) (CoAndKleisli a2) = undefined

--  CoAndKleisli $ \ waa -> (a1 $ fmap fst waa) $## (a2 $ fmap snd waa)

{-
-- profunctors

compProfunctorSys ::
  (CxtSystemCore s1, CxtSystemCore s2, SysMonad s1 ~ SysMonad s2, Cat.Category p, Strong p) =>
  SysProMonad s1 p a b ->
  SysProMonad s2 p a2 b2 ->
  SysProMonad (s1 :##: s2) p (a, a2) (b, b2)
compProfunctorSys (SysProMonad p1) (SysProMonad p2) = SysProMonad $ rmap (\(x, y) -> x $## y) $ ((first' p1) Cat.. (second' p2))

combProfunctorSys ::
  (CxtSystemCore s1, CxtSystemCore s2, SysMonad s1 ~ SysMonad s2, Cat.Category p, Strong p) =>
  SysProMonad s1 p a b ->
  SysProMonad s2 p a2 b2 ->
  SysProMonad (s1 :+*: s2) p (a, a2) (Either b b2)
combProfunctorSys (SysProMonad p1) (SysProMonad p2) = SysProMonad $ rmap (\(x, y) -> x $+* y) $ ((first' p1) Cat.. (second' p2))

type instance SysProfunctor (s1 :##: s2) = Procompose (SysProfunctor s2) (SysProfunctor s1)

type instance SysProfunctor (s1 :+*: s2) = Procompose (SysProfunctor s2) (SysProfunctor s1)

compProSysM ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysMonad s1 ~ SysMonad s2,
    Cat.Category (SysProfunctor s1),
    Strong (SysProfunctor s1),
    Cat.Category (SysProfunctor s2),
    Strong (SysProfunctor s2)
  ) =>
  SysPMonad s1 a b ->
  SysPMonad s2 a2 b2 ->
  SysPMonad (s1 :##: s2) (a, a2) (b, b2)
compProSysM (SysPMonad p1) (SysPMonad p2) = SysPMonad $ rmap (\(x, y) -> x $## y) $ (Procompose (second' p2) (first' p1))

combProSysM ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysMonad s1 ~ SysMonad s2,
    Cat.Category (SysProfunctor s1),
    Strong (SysProfunctor s1),
    Cat.Category (SysProfunctor s2),
    Strong (SysProfunctor s2)
  ) =>
  SysPMonad s1 a b ->
  SysPMonad s2 a2 b2 ->
  SysPMonad (s1 :+*: s2) (a, a2) (Either b b2)
combProSysM (SysPMonad p1) (SysPMonad p2) = SysPMonad $ rmap (\(x, y) -> x $+* y) $ (Procompose (second' p2) (first' p1))

compProSysW ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysComonad s1 ~ SysComonad s2,
    Cat.Category (SysProfunctor s1),
    Strong (SysProfunctor s1),
    Cat.Category (SysProfunctor s2),
    Strong (SysProfunctor s2)
  ) =>
  SysPComonad s1 a b ->
  SysPComonad s2 a2 b2 ->
  SysPComonad (s1 :##: s2) (a, a2) (b, b2)
compProSysW (SysPComonad p1) (SysPComonad p2) = SysPComonad $ lmap (unCompSysAdjComonad) $ (Procompose (second' (lmap (fmap snd) p2)) (first' (lmap (fmap fst) p1))) -- (second' (lmap fst p2) ) (first' (lmap snd p1)))

combProSysW ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysComonad s1 ~ SysComonad s2,
    Cat.Category (SysProfunctor s1),
    Choice (SysProfunctor s1),
    Cat.Category (SysProfunctor s2),
    Choice (SysProfunctor s2)
  ) =>
  SysPComonad s1 a b ->
  SysPComonad s2 a2 b2 ->
  SysPComonad (s1 :+*: s2) (Either a a2) (Either b b2)
combProSysW (SysPComonad p1) (SysPComonad p2) = SysPComonad $ lmap (\w -> sysAdjWunSumProd w (extract w)) $ (Procompose (right' p2) (left' p1))

{-
[SysAdjComonad (s1 :+*: s2) (Either a2 b2)] -> SysAdjComonad (s1 :+*: s2) (Either a2 b2))
-}

-- for Procompos Kleisly and Cokleisly

(@$##) ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysComonad s1 ~ SysComonad s2,
    CxtSysAdjComp s1 s2,
    SysMonad s1 ~ SysMonad s2,
    MonadPlus (SysMonad s1),
    Traversable (SysAdjF s1),
    Traversable (SysAdjF s2)
  ) =>
  PCAKSysAdj s1 a a2 ->
  PCAKSysAdj s2 b b2 ->
  PCAKSysAdj (s1 :##: s2) (a, b) (a2, b2)
(@$##) (PCAKleisliSysAdj (Procompose p q)) (PCAKleisliSysAdj (Procompose p1 q1)) = PCAKleisliSysAdj $ Procompose (p $$## p1) (q @@## q1)

(@$+*) ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysComonad s1 ~ SysComonad s2,
    CxtSysAdjComp s1 s2,
    SysMonad s1 ~ SysMonad s2,
    MonadPlus (SysMonad s1),
    Traversable (SysAdjF s1),
    Traversable (SysAdjF s2)
  ) =>
  PCAKSysAdj s1 a a2 ->
  PCAKSysAdj s2 b b2 ->
  PCAKSysAdj (s1 :+*: s2) (Either a b) (Either a2 b2)
(@$+*) (PCAKleisliSysAdj (Procompose p q)) (PCAKleisliSysAdj (Procompose p1 q1)) = PCAKleisliSysAdj $ Procompose (p $$+* p1) (q @@+* q1)

-- for Object, Subject

(%##) ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysMonad s1 ~ SysMonad s2,
    MonadPlus (SysMonad s1),
    Traversable (SysAdjF s1),
    Traversable (SysAdjF s2)
  ) =>
  (forall x. SysAdjMonad (s1 :##: s2) (x, x) -> SysAdjMonad (s1 :##: s2) x) ->
  Object s1 a ->
  Object s2 a ->
  Object (s1 :##: s2) a
(%##) f o1 o2 = jointWith (SysAdjMonad $ lift mzero) (SysAdjMonad $ lift mzero) (\x y -> f $ x $## y) o1 o2

(%+*) ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysMonad s1 ~ SysMonad s2,
    MonadPlus (SysMonad s1),
    Traversable (SysAdjF s1),
    Traversable (SysAdjF s2)
  ) =>
  (forall x. SysAdjMonad (s1 :+*: s2) (Either x x) -> SysAdjMonad (s1 :+*: s2) x) ->
  Object s1 a ->
  Object s2 a ->
  Object (s1 :+*: s2) a
(%+*) f o1 o2 = jointWith (SysAdjMonad $ lift mzero) (SysAdjMonad $ lift mzero) (\x y -> f $ x $+* y) o1 o2

(^##) ::
  (CxtSystemCore s1, CxtSystemCore s2, SysComonad s1 ~ SysComonad s2, ComonadApply (SysComonad s2)) =>
  (forall x. SysAdjComonad (s1 :##: s2) (x, x) -> SysAdjComonad (s1 :##: s2) x) ->
  (forall x. SysAdjComonad s2 x -> SysAdjComonad s1 x) ->
  (forall x. SysAdjComonad s1 x -> SysAdjComonad s2 x) ->
  Subject s1 a ->
  Subject s2 a ->
  Subject (s1 :##: s2) a
(^##) f fp gp o1 o2 = jointWithPoint fp gp (\x y -> f $ x @## y) o1 o2

(^+*) ::
  (CxtSystemCore s1, CxtSystemCore s2, SysComonad s1 ~ SysComonad s2, ComonadApply (SysComonad s2)) =>
  (forall x. [SysAdjComonad (s1 :+*: s2) (Either x x)] -> SysAdjComonad (s1 :+*: s2) x) ->
  (forall x. x -> SysAdjComonad s1 x) ->
  (forall x. x -> SysAdjComonad s2 x) ->
  Subject s1 a ->
  Subject s2 a ->
  Subject (s1 :+*: s2) a
(^+*) f fp gp o1 o2 = jointWithPoint (fp . extract) (gp . extract) (\x y -> f $ x @+* y) o1 o2

-- for Arrows Object, Subject

(%%##) ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysMonad s1 ~ SysMonad s2,
    MonadPlus (SysMonad s1),
    Traversable (SysAdjF s1),
    Traversable (SysAdjF s2)
  ) =>
  (forall x. KleisliSAM (s1 :##: s2) x (x, x)) ->
  (forall x. KleisliSAM (s1 :##: s2) (x, x) x) ->
  ArrObject s1 a a2 ->
  ArrObject s2 a a2 ->
  ArrObject (s1 :##: s2) a a2
(%%##) f1 f o1 o2 =
  jointWith
    (KleisliSAM $ Kleisli $ \_ -> SysAdjMonad $ lift $ mzero)
    (KleisliSAM $ Kleisli $ \_ -> SysAdjMonad $ lift $ mzero)
    (\x y -> f1 >>> (x $$## y) >>> f)
    o1
    o2

(%%+*) ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysMonad s1 ~ SysMonad s2,
    MonadPlus (SysMonad s1),
    Traversable (SysAdjF s1),
    Traversable (SysAdjF s2)
  ) =>
  (forall x. KleisliSAM (s1 :+*: s2) x (Either x x)) ->
  (forall x. KleisliSAM (s1 :+*: s2) (Either x x) x) ->
  ArrObject s1 a a2 ->
  ArrObject s2 a a2 ->
  ArrObject (s1 :+*: s2) a a2
(%%+*) f1 f o1 o2 =
  jointWith
    (KleisliSAM $ Kleisli $ \_ -> SysAdjMonad $ lift $ mzero)
    (KleisliSAM $ Kleisli $ \_ -> SysAdjMonad $ lift $ mzero)
    (\x y -> f1 >>> (x $$+* y) >>> f)
    o1
    o2

(^^##) ::
  (CxtSystemCore s1, CxtSystemCore s2, SysComonad s1 ~ SysComonad s2, ComonadApply (SysComonad s2)) =>
  (forall x. CokleisliSAW (s1 :##: s2) x (x, x)) ->
  (forall x. CokleisliSAW (s1 :##: s2) (x, x) x) ->
  (forall x. x -> SysAdjComonad s1 x) ->
  (forall x. x -> SysAdjComonad s2 x) ->
  ArrSubject s1 a a2 ->
  ArrSubject s2 a a2 ->
  ArrSubject (s1 :##: s2) a a2
(^^##) f1 f2 w1 w2 o1 o2 =
  jointWithPoint
    (\w -> CokleisliSAW $ Cokleisli $ unCokleisliSAW w . w2 . extract)
    (\w -> CokleisliSAW $ Cokleisli $ unCokleisliSAW w . w1 . extract)
    (\x y -> f1 >>> (x @@## y) >>> f2)
    o1
    o2

(^^+*) ::
  (CxtSystemCore s1, CxtSystemCore s2, SysComonad s1 ~ SysComonad s2, ComonadApply (SysComonad s2)) =>
  (forall x. CokleisliSAW (s1 :+*: s2) x (Either x x)) ->
  (forall x. CokleisliSAW (s1 :+*: s2) (Either x x) x) ->
  (forall x. x -> SysAdjComonad s1 x) ->
  (forall x. x -> SysAdjComonad s2 x) ->
  ArrSubject s1 a a2 ->
  ArrSubject s2 a a2 ->
  ArrSubject (s1 :+*: s2) a a2
(^^+*) f1 f2 w1 w2 o1 o2 =
  jointWithPoint
    (\w -> CokleisliSAW $ Cokleisli $ unCokleisliSAW w . w2 . extract)
    (\w -> CokleisliSAW $ Cokleisli $ unCokleisliSAW w . w1 . extract)
    (\x y -> f1 >>> (x @@+* y) >>> f2)
    o1
    o2

-- for Procompos Kleisly and Cokleisly in Object and Subject

(^%##) ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysComonad s1 ~ SysComonad s2,
    ComonadApply (SysComonad s2),
    CxtSystemCore s1,
    CxtSystemCore s2,
    SysMonad s1 ~ SysMonad s2,
    MonadPlus (SysMonad s1),
    Traversable (SysAdjF s1),
    Traversable (SysAdjF s2)
  ) =>
  (forall x. CokleisliSAW (s1 :##: s2) x (x, x)) ->
  (forall x. CokleisliSAW (s1 :##: s2) (x, x) x) ->
  (forall x. KleisliSAM (s1 :##: s2) x (x, x)) ->
  (forall x. KleisliSAM (s1 :##: s2) (x, x) x) ->
  (forall x. x -> SysAdjComonad s1 x) ->
  (forall x. x -> SysAdjComonad s2 x) ->
  ProObjSubject s1 a a2 ->
  ProObjSubject s2 a a2 ->
  ProObjSubject (s1 :##: s2) a a2
(^%##) f1 f2 (g1 :: forall x. KleisliSAM (s1 :##: s2) x (x, x)) g2 w1 w2 o1 o2 =
  jointWithPoint
    (\(Procompose k w) -> Procompose (KleisliSAM $ Kleisli $ \_ -> SysAdjMonad $ lift $ mzero) (CokleisliSAW $ Cokleisli $ unCokleisliSAW w . w2 . extract))
    (\(Procompose k w) -> Procompose (KleisliSAM $ Kleisli $ \_ -> SysAdjMonad $ lift $ mzero) (CokleisliSAW $ Cokleisli $ unCokleisliSAW w . w1 . extract))
    ( \(Procompose (KleisliSAM (Kleisli a1)) (CokleisliSAW (Cokleisli b1))) (Procompose (KleisliSAM (Kleisli a2)) (CokleisliSAW (Cokleisli b2))) ->
        (\x -> PCAKleisliSysAdj $ Procompose x (CokleisliSAW $ Cokleisli $ id)) $
          (>>> g2) $
            (\(CokleisliSAW (Cokleisli x)) -> KleisliSAM $ Kleisli x) $
              (>>> (arr (\(x, y) -> x $## y))) $
                (f1 >>>) $
                  (\f g -> (CokleisliSAW $ Cokleisli f) @@## (CokleisliSAW $ Cokleisli g)) (a1 . b1) (a2 . b2)
    )
    (mapFree unProCoAndKleisliSysAdj' o1)
    (mapFree unProCoAndKleisliSysAdj' o2)

{- (\f-> arrowToPCAKSysAdj ((g2 >>>) $ KleisliSAM $ (arr (\x->(x,())) >>>) $ (app <<<) $ Kleisli (\(x,g)-> return (Kleisli (\a-> (x $## (proCAKSysAdjToKleisli f ))), (x,g))) )
		((proCAKSysAdjToCokleisli f) @@## (CokleisliSAW $ Cokleisli $ _a $ (\t x -> (\y-> (const x) <$> x) <$> t x ) $ unCokleisliSAW (proCAKSysAdjToCokleisli f) . w2 . extract )) )-}
-- 	_b _c o1 o2
-- 	where
-- 		kl ::

(^%+*) ::
  ( CxtSystemCore s1,
    CxtSystemCore s2,
    SysComonad s1 ~ SysComonad s2,
    ComonadApply (SysComonad s2),
    CxtSystemCore s1,
    CxtSystemCore s2,
    SysMonad s1 ~ SysMonad s2,
    MonadPlus (SysMonad s1),
    Traversable (SysAdjF s1),
    Traversable (SysAdjF s2)
  ) =>
  (forall x. CokleisliSAW (s1 :+*: s2) x (Either x x)) ->
  (forall x. CokleisliSAW (s1 :+*: s2) (Either x x) x) ->
  (forall x. KleisliSAM (s1 :+*: s2) x (Either x x)) ->
  (forall x. KleisliSAM (s1 :+*: s2) (Either x x) x) ->
  (forall x. x -> SysAdjComonad s1 x) ->
  (forall x. x -> SysAdjComonad s2 x) ->
  ProObjSubject s1 a a2 ->
  ProObjSubject s2 a a2 ->
  ProObjSubject (s1 :+*: s2) a a2
(^%+*) f1 f2 g1 g2 w1 w2 o1 o2 =
  jointWithPoint
    (\(Procompose k w) -> Procompose (KleisliSAM $ Kleisli $ \_ -> SysAdjMonad $ lift $ mzero) (CokleisliSAW $ Cokleisli $ unCokleisliSAW w . w2 . extract))
    (\(Procompose k w) -> Procompose (KleisliSAM $ Kleisli $ \_ -> SysAdjMonad $ lift $ mzero) (CokleisliSAW $ Cokleisli $ unCokleisliSAW w . w1 . extract))
    ( \(Procompose (KleisliSAM (Kleisli a1)) (CokleisliSAW (Cokleisli b1))) (Procompose (KleisliSAM (Kleisli a2)) (CokleisliSAW (Cokleisli b2))) ->
        PCAKleisliSysAdj $
          (\x -> Procompose x (CokleisliSAW $ Cokleisli $ id)) $
            (>>> g2) $
              (\(CokleisliSAW (Cokleisli x)) -> KleisliSAM $ Kleisli x) $
                (>>> (arr fu)) $
                  (f1 >>>) $
                    (\f g -> (CokleisliSAW $ Cokleisli f) @@+* (CokleisliSAW $ Cokleisli g)) (a1 . b1) (a2 . b2)
    )
    (mapFree unProCoAndKleisliSysAdj' o1)
    (mapFree unProCoAndKleisliSysAdj' o2)
  where
    fu (Left x) = x $+* (SysAdjMonad $ lift mzero)
    fu (Right x) = (SysAdjMonad $ lift mzero) $+* x
-}
