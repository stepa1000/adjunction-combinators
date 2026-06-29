{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- Для связи нескольких параметров (cat1, cat2, f, g)
{-# LANGUAGE FunctionalDependencies #-} -- Для связей f -> g, g -> f
{-# LANGUAGE PolyKinds #-}              -- Для поддержки категорий над любыми видами
{-# LANGUAGE FlexibleContexts #-}       -- Для сложных ограничений в сигнатурах
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Control.PolyAdjoint.Adjunction where

import Data.Kind
import Data.Profunctor
import Data.Profunctor.Composition
import Data.Functor.Contravariant
import Data.Bifunctor.Product
import qualified Control.Category as Cat
import Control.Comonad.Trans.Env
import Data.Bifunctor.Tannen
import Data.Proxy
import Control.Arrow

import qualified Data.Functor.Adjunction as AdjS

class (Cat.Category cat1, Cat.Category cat2) => Adjunction cat1 cat2 (f :: k1 -> k2) (g :: k2 -> k1) 
   | f -> g, g -> f, cat1 -> cat2, cat2 -> cat1, cat1 -> f, f -> cat1 where
   unit :: forall (a :: k1). cat1 a (g (f a))
   --unit           = leftAdjunct id
   counit :: forall (a :: k2). cat2 (f (g a)) a
   --counit         = rightAdjunct id

   leftAdjunct :: forall (a :: k1) (b :: k2). (cat2 (f a) b) -> cat1 a (g b) 
   --leftAdjunct f  = fmap f . unit
   rightAdjunct :: forall (a :: k1) (b :: k2). (cat1 a (g b)) -> cat2 (f a) b
   --rightAdjunct f = counit . fmap f

newtype SAdj f a = SAdj {unSAdj :: f a} deriving ()

newtype SAdjTannen (f :: * -> *) (g :: * -> *) a b = SAdjTannen {runSAdjTannen :: forall x. TannenPF (f x, g x) (->) a b} deriving ()
{-
instance AdjS.Adjunction f g => Adjunction (SAdjTannen f g) (SAdjTannen f g) (SAdj f) (SAdj g) where
   unit = arr $ \ a -> fmap (SAdj . fmap SAdj) AdjS.unit a
   counit = arr $ AdjS.counit . fmap unSAdj . unSAdj 
   
   leftAdjunct f a = arr $ SAdj $ AdjS.leftAdjunct (f . SAdj) a
   rightAdjunct g fa = arr $ AdjS.rightAdjunct (unSAdj . g) (unSAdj fa)
-}
data Nat = Z | S Nat
data DeltaObj (n :: Nat)
{-
newtype SSet = SSet {getSimplex :: forall n. DeltaObj n -> Type}

data Cat = Cat {object :: Type, arrows :: Type -> Type -> Type}

newtype Nerve (c :: Cat) (n :: DeltaObj k) = Nerve
   { getChains :: Chain c n}

data Realize (s :: SSet) = Realize
   { objRealize :: getSimplex s (DeltaObj Z)
   , arrRealize :: Path s
   }

instance Adjunction Realize Nerve where
   unit :: forall (s :: SSet) (n :: DeltaObj k). getSimplex s n -> getChains (Nerve (Reaalize s) n)
   unit simplex = constructorChainsFromSimplex simplex

   counit :: forall (c :: Cat). Realize (Nerve c) -> c
   counit (Realize vertices path) = colapsePathToArrows path
      where
         colapsePathToArrows :: Path (Nerve c) -> arrows c x y
	 colapsePathToArrows (SingleArrow (Nerve chain)) = chain
	 colapsePathToArrows (ComposePath p1 p2) = (colapsePathToArrows p1) .>>>. (colapsePathToArrows p2)
-}

{-
class TheBijection k1 k2 where
   toBij :: forall (a :: k1) (b :: k2). a -> b
   fromBij :: forall (b :: k2) (a :: k1). b -> a

type family K1ToK2 k1 k2 :: k1 -> k2 where
   K1ToK2 k1 = toBij k1

type family K1FromK2 k1 k2 :: k2 -> k1

instance TheBijection k1 k2 => Adjunction ((TheKind k1) :: k1 -> k2) (g :: k2 -> k1)
-}
-- data TheKind k (a :: k) = TheKind {unTheKinf :: a} 

--data TheBijL k1 k2 (a :: k1) = TheBijL (runBijL :: a)

--data TheBijR k1 k2 (a :: k2) = TheBijR (runBijR :: a)

--instance TheBijection k1 k2 => Adjunction ((TheKind k1) :: k1 -> k2) (g :: k2 -> k1)

newtype GStar cat (f :: k1 -> k2) a b =
   GStar {runGStar :: cat a (f b)}

instance Functor f => Profunctor (GStar (->) f) where
   dimap f g (GStar h) = GStar (\a -> fmap g (h (f a)))

newtype GCostar cat (g :: k2 -> k1) a b =
   GCostar {runGCostar :: cat (g a) b}

instance Functor f => Profunctor (GCostar (->) f) where
   dimap f g (GCostar h) = GCostar (\ga -> g (h (fmap f ga)))

class (Profunctor l, Profunctor r) => ProAdjunction l r | l -> r, r -> l where
   proUnit :: a -> b -> Procompose r l a b
   proCounit :: Procompose l r a b -> a -> b

newtype TannenPF f p a b = TannenPF {runTannenPF :: Tannen (Env (Proxy f)) p a b} deriving ()

newtype CoAndStarTannen (f :: * -> *) (g :: * -> *) a b = CoAndStarTannen {runCoAndStarTannen :: forall x. TannenPF (f x, g x) (->) a b} deriving ()
{-
class PolyProfunctor cat p where
   dimap :: cat a b -> cat c d -> cat (p b c) (p a d)

class (Profunctor l, Profunctor r) => PolyProAdjunction cat l r | l -> r, r -> l where
   pproUnit :: cat a b -> Procompose r l a b
   pproCounit :: Procompose l r a b -> cat a b

instance (Arrow cat, Adjunction cat cat f g, Functor f, Functor g)
   => PolyProAdjunction cat (GStar cat (f :: k1 -> k2)) (GCostar cat (g :: k2 -> k1)) where
   pproUnit a b = Procompose rightPro (GStar $ _a $ arr (\_-> fmap (\ga-> fmap (const b) ga)) <<< (unit <<< arr (const a) ))
      where
         --leftPro = _a $ GStar (\_-> fmap (\_-> b) (unit a))
	 rightPro = GCostar counit
   pproCounit (Procompose (GStar leftOp) (GCostar rightOp)) a = let
      ga = unit a -- g (f a)
      x = rightOp (fmap (\fa-> a) ga)
      fb = leftOp x
      in counit $ fmap (\b-> fmap (const b) ga) fb
-}
{-
instance (ProAdjunction l1 r1, ProAdjunction l2 r2) => ProAdjunction (Procompose l1 l2) (Procompose r2 r1)  where
   proUnit a b = case proUnit a b of
      Procompose r1 l1 -> case proUnit a b of
         Procompose r2 l2 -> 
	    Procompose (Procompose r2 (dimap id (\_-> b ) r1))
	               (Procompose (dimap (\_-> a) id l1) l2)
   proCounit (Procompose (Procompose l1 l2) (Procompose r2 r1)) a = let
      f2 = proCounit (Procompose l2 r2)
      f1 = proCounit (Procompose l1 r1) 
      in f1 a . f2 a

instance (ProAdjunction l1 r1, ProAdjunction l2 r2) => ProAdjunction (Product l1 l2) (Product r1 r2)  where
   proUnit a b = case proUnit a b of
      Procompose r1 l1 -> case proUnit a b of
         Procompose r2 l2 -> Procompose (Pair r1 r2) (Pair l1 l2)
   proCounit (Procompose (Pair l1 l2) (Pair r1 r2)) a = proCounit (Procompose l1 r1) a
-}
-- class IsabelNucleus r f g
