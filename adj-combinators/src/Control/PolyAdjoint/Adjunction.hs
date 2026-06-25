module Control.PolyAdjoint.Adjunction where

import Data.Kind
import Data.Profunctor
import Data.Functor.Contravariant

import Data.Functor.Adjunction as AdjS

class Adjunction (f :: k1 -> k2) (g :: k2 -> k1) | f -> g, g -> f where
   unit :: a -> g (f a)
   unit           = leftAdjunct id
   counit :: f (g a) -> a
   counit         = rightAdjunct id

   leftAdjunct :: (f a -> b) -> a -> g b 
   leftAdjunct f  = fmap f . unit
   rightDjunct :: (a -> g b) -> f a -> b
   rightAdjunct f = counit . fmap f

newtype SAdj f a = SAdj {unSAdj :: f a} deriving ()

instance AdjS.Adjunction f g => Adjunction (SAdj f) (SAdj g) where
   unit a = fmap (SAdj . fmap SAdj) unit a
   counit = counit . fmap (unSAdj . fmap unSAdj)
   
   leftADjunct f a = SAdj $ leftAdjunct (f . unSAdj) a
   rightDjunct g fa = rightDjunct (unSAdj . g) (unSAdj fa)

data Nat = Z | S Nat
data DeltaObj (n :: Nat)

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

newtype GStar (f :: k -> Type) (a :: k) (b :: Type) =
   GStar {runGStar :: a -> f b}

instance Functor f => Profunctor (GStar f) where
   dimap f g (GStar h) = GStar (\a -> fmap g (h (f a)))

newtype GCostar (g :: k -> Type) (a :: k) (b :: Type) =
   GCostar {runGCostar :: g a -> b}

instance Functor f => Profunctor (GCostar f) where
   dimap f g (GCostar h) = GCostar (\ga -> g (h (fmap f ga)))

class (Profunctor l, Profunctor r) => ProAdjunction l r | l -> r, r -> l where
   proUnit :: a -> b -> Procompose r l a b
   proCounit :: Procompose l r a b -> a -> b

instance (Adjunction f g, Functor f, Functor g)
   => ProAdjunction (GStar (f :: Type -> Type)) (GCostar (g :: Type -> Type)) where
   proUnit a b = Procompose rightPro leftPro
      where
         leftPro = GStar (\_-> fmap (\_-> b) (unit a)))
	 rightPro = GCostar counit
   proCounit (Procompos (GStar leftOp) (GCostar rightOp)) a = let
      ga = unit a
      x = rightOp (fmap (\_-> a) ga)
      fb = leftOp x
      in counit fb

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
   proUnit a b = case proUnit a b ofa
      Procompose r1 l1 -> case proUnit a b of
         Procompose r2 l2 -> Procompose (Pair r1 r2) (Pair l1 l2)
   proCounit (Procompose (Pair l1 l2) (Pair r1 r2)) a = proCounit (Procompose l1 r1) a

-- class IsabelNucleus r f g
