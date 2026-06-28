{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}

module Control.Higher.Adjunction where

import Data.Functor.Combinator
import Control.Monad.Free as F

class (HFunctor t, HFunctor u) => HAdjunction (t :: (* -> *) -> * -> *) (u :: (* -> *) -> * -> *) | t -> u, u -> twhere
   leftHAdjunct :: (Functor f, Functor g) => (t f ~> g) -> (f ~> u g)
   rightHAdjunct :: (Functor f, Functor g) => (f ~> u g) -> (t f ~> g)

newtype PairF f a = PairF (f a, f a)

instance Functor f => Functor (PairF f) where
   fmap f (PairF (x,y)) = PairF (fmap f x,fmap f y)

instance HFunctor PairF where
   hmap f (PairF (x,y)) = PairF (f x,f y)

hfmap = hmap

newtype BoolReaderF g a = BoolReaderF (Bool -> g a)

instance Functor g => Functor (BoolReaderF g) where
   fmap f (BoolReaderF h) = BoolReaderF $ \b-> fmap f $ h b

instance HFunctor BoolReaderF where
   hmap f (BoolReaderF h) = BoolReaderF $ \b-> f $ h b

instance HAdjunction PairF BoolReaderF where
   leftHAdjunct nat fx = BoolReaderF (\b->
      if b then nat (PairF (fx,fx) ) else nat (PairF (fx,fx) )
      )
   rightHAdjunct nat (PairF (fx,fy)) = let
      BoolReaderF hx = nat fx
      BoolReaderF hy = nat fy
      in hx True

class HFunctor m => HMonad (m :: (* -> *) -> * -> *) where
   hreturn :: Functor f => f ~> m f
   hjoin :: Functor f => m (m f) ~> m f

hunit :: (HAdjunction t u, Functor f) => f ~> u (t f)
hunit = leftHAdjunct id

newtype CompH (t :: (* -> *) -> * -> *) (u :: (* -> *) -> * -> *) (f :: * -> *) (a :: *) = CompH {runCompH :: u (t f) a}

instance HAdjunction t u => HMonad (CompH t u) where
   hreturn fx = CompH (hunit fx)

   hjoin (CompH ututf) = CompH (hfmap (rightHAdjunct runCompH) $ _a ututf)

class HFunctor w => HComonad w where
   hextract :: w f ~> f
   hcojoin :: w f ~> w (w f)

hcoiunit :: (HAdjunction t u, Functor f) => (t (u f)) ~> f
hcoiunit = rightHAdjunct id

newtype CompHCo (t :: (* -> *) -> * -> *) (u :: (* -> *) -> * -> *) f a = CompHCo {runCompHCo :: t (u f) a}

instance (HFunctor t, HFunctor u, Functor f) => Functor (CompHCo t u f) where
   fmap f (CompHCo x) = CompHCo $ fmap f x

instance (HFunctor t, HFunctor u) => HFunctor (CompHCo t u) where
   hmap f (CompHCo x) = CompHCo $ hfmap (hfmap f) x

instance HAdjunction t u => HComonad (CompHCo t u) where
   hextract (CompHCo tuf) = hcounit tuf

   hcojoin (CompHCo tuf) = CompHCo (hfmap (leftAdjunct CompHCo) tuf)
{-
newtype HComp u v f a = HComp {runHComp :: u (v f) a}

instance (HFunctor t, HFunctor u, Functor f) => Functor (HComp t u f) where
   fmap f (HComp x) = HComp $ fmap f x

instance (HFunctor t, HFunctor u) => HFunctor (HComp t u) where
   hmap f (HComp x) = HComp $ hfmap (hfmap f) x
-}
data HSum (u :: (* -> *) -> * -> *) (v :: (* -> *) -> * -> *) f a = HInL (u f a) | HInR (v f a)



instance (HFunctor t, HFunctor u, Functor f) => Functor (HSum t u f) where
   fmap f (HInL x) = HInL $ fmap f x
   fmap f (HInR x) = HInR $ fmap f x

instance (HFunctor t, HFunctor u) => HFunctor (HSum t u) where
   hmap f (HInL x) = HInL $ hmap f x
   hmap f (HInR x) = HInR $ hmap f x

newtype HProd (u :: (* -> *) -> * -> *) (v :: (* -> *) -> * -> *) f a = HProd {runHProd :: (u f a, v f a)}

instance (HFunctor t, HFunctor u, Functor f) => Functor (HProd t u f) where
   fmap f (HProd (x,y)) = HProd (fmap f x, fmap f y)

instance (HFunctor t, HFunctor u) => HFunctor (HProd t u) where
   hmap f (HProd (x,y)) = HProd (hfmap f x, hfmap f y)

instance (HAdjunction t1 u1, HAdjunction t2 u2) => HAdjunction (HSum t1 t2) (HProd u1 u2) where
   leftHAdjunct nat fx = HProd (natL fx, natR fx)
      where
         natL t1f = nat (HInL t1f)
	 natR t2f = nat (HInR t2f)
   rightHAdjunct nat (HInL t1f) = let
      getLeft fx = let HProd (u1g,_) = nat fx in u1g
      in rightHAdjunct getLeft t1f
   rightHAdjunct nat (HInR t1f) = let
      getLeft fx = let HProd (_,u1g) = nat fx in u1g
      in rightHAdjunct getLeft t1f
   
instance HAdjunction F.Free IdentityT where
   leftHAdjunct nat fx = IdentityT $ nat $ Free $ fmap Pure fx
   rightHAdjunct nat (Free freeStruct) = foldFeree (runIdentityT . nat) freeStruct
      where
         foldFree _ (Pure x) = error "g is not Monad"
	 foldFree u (Free x) = u (fmap (foldFree) x)




