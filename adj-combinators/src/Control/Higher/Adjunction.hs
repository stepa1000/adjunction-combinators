
module Control.Higher.Adjunction where

import Data.Functor.Combinator

class (HFunctor t, HFunctor u) => HAdjunction t u where
   leftHAdjunct :: (Functor f, Functor g) => (t f ~> g) -> (f ~> u g)
   rightHAdjunct :: (Functor f, Functor g) => (f ~> u g) -> (t f ~> g)

newtype PairF f a = PairF (f a, f a)

instance Functor f => Functor (PairF f) where
   fmap f (PairF (x,y)) = PairF (fmap f x,fmap f y)

instance HFunctor PairF where
   hfmap f (PairF (x,y)) = PairF (f x,f y)

newtype BoolReaderF g a = BoolReaderF (Bool -> g a)

instance Functor g => Functor (BoolReaderF g) where
   fmap f (BoolReaderF h) = BoolReaderF $ \b-> fmap f $ h b

instance HFunctor BoolReaderF where
   hfmap f (BoolReaderF h) = BoolReaderF $ \b-> f $ h b

instance HAdjunction PairF BoolReaderF where
   leftHAdjunct nat fx = BoolReader (\b->
      if b then nat (PairF (fx,fx) ) else nat (PairF (fx,fx) )
      )
   rightHAdjunct nat (PairF (fx,fy)) = let
      BoolReaderF hx = nat fx
      BoolReaderF hy = nat fy
      in hx True

class HFUnctor m => HMonad m where
   hreturn :: Functor f => f ~> m f
   hjoin :: Functor f => m (m f) ~> m f

hunit :: (HAdjunction t u, Functor f) => f ~> u (t f)
hunit = leftHAdjunct id

instance HAdjunction t u => HMonad (Comp t u) where
   hreturn :: 
