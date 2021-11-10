{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Attendance.Internal where

import Control.Lens
import Control.Lens.Internal.Bazaar
import Prettyprinter (Pretty(..), sep, nest, align, group, vsep, (<+>))

class Applicative f => Attendable f where
  attending :: (Trail String -> Trail String) -> f a -> f a

instance Attendable Identity where
  attending _ = id

instance Monoid a => Attendable (Const a) where
  attending _ = id

instance Attendable (BazaarT p f a b) where
  attending _ = id

doing :: Attendable f => String -> f a -> f a
doing t = attending (t:>>)

infixl 9 :>>
infixr 8 :||

data Trail a = Tip
  | a :>> Trail a
  | Trail a :|| Trail a
  deriving Show

instance Pretty a => Pretty (Trail a) where
  pretty (a :>> Tip) = pretty a
  pretty (a :>> f) = group $ nest 2 $ sep [pretty a, pretty ">>" <+> pretty f]
  pretty (f :|| g) = align $ vsep [pretty f, pretty g]
  pretty Tip = mempty

instance Eq a => Semigroup (Trail a) where
  Tip <> t = t
  t <> Tip = t
  a <> f :|| g = a :|| f :|| g
  f :|| g <> a = f :|| g :|| a
  xs'@(x :>> xs) <> ys'@(y :>> ys)
    | x == y = x :>> (xs <> ys)
    | otherwise = xs' :|| ys'

instance Eq a => Monoid (Trail a) where
  mempty = Tip

data Attendance f a = Attendance (Trail String) Bool (f a) deriving Functor

instance Applicative f => Applicative (Attendance f) where
  pure a = Attendance mempty False (pure a)
  Attendance xs p f <*> Attendance ys q g = Attendance (xs <> ys) (p || q) (f <*> g)

instance Applicative f => Attendable (Attendance f) where
  attending p (Attendance trail False f) = Attendance (p trail) False f
  attending _ (Attendance _ True f) = Attendance mempty True f

attend :: f a -> Attendance f a
attend = Attendance mempty True

data BazaarD a b t = BazaarD
  { runBazaarD :: forall f. Attendable f => (a -> f b) -> f t }
  deriving Functor

instance Applicative (BazaarD a b) where
  pure a = BazaarD $ const $ pure a
  f <*> g = BazaarD $ \afb -> runBazaarD f afb <*> runBazaarD g afb

instance Attendable (BazaarD a b) where
  attending t b = BazaarD $ \k -> attending t $ runBazaarD b k
