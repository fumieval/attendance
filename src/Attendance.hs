{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Attendance
  ( aix
  , imprinting
  , failingA
  , TraversalA'
  , PrismA'
  , toListWhy
  , overWhy
  ) where

import Control.Lens
import Data.Monoid
import Prettyprinter (Pretty(..), Doc)
import Attendance.Internal

-- | 'toListOf' but it does not find anything, explains why
toListWhy :: LensLike' (Attendance (Const (Endo [a]))) s a -> s -> (Doc x, [a])
toListWhy l s = case l (attend . Const . Endo . (:)) s of
  Attendance trail _ (Const (Endo f)) -> (pretty trail, f [])

-- | 'over' but it doesn't update anything, explains why
overWhy :: LensLike (Attendance Identity) s t a b -> (a -> b) -> s -> (Doc x, t)
overWhy l f s = case l (attend . Identity . f) s of
  Attendance trail _ (Identity t) -> (pretty trail, t)

-- | Annotate an optic
imprinting :: (Profunctor p, Attendable f) => String -> Over p f s t a b -> Over p f s t a b
imprinting t l = fmap (doing t) `rmap` l

failingA :: Attendable f => LensLike (BazaarD a b) s t a b -> LensLike f s t a b -> LensLike f s t a b
failingA l r f s = case runBazaarD b (attend . Const . pure) of
  Attendance trail _ (Const []) -> attending (trail<>) -- TODO: prevent it from exploding somehow
    $ r f s
  _ -> runBazaarD b f
  where
    b = l (\a -> BazaarD $ \k -> k a) s

-- | Annotated 'ix'
aix :: (Ixed t, Attendable f, Show (Index t)) => Index t -> LensLike' f t (IxValue t)
aix i = imprinting ("aix " <> show i) $ ix i

type TraversalA' s a = forall f. Attendable f => LensLike' f s a

type PrismA' s a = forall p f. (Choice p, Attendable f) => Over' p f s a