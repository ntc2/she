{-# OPTIONS_GHC -F -pgmF she #-}
{-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs,
    TypeFamilies, MultiParamTypeClasses, UndecidableInstances,
    FlexibleContexts #-} -- KitchenSink

module HIW where

import ShePrelude
import Control.Applicative
import Data.Foldable
import Data.Traversable

data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat
  deriving SheSingleton

data Vec :: {Nat} -> * -> * where
  VNil :: Vec {Z} x
  (:>) :: x -> Vec {n} x -> Vec {S n} x

vapp :: Vec {n} (s -> t) -> Vec {n} s -> Vec {n} t
vapp VNil VNil = VNil
vapp (f :> fs) (s :> ss) = f s :> vapp fs ss

vec :: forall x. pi (n :: Nat). x -> Vec {n} x
vec {Z} x = VNil
vec {S n} x = x :> vec {n} x

instance {: n :: Nat :} => Applicative (Vec {n}) where
  pure = vec {: n :: Nat :}
  (<*>) = vapp

instance Functor (Vec {n}) where
  fmap = fmapDefault

instance Traversable (Vec {n}) where
  traverse f VNil = (| VNil |)
  traverse f (x :> xs) = (| f x :> traverse f xs |)

instance Foldable (Vec {n}) where
  foldMap = foldMapDefault


vecList :: Vec {n} x -> [x]
vecList = foldMap pure

instance Show x => Show (Vec {n} x) where
  show xs = show (vecList xs)

idMat :: pi (n :: Nat). Vec {n} (Vec {n} Int)
idMat {Z} = VNil
idMat {S n} = (1 :> (| 0 |)) :>
           (| (| 0 |) :> idMat {n} |)
