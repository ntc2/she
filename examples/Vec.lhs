> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE GADTs, KindSignatures, TypeOperators, TypeFamilies, FlexibleContexts,
>     MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables,
>     RankNTypes #-}

> module Vec where

> import Control.Applicative

> import ShePrelude

> data Nat :: * where
>   Z :: Nat
>   S :: Nat -> Nat
>   deriving (SheSingleton)

> {-
> data Down :: ({Nat} -> *) -> {Nat} -> * where
>   DNil :: Down x {Z}
>   DCons :: x {n} -> Down x {n} -> Down x {S n}
> -}

> data Vec :: {Nat} -> * -> * where
>   VNil :: Vec {Z} x
>   (:>) :: x -> Vec {n} x -> Vec {S n} x
>   deriving ()

> instance Show x => Show (Vec {n} x) where
>   show VNil       = "VNil"
>   show (x :> xs)  = show x ++ " :> " ++ show xs

> vtail :: Vec {S n} x -> Vec {n} x
> vtail (x :> xs) = xs

> type family (m :: {Nat}) :+ (n :: {Nat}) :: {Nat}
> type instance {Z} :+ n = n
> type instance {S m} :+ n = {S} (m :+ n)

> instance Functor (Vec {n}) where
>   fmap f VNil = VNil
>   fmap f (x :> xs) = f x :> fmap f xs

> vappend :: Vec m x -> Vec n x -> Vec (m :+ n) x
> vappend VNil ys = ys
> vappend (x :> xs) ys = x :> vappend xs ys

> instance {:n :: Nat:} => Applicative (Vec {n}) where
>   pure = vec {:n :: Nat:} where
>     vec :: forall x. pi (n :: Nat). x -> Vec {n} x
>     vec {Z}    x = VNil
>     vec {S n}  x = x :> vec n x
>   (<*>) = vapp where
>     vapp :: Vec {m} (s -> t) -> Vec {m} s -> Vec {m} t
>     vapp VNil VNil = VNil
>     vapp (f :> fs)  (s :> ss) = f s :> vapp fs ss

> fiveByFive :: Vec {S (S (S (S (S Z))))} Int
> fiveByFive = pure 5

> vtakePox :: forall n. Pox n ->
>             forall x. pi (m :: Nat). Vec ({m} :+ {n}) x -> Vec {m} x
> vtakePox n {Z}   xs         = VNil
> vtakePox n {S m} (x :> xs)  = x :> vtakePox n {m} xs

> listVec :: [a] -> (pi (n :: Nat). Vec {n} a -> t) -> t
> listVec [] f = f {Z} VNil
> listVec (x : xs) f = listVec xs (\ n ys -> f {S n} (x :> ys))

> data Pox n = Pox

> pox :: forall box. (forall n. Pox n -> box n) -> (forall n. box n)
> pox f = f Pox

> newtype TakeBox n = TakeBox
>   {unboxTake :: forall x. pi (m :: Nat). Vec ({m} :+ {n}) x -> Vec {m} x}

> -- vtake :: forall n x. pi (m :: Nat). Vec ({m} :+ {n}) x -> Vec {m} x
> vtake = unboxTake (pox (TakeBox . vtakePox))
