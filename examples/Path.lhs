> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, GADTs, RankNTypes, TypeOperators #-}

> module Path where

> import Unsafe.Coerce

> import IFunctor
> import IMonad

> data Path :: ({i,i} -> *) -> {i,i} -> * where
>   Nil :: Path r {i,i}
>   (:-:) :: r {i,j} -> Path r {j,k} -> Path r {i,k}

> instance IFunctor Path where
>   imap f Nil = Nil
>   imap f (r :-: rs) = f r :-: imap f rs

> (+-+) :: Path r {i,j} -> Path r {j,k} -> Path r {i,k}
> Nil +-+ ps = ps
> (r :-: rs) +-+ ps = r :-: (rs +-+ ps)

> instance IMonad Path where
>   iskip = splip $ \ r -> r :-: Nil
>   iextend f (r :-: rs) = f r +-+ iextend f rs

> splip :: forall (s :: {i,j} -> *) (t :: {i,j} -> *) .
>            (forall i j . s {i,j} -> t {i,j}) -> s :-> t
> splip = unsafeCoerce