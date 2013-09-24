> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs #-}

> module IMonad where

> import IFunctor

> class IFunctor phi => IMonad (phi :: ({i} -> *) -> {i} -> *) where
>   iskip :: s :-> phi s
>   iextend :: (s :-> phi t) -> (phi s :-> phi t)

> iseq :: IMonad phi => (r :-> phi s) -> (s :-> phi t) -> (r :-> phi t)
> iseq f g = iextend g . f

> (>>-) :: IMonad phi => phi s i -> (s :-> phi t) -> phi t i
> (>>-) = flip iextend

> data (:*) :: (({i} -> *) -> {i} -> *) -> ({i} -> *) -> {i} -> * where
>   Ret  :: q i -> (phi :* q) i
>   Do   :: phi (phi :* q) i -> (phi :* q) i

> instance IFunctor phi => IFunctor ((:*) phi) where
>   imap f = iextend (iskip . f)

> instance IFunctor phi => IMonad ((:*) phi) where
>   iskip = Ret
>   iextend f (Ret x) = f x
>   iextend f (Do d) = Do (imap (iextend f) d)

My favourite predicate.

> data (:-) :: * -> {x} -> {x} -> * where
>   V :: a -> (a :- x) x

> ret :: IMonad phi => a -> phi (a :- i) i
> ret a = iskip (V a)

> knownState :: (a -> t i) -> (a :- i) :-> t
> knownState f (V a) = f a

> (?-) :: IMonad phi => phi (a :- j) i -> (a -> phi t j) -> phi t i
> c ?- f = c >>- knownState f
