> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs #-}

> module IFunctor where

> type s :-> t = forall i. s i -> t i

> class IFunctor (phi :: ({i} -> *) -> {o} -> *) where
>   imap :: (s :-> t) -> phi s :-> phi t

> data (pre :>>: post) t i = pre i :& (post :-> t)

> instance IFunctor (pre :>>: post) where
>   imap f (p :& k) = p :& (f . k)

> data (IFunctor phi, IFunctor psi) => (phi :+: psi) t o
>   =  InL  (phi t o)
>   |  InR  (psi t o)

> infixr 4 :+:
> infixr 5 :>>:

> instance (IFunctor phi, IFunctor psi) => IFunctor (phi :+: psi) where
>   imap f (InL  p)  = InL  (imap f p)
>   imap f (InR  p)  = InR  (imap f p)
