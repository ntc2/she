> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs #-}

> module IFunctor where

> type s :-> t = forall i. s i -> t i

> class IFunctor (phi :: ({i} -> *) -> {o} -> *) where
>   imap :: (s :-> t) -> phi s :-> phi t

