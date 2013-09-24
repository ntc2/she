> {-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs,
>     NoImplicitPrelude, NoMonomorphismRestriction #-}

> module IPrelude where

> import Prelude hiding (return, (>>=))
> import IFunctor
> import IMonad
> import OldIO

> return = ireturn
> (>>=) = (=>=)


