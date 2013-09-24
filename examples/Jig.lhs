> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE TypeOperators #-}

> module Jig where

> import -> ExpF where
>   NegF :: t -> ExpF t

> import -> ExpPS where
>   pattern Neg x = C (NegF x)

> import -> FunctorExpF where
>   fmap f (NegF a) = NegF (f a)

> import -> EvAlg where
>   NegF u -> negate u

> import -> EvParser where
>   <|> Neg <$ teq '-' <*> pExp
