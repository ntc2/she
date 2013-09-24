> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE TypeOperators #-}

> module Hig where

> import -> ExpF where
>   AddF :: t -> t -> ExpF t

> import -> ExpPS where
>   pattern Add x y = C (AddF x y)

> import -> FunctorExpF where
>   fmap f (AddF a b) = AddF (f a) (f b)

> import -> EvAlg where
>   AddF u v -> u + v

> import -> EvParser where
>   <|> Add <$ teq '(' <*> pExp <* teq '+' <*> pExp <* teq ')'
