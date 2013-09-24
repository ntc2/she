> {-# OPTIONS_GHC -F -pgmF she #-}
> module MaybeAMonad where

> import Classy

> instance AMonad Maybe where
>   areturn = Just
>   Nothing >^>= _ = Nothing
>   Just x >^>= f = f x

> instance AMonad [] where
>   areturn x = [x]
>   [] >^>= f = []
>   (x : xs) >^>= f = f x ++ xs >^>= f
>   hiding instance AFunctor []

> instance AFunctor [] where
>   afmap = map

> instance AMonad ((->) s) where
>   areturn x s = x
>   ff <^*> fa = \ s -> ff s (fa s)
>   fx >^>= f = \ s -> f (fx s) s