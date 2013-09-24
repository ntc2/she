> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE TypeOperators #-}

> module IParse where

> import Data.Char
> import Control.Applicative

> import Parsley

> data Exp
>   = V Char
>   | Neg Exp
>   | Exp :+: Exp
>   deriving Show

> pExp :: P Char Exp
> pExp =
>   (| Neg (%teq '-'%) pExp
>    | (%teq '('%) pExp :+: (%teq '+'%) pExp (%teq ')'%)
>    | V (tok isAlpha)
>    |)
