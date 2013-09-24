> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE TypeOperators, KindSignatures, GADTs #-}

> module Pig where

> import Control.Applicative
> import Data.Char

> import Parsley

> import Hig
> import Jig

> data ExpF :: * -> * where
>   import <- ExpF

> instance Functor ExpF where
>   import <- FunctorExpF

> data Free :: (* -> *) -> * -> * where
>   V :: x -> Free f x
>   C :: f (Free f x) -> Free f x

> fEval :: Functor f => (x -> t) -> (f t -> t) -> Free f x -> t
> fEval g f (V x)  = g x
> fEval g f (C fe) = f (fmap (fEval g f) fe)

> type Exp = Free ExpF
> import <- ExpPS

> eval :: (x -> Int) -> Exp x -> Int
> eval g = fEval g $ \ e -> case e of
>   import <- EvAlg

> pExp :: P Char (Exp Char)
> pExp = V <$> tok isAlpha
>   import <- EvParser
