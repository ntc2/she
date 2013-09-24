> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE GADTs, KindSignatures, TypeOperators, TypeFamilies #-}

> module Binding where

> import Control.Applicative

> import ShePrelude

> data Tm
>   = V Int
>   | L Tm
>   | Tm :$ Tm
>   deriving Show

> infixl 6 :$

> data Va
>   = F (Va -> Va)
>   | [Va] :- Int    -- backwards

> ($$) :: Va -> Va -> Va
> F f $$ v = f v
> (vs :- p) $$ v = (v : vs) :- p

> ev :: Tm -> [Va] -> Va
> ev (V i)     g = g !! i
> ev (L t)     g = F (\ v -> ev t (v : g))
> ev (f :$ s)  g = (|ev f $$ ev s|) g

> qu :: Int -> Va -> Tm
> qu i (F f)     = L (qu (i + 1) (f ([] :- i)))
> qu i (vs :- j) = foldr (flip (:$)) (V (i - j - 1)) (map (qu i) vs)

> zz :: Tm
> zz = L [.f. L [.x. V x]]
> ss :: Tm
> ss = L [.n. L [.f. L [.x. V f :$ (V n :$ V f :$ V x)]]]

> test3 :: Va
> test3 = ev [.zz.ss. V ss :$ (V ss :$ (V ss :$ V zz))] [ev ss [],ev zz []]

> norm :: Tm -> Tm
> norm t = qu 0 (ev t [])
