> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE TypeOperators #-}

> module Fix where

> import Control.Arrow

> data Fix f = In (f (Fix f))

> newtype (:+:) f g x = Plus (Either (f x) (g x))
> newtype (:*:) f g x = Times (f x, g x)
> newtype K a x = K a
> newtype I x = I x

> infixr 4 :+:
> infixr 5 :*:

> type ListF x = K () :+: (K x :*: I)

> type List x = Fix (ListF x)

> pattern NilF = Plus (Left (K ()))
> pattern ConsF x xs = Plus (Right (Times (K x, I xs)))
> pattern Nil = In NilF
> pattern Cons x xs = In (ConsF x xs)

> foldFix :: Functor f => (f t -> t) -> Fix f -> t
> foldFix phi (In xs) = phi (fmap (foldFix phi) xs)

> paraFix :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
> -- paraFix g = snd . foldFix (\ fxt -> (In (fmap fst fxt), g fxt))
> paraFix g (In ff) = g (fmap (id &&& paraFix g) ff)

> (+++) :: List x -> List x -> List x
> xs +++ ys = foldFix phi xs where
>   phi NilF = ys
>   phi (ConsF x xs) = Cons x xs

> blat :: List x -> [x]
> blat = foldFix phi where
>   phi NilF = []
>   phi (ConsF x xs) = x : xs

> talb :: [x] -> List x
> talb = foldr Cons Nil

> instance (Functor f, Functor g) => Functor (f :+: g) where
>   fmap p (Plus (Left fx)) = Plus (Left (fmap p fx))
>   fmap p (Plus (Right gx)) = Plus (Right (fmap p gx))

> instance (Functor f, Functor g) => Functor (f :*: g) where
>   fmap p (Times (fx, gx)) = Times (fmap p fx, fmap p gx)

> instance Functor (K a) where
>   fmap p (K a) = K a

> instance Functor I where
>   fmap p (I a) = I (p a)

