> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs #-}

> module IMonad where

> import IFunctor

My favourite predicate.

> data (:-) :: forall (x :: *). * -> {x} -> {x} -> * where
>   V :: a -> (a :- x) x

> infixr 6 :-

> class IFunctor phi => IApplicative (phi :: ({i} -> *) -> {i} -> *) where
>   pure :: x -> phi (x :- i) i
>   (<*>) :: phi ((s -> t) :- j) i -> phi (s :- k) j -> phi (t :- k) i

> (<*) :: IApplicative phi => phi (s :- j) i -> phi (t :- k) j -> phi (s :- k) i
> s <* t = (|const s t|)

> class IApplicative phi => IMonad (phi :: ({i} -> *) -> {i} -> *) where
>   iskip :: s :-> phi s
>   iextend :: (s :-> phi t) -> (phi s :-> phi t)

> iseq :: IMonad phi => (r :-> phi s) -> (s :-> phi t) -> (r :-> phi t)
> iseq f g = iextend g . f

> (?>=) :: IMonad phi => phi s i -> (s :-> phi t) -> phi t i
> (?>=) = flip iextend

> ireturn :: IMonad phi => a -> phi (a :- i) i
> ireturn a = iskip (V a)

 knownState :: (a -> t i) -> (a :- i) :-> t
 knownState f (V a) = f a

> (=>=) :: IMonad phi => phi (a :- j) i -> (a -> phi t j) -> phi t i
> c =>= f = c ?>= \ (V a) -> f a

> data (:*) :: (({i} -> *) -> {i} -> *) -> ({i} -> *) -> {i} -> * where
>   Ret  :: s i -> (phi :* s) i
>   Do   :: phi (phi :* s) i -> (phi :* s) i

> instance IFunctor phi => IFunctor ((:*) phi) where
>   imap f = iextend (iskip . f)

> instance IFunctor phi => IApplicative ((:*) phi) where
>   pure = ireturn
>   mf <*> ms =
>     mf =>= \ f ->
>     ms =>= \ s ->
>     ireturn (f s)

> instance IFunctor phi => IMonad ((:*) phi) where
>   iskip = Ret
>   iextend f (Ret x) = f x
>   iextend f (Do d) = Do (imap (iextend f) d)

> data (:^) :: (({i} -> *) -> {i} -> *) -> ({i} -> *) -> {i} -> * where
>   RET   :: s i -> (phi :^ s) i
>   DO    :: (forall t. (s :-> (phi :^ t)) -> phi (phi :^ t) i) -> (phi :^ s) i

> thunk :: IFunctor f => Either (f (f :^ t) {i}) (t {i}) -> (f :^ t) {i}
> thunk (Right t)   = RET t
> thunk (Left fft)  = DO (\k -> imap (iextend k) fft)

> force :: (f :^ t) {i} -> Either (f (f :^ t) {i}) (t {i})
> force (RET t)  = Right t
> force (DO k)   = Left (k RET)

> instance IApplicative ((:^) phi) where
>   pure = ireturn
>   mf <*> ms =
>     mf =>= \ f ->
>     ms =>= \ s ->
>     ireturn (f s)

> instance IFunctor ((:^) phi) where
>   imap f (RET s)  = RET (f s)
>   imap f (DO g)   = DO $ \ k -> g $ \ s -> k (f s)

> instance IMonad ((:^) phi) where
>   iskip = RET
>   iextend f (RET s) = f s
>   iextend f (DO g)  = DO $ \ k -> g $ \ s -> iextend k (f s)

