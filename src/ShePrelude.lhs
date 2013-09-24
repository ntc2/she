> {-# LANGUAGE TypeOperators, KindSignatures, TypeFamilies, MultiParamTypeClasses, GADTs,
>     ScopedTypeVariables, FlexibleContexts
> #-}

> module ShePrelude where

> data SheProxy (ty :: *) (tm :: *) where SheProxy :: SheProxy ty tm
> class SheChecks (ty :: *) (tm :: *) where
>   sheTypes :: SheProxy ty tm -> SheSingleton ty tm
> data family SheSingleton ty :: * -> *

> data SheTyLeft x = SheTyLeft x
> data SheTyRight x = SheTyRight x
> data instance SheSingleton (Either s t) dummy where
>   SheWitLeft   :: SheSingleton s x -> SheSingleton (Either s t) (SheTyLeft x)
>   SheWitRight  :: SheSingleton t x -> SheSingleton (Either s t) (SheTyRight x)
> instance SheChecks s x => SheChecks (Either s t) (SheTyLeft x) where
>   sheTypes _ = SheWitLeft (sheTypes (SheProxy :: SheProxy s x))
> instance SheChecks t x => SheChecks (Either s t) (SheTyRight x) where
>   sheTypes _ = SheWitRight (sheTypes (SheProxy :: SheProxy t x))

> data SheTyTrue = SheTyTrue
> data SheTyFalse = SheTyFalse
> data instance SheSingleton Bool dummy where
>   SheWitTrue   :: SheSingleton Bool SheTyTrue
>   SheWitFalse  :: SheSingleton Bool SheTyFalse
> instance SheChecks Bool SheTyTrue where
>   sheTypes _ = SheWitTrue
> instance SheChecks Bool SheTyFalse where
>   sheTypes _ = SheWitFalse

> data SheSpecialNil = SheSpecialNil
> data (:$#$#$#:) x y = (:$#$#$#:) x y
> data instance SheSingleton [t] dummy where
>   SheSpecialWitNil :: SheSingleton [t] SheSpecialNil
>   (:$%$%$%:) :: SheSingleton t x -> SheSingleton [t] xs ->
>                 SheSingleton [t] (x :$#$#$#: xs)
> instance SheChecks [t] SheSpecialNil where
>   sheTypes _ = SheSpecialWitNil
> instance (SheChecks t x, SheChecks [t] xs) => SheChecks [t] (x :$#$#$#: xs) where
>   sheTypes _ = (sheTypes (SheProxy :: SheProxy t x)) :$%$%$%:
>                (sheTypes (SheProxy :: SheProxy [t] xs))
