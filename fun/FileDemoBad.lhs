> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs,
>     TypeFamilies, MultiParamTypeClasses #-}

> module FileDemo where

> import System.FilePath
> import System.IO
> import System.IO.Error

> import ShePrelude
> import IFunctor
> import IMonad

> data State :: * where
>   Open    :: State
>   Closed  :: State
>   deriving SheSingleton

> type FH -- |:: ({State} -> *) -> {State} -> *|
>   =    {- fOpen -}   FilePath  :- {Closed}  :>>: (:: State)
>   :+:  {- fGetC -}   ()        :- {Open}    :>>: Maybe Char :- {Open}
>   :+:  {- fClose -}  ()        :- {Open}    :>>: () :- {Closed}

> pattern FOpen p  k = Do (InL (V p :& k))
> pattern FGetC    k = Do (InR (InL (V () :& k)))
> pattern FClose   k = Do (InR (InR (V () :& k)))

> fOpen    ::  FilePath -> (FH :* (:: State)) {Closed}
> fOpen p  =   FOpen p Ret
> fGetC    ::  (FH :* (Maybe Char :- {Open})) {Open}
> fGetC    =   FGetC Ret
> fClose   ::  (FH :* (() :- {Closed})) {Open}
> fClose   =   FClose Ret

> runFH :: (FH :* (a :- {Closed})) {Closed} -> IO a
> runFH (Ret (V a)) = return a
> runFH (FOpen s k) = catch
>   (openFile s ReadMode >>= openFH (k {Open}))
>   (\ _ -> runFH (k {Closed}))
>   where
>     openFH :: (FH :* (a :- {Closed})) {Open} -> Handle -> IO a
>     openFH (FClose  k) h = hClose h >> runFH (k (V ()))
>     openFH (FGetC   k) h = catch
>       (hGetChar h >>= \ c -> openFH (k (V (Just c))) h)
>       (\ _ -> openFH (k (V Nothing)) h)

> fileContents :: FilePath -> (FH :* (Maybe String :- {Closed})) {Closed}
> fileContents p = fOpen p ?>= \ s -> case s of
>   {Closed}  -> (| Nothing |)
>   {Open}    -> (| Just readOpenFile (-fClose-) |)

> readOpenFile :: (FH :* (String :- {Open})) {Open}
> readOpenFile = fGetC =>= \ x -> case x of
>   Nothing  ->  (| "" |)
>   Just c   ->  (| ~c : readOpenFile |)
