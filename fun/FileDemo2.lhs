> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs,
>     TypeFamilies, MultiParamTypeClasses #-}

> module FileDemo2 where

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

> pattern FRet a = (Right (V a))
> pattern FOpen p  k = Left (InL (V p :& k))
> pattern FGetC    k = Left (InR (InL (V () :& k)))
> pattern FClose   k = Left (InR (InR (V () :& k)))

> fOpen    ::  FilePath -> (FH :^ (:: State)) {Closed}
> fOpen p  =   thunk (FOpen p RET)
> fGetC    ::  (FH :^ (Maybe Char :- {Open})) {Open}
> fGetC    =   thunk (FGetC RET)
> fClose   ::  (FH :^ (() :- {Closed})) {Open}
> fClose   =   thunk (FClose RET)

> runFH :: (FH :^ (a :- {Closed})) {Closed} -> IO a
> runFH c = case force c of
>     FRet a     -> return a
>     FOpen s k  -> catch
>       (openFile s ReadMode >>= openFH (k {Open}))
>       (\ _ -> runFH (k {Closed}))
>   where
>     openFH :: (FH :^ (a :- {Closed})) {Open} -> Handle -> IO a
>     openFH c h = case force c of
>       FClose  k -> hClose h >> runFH (k (V ()))
>       FGetC   k -> catch
>         (hGetChar h >>= \ c -> openFH (k (V (Just c))) h)
>         (\ _ -> openFH (k (V Nothing)) h)

> fileContents :: FilePath -> (FH :^ (Maybe String :- {Closed})) {Closed}
> fileContents p = fOpen p ?>= \ s -> case s of
>   {Closed}  -> (| Nothing |)
>   {Open}    -> (| Just readOpenFile (-fClose-) |)

> readOpenFile :: (FH :^ (String :- {Open})) {Open}
> readOpenFile = fGetC =>= \ x -> case x of
>   Nothing  ->  (| "" |)
>   Just c   ->  (| ~c : readOpenFile |)
