> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE TypeOperators, GADTs, KindSignatures, RankNTypes #-}

> module FH where

> import System.FilePath
> import System.IO
> import System.IO.Error

> import IFunctor
> import IMonad

> data FHState = FOpen | FClosed
> data FHSTATE :: {FHState} -> * where
>   FOPEN    :: FHSTATE {FOpen}
>   FCLOSED  :: FHSTATE {FClosed}

> data FH :: ({FHState} -> *) -> {FHState} -> * where
>   FHOpen   :: FilePath -> (FHSTATE :-> q) -> FH q {FClosed}
>   FHClose  :: q {FClosed} -> FH q {FOpen}
>   FHRead   :: (Maybe Char -> q {FOpen}) -> FH q {FOpen}

> instance IFunctor FH where
>   imap f (FHOpen s k) = FHOpen s (f . k)
>   imap f (FHClose q) = FHClose (f q)
>   imap f (FHRead k) = FHRead (f . k)

> fhOpen :: FilePath -> (FH :* FHSTATE) {FClosed}
> fhOpen f = Do $ FHOpen f Ret

> fhClose :: (FH :* (() :- {FClosed})) {FOpen}
> fhClose = Do . FHClose . Ret $ V ()

> fhRead :: (FH :* (Maybe Char :- {FOpen})) {FOpen}
> fhRead = Do . FHRead $ \ c -> Ret (V c)

> runFH :: (FH :* (a :- {FClosed})) {FClosed} -> IO a
> runFH (Ret (V a)) = return a
> runFH (Do (FHOpen s f)) = catch
>   (openFile s ReadMode >>= openFH (f FOPEN))
>   (\ _ -> runFH (f FCLOSED))
>   where
>     openFH :: (FH :* (a :- {FClosed})) {FOpen} -> Handle -> IO a
>     openFH (Do (FHClose k)) h = hClose h >> runFH k
>     openFH (Do (FHRead f)) h = catch
>       (hGetChar h >>= \ c -> openFH (f (Just c)) h)
>       (\ _ -> openFH (f Nothing) h)

> myReadFile :: FilePath -> IO (Maybe String)
> myReadFile s = runFH $ fhOpen s >>- check where
>   check :: FHSTATE :-> (FH :* (Maybe String :- {FClosed}))
>   check FCLOSED = ret Nothing
>   check FOPEN = grab ?- \ s -> fhClose ?- \ _ -> ret (Just s)
>   grab :: (FH :* (String :- {FOpen})) {FOpen}
>   grab = fhRead ?- \ x -> case x of
>     Nothing -> ret ""
>     Just c -> grab ?- \ cs -> ret (c : cs)

