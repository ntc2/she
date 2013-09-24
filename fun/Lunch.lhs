> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, GADTs,
>     TypeFamilies, NoImplicitPrelude, MultiParamTypeClasses,
>     FlexibleContexts #-}

> module Main where

> import Prelude hiding (return, (>>=))
> import System.FilePath
> import System.IO
> import System.IO.Error
> import Debug.Trace

> import ShePrelude
> import OldIOStuff

> data Nat :: * where
>   Z :: Nat
>   S :: Nat -> Nat
>   deriving SheSingleton

> data Vec :: {Nat} -> * -> * where
>   VNil :: Vec {Z} x
>   (:>) :: x -> Vec {n} x -> Vec {S n} x

> vec :: forall x. pi (n :: Nat). x -> Vec {n} x
> vec {Z} x = VNil
> vec {S n} x = x :> vec {n} x

> type s :-> t = forall i. s i -> t i

> class IFunctor (phi :: ({i} -> *) -> {o} -> *) where
>   imap :: (s :-> t) -> phi s :-> phi t

> data Base :: * where
>   Bool :: Base
>   Int  :: Base

> data Ty :: * where
>   B :: Base -> Ty
>   Arr :: Ty -> Ty -> Ty

> data Tm :: ({Base} -> *) -> ({Ty} -> *) where
>   Var :: x {b} -> Tm x {B b}
>   (:$) :: Tm x {Arr s t} -> Tm x {s} -> Tm x {t}
>   Plus :: Tm x {Arr (B Int) (Arr (B Int) (B Int))}
>   Less :: Tm x {Arr (B Int) (Arr (B Int) (B Bool))}
>   Cond :: Tm x {Arr (B Bool) (Arr t (Arr t t))}

> instance IFunctor Tm where
>   imap ren (Var x) = Var (ren x)
>   imap ren (f :$ a) = imap ren f :$ imap ren a
>   imap ren Plus = Plus
>   imap ren Less = Less
>   imap ren Cond = Cond

> class {-IFunctor phi =>-} IMonad (phi :: ({i} -> *) -> {i} -> *) where
>   iskip :: s :-> phi s
>   iextend :: (s :-> phi t) -> (phi s :-> phi t)

> iseq :: IMonad phi => (r :-> phi s) -> (s :-> phi t) -> (r :-> phi t)
> iseq f g = iextend g . f

> (>>-) :: IMonad phi => phi s i -> (forall j. s j -> phi t j) -> phi t i
> (>>-) = flip iextend

> data (:-) :: * -> {x} -> {x} -> * where
>   V :: a -> (a :- x) x

> return :: IMonad phi => a -> phi (a :- i) i
> return a = iskip (V a)

> knownState :: (a -> t i) -> (a :- i) :-> t
> knownState f (V a) = f a

> (>>=) :: IMonad phi => phi (a :- j) i -> (a -> phi t j) -> phi t i
> c >>= f = c >>- knownState f

> data (:*) :: (({i} -> *) -> {i} -> *) -> ({i} -> *) -> {i} -> * where
>   Ret  :: q i -> (phi :* q) i
>   Do   :: phi (phi :* q) i -> (phi :* q) i

> instance IFunctor phi => IFunctor ((:*) phi) where
>   imap f = iextend (iskip . f)

> instance IFunctor phi => IMonad ((:*) phi) where
>   iskip = Ret
>   iextend f (Ret x)  = f x
>   iextend f (Do c)   = Do (imap (iextend f) c)


> data (:^) :: (({i} -> *) -> {i} -> *) -> ({i} -> *) -> {i} -> * where
>   RET   :: q i -> (phi :^ q) i
>   KDO   :: (forall r. (q :-> (phi :^ r)) -> phi (phi :^ r) i) -> (phi :^ q) i

> instance IMonad ((:^) phi) where
>   iskip = RET
>   iextend f (RET s) = f s
>   iextend f (KDO g) = KDO (\ k -> g (\ s -> iextend k (f s)))

> instance IApplicative ((:^) phi) where
>   pure = return
>   mf <*> ms = do
>     f <- mf
>     s <- ms
>     return (f s)

> data State :: * where
>   Open   :: State
>   Closed :: State
>   deriving SheSingleton

> data FH :: ({State} -> *) -> {State} -> * where
>   FOpen   :: FilePath -> (pi (s :: State). q s) -> FH q {Closed}
>   FGetC   :: (Maybe Char -> q {Open}) -> FH q {Open}
>   FClose  :: q {Closed} -> FH q {Open}

> instance IFunctor FH where
>   imap f (FOpen s k)  = FOpen s (f . k)
>   imap f (FClose q)   = FClose (f q)
>   imap f (FGetC k)    = FGetC (f . k)

> fOpen :: FilePath -> (FH :* (:: State)) {Closed}
> fOpen p = Do $ FOpen p Ret

> kfOpen :: FilePath -> (FH :^ (:: State)) {Closed}
> kfOpen p = KDO $ FOpen p

> fGetC :: (FH :* (Maybe Char :- {Open})) {Open}
> fGetC = Do $ FGetC return

> kfGetC :: (FH :^ (Maybe Char :- {Open})) {Open}
> kfGetC = KDO $ \ k -> FGetC (k . V)

> fClose :: (FH :* (() :- {Closed})) {Open}
> fClose = Do . FClose $ return ()

> kfClose :: (FH :^ (() :- {Closed})) {Open}
> kfClose = KDO $ \ k -> FClose (k (V ()))

> myReadFile :: FilePath -> (FH :* (Maybe String :- {Closed})) {Closed}
> myReadFile s = fOpen s >>- check where
>   check :: (:: State) :-> (FH :* (Maybe String :- {Closed}))
>   check {Closed}  = return Nothing
>   check {Open}    = do
>     s <- grab
>     _ <- fClose
>     return (Just s)
>   grab :: (FH :* (String :- {Open})) {Open}
>   grab = do
>     x <- fGetC
>     case x of
>       Nothing -> Ret (V "")
>       Just c -> do
>         s <- grab
>         return (c : s)

> class IApplicative (phi :: ({i} -> *) -> {i} -> *) where
>   pure :: x -> phi (x :- i) i
>   (<*>) :: phi ((s -> t) :- j) i -> phi (s :- k) j -> phi (t :- k) i
> (<*) :: IApplicative phi => phi (s :- j) i -> phi (t :- k) j -> phi (s :- k) i
> s <* t = (|const s t|)

> readOpenFile :: (FH :^ (String :- {Open})) {Open}
> readOpenFile = do
>   x <- kfGetC
>   case x of
>     Nothing ->  (|""|)
>     Just c ->   (| ~c : readOpenFile|)

> fileContents :: FilePath -> (FH :^ (Maybe String :- {Closed})) {Closed}
> fileContents p = kfOpen p >>- \ s -> case s of
>     {Closed}  -> (|Nothing|)
>     {Open}    -> (|Just readOpenFile (-kfClose-)|)

> runFH :: (FH :* (a :- {Closed})) {Closed} -> IO a
> runFH (Ret (V a)) = ioRet a
> runFH (Do (FOpen s f)) = catch
>   (openFile s ReadMode `ioBind` openFH (f {Open}))
>   (\ _ -> runFH (f {Closed}))
>   where
>     openFH :: (FH :* (a :- {Closed})) {Open} -> Handle -> IO a
>     openFH (Do (FClose k)) h = hClose h >> runFH k
>     openFH (Do (FGetC f)) h = catch
>       (hGetChar h `ioBind` \ c -> openFH (f (Just c)) h)
>       (\ _ -> openFH (f Nothing) h)

> kout :: (phi :^ t) i -> Either (phi (phi :^ t) i) (t i)
> kout (RET t) = Right t
> kout (KDO k) = Left (k RET)

> krunFH :: (FH :^ (a :- {Closed})) {Closed} -> IO a
> krunFH c = case kout c of
>   Right (V a) -> ioRet a
>   Left (FOpen s f) -> catch
>     (openFile s ReadMode `ioBind` krunFHOpen (f {Open}))
>     (\ _ -> krunFH (f {Closed}))
>   where
>     krunFHOpen :: (FH :^ (a :- {Closed})) {Open} -> Handle -> IO a
>     krunFHOpen c h = case kout c of
>       Left (FClose k)  -> hClose h >> krunFH k
>       Left (FGetC f)   -> catch
>         (hGetChar h `ioBind` \ c -> krunFHOpen (f (Just c)) h)
>         (\ _ -> krunFHOpen (f Nothing) h)

> main :: IO ()
> main =
>   krunFH (fileContents "Lunch.lhs") `ioBind` \ s ->
>   print s

