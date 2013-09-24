> module OldIO where

> ioRet :: a -> IO a
> ioRet = return

> ioBind :: IO a -> (a -> IO b) -> IO b
> ioBind = (>>=)

