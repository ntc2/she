> module Main where

> import System.Environment
> import System.FilePath
> import Data.Char
> import Data.Traversable
> import Data.Foldable
> import Data.List
> import Debug.Trace

> import HaLay
> import Imports
> import Aspect
> import DeBruijn
> import Patsy
> import TypesToKinds
> import IdiomBrackets
> import Superclass

> sheGoes :: FilePath -> [[Tok]] -> [[Tok]] -> ([[Tok]], [[Tok]])
> sheGoes mo inh hs0 =
>   let nl = dental hs0
>       hersi0 = getGuts inh
>       (higs0, _) = foldMap higgle hersi0
>       (higs1, hs1) = foldMap higgle hs0
>       higs = higs0 ++ higs1
>       herso0 = higs1 >>= higOut
>       hs2 = piggle higs hs1
>       hs2'5 = deBruijn hs2
>       hs2'75 = map idiomBrackets hs2'5
>       ((ps0, _), _) = traverse getPatsy hersi0
>       ((ps1, herso1), hs3) = traverse getPatsy hs2'75
>       (hs3'5, herso2) = superclass nl hersi0 hs3
>       hs4 = case prepare (ps0 ++ ps1) of
>               Nothing -> hs3'5
>               Just ps -> map (processes ps) hs3'5
>       hs5 = typesToKinds (noDerSing hs4) ++
>             redent nl ((hs4 >>= dataGrok) ++ (hs4 >>= singGrok))
>   in  (inh ++
>        [[NL (mo ++ ".hers", 0)],
>         [KW "module", Spc " ", Uid mo, Spc " ", L "where"
>           (redent [NL (mo ++ ".hers", 0), Spc "  "]
>            (herso0 ++ [[NL (mo ++ ".hers", 0)]] ++ herso1
>                    ++ [[NL (mo ++ ".hers", 0)]] ++ herso2))]]
>       , hs5)

> hsAndHers :: String -> FilePath -> String -> IO (String, String)
> hsAndHers f mo s = do
>   let ihs = ready f s
>   pcs <- storySoFar ihs
>   let (hers, hs) = sheGoes mo pcs ihs
>   return (tokssOut hs, tokssOut hers)

> main :: IO ()
> main = do
>   x : y : z : _ <- getArgs
>   let x' = replaceExtension x ".hers"
>   putStrLn x
>   putStrLn y
>   putStrLn z
>   f <- readFile y
>   (f', h) <- hsAndHers x (takeBaseName x) f
>   writeFile x' h
>   writeFile z f'

