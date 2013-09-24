> module Imports where

> import Control.Applicative
> import System.IO.Error
> import System.FilePath
> import Data.Foldable hiding (elem)
> import Data.Monoid
> import Data.Maybe

> import HaLay
> import Parsley
> import ShesHers

> tryReadFile :: FilePath -> IO String
> tryReadFile s = catchIOError (readFile s) $ \ e ->
>  if isDoesNotExistError e then return "" else ioError e

> pImport :: P Tok [String]
> pImport = teq (KW "import") *> spc *> pSep (teq (Sym ".")) uid <* pRest

> grokImports :: [Tok] -> [[FilePath]]
> grokImports cs = foldMap pure $ parse pImport cs

> getHers :: [String] -> IO [[Tok]]
> getHers p = ready f <$> tryReadFile f where f = joinPath p <.> "hers"

> storySoFar :: [[Tok]] -> IO [[Tok]]
> storySoFar hs = (shesHers ++) <$>
>                 (oneOfEach [] <$> foldMap getHers (hs >>= grokImports))

> pModule :: P Tok String
> pModule = (teq (KW "module") *> spc *> uid) <* pRest

> oneOfEach :: [String] -> [[Tok]] -> [[Tok]]
> oneOfEach xs [] = []
> oneOfEach xs (ts : tss) = case parse pModule ts of
>   Just x | elem x xs  -> oneOfEach xs tss
>          | otherwise  -> ts : oneOfEach (x : xs) tss
>   Nothing -> ts : oneOfEach xs tss

> pModuleGuts :: P Tok [[Tok]]
> pModuleGuts = teq (KW "module") *> spc *> uid *> spc *> pLay "where" pRest

> getGuts :: [[Tok]] -> [[Tok]]
> getGuts = foldMap (fromMaybe [] . parse pModuleGuts)

> instance Monoid x => Monoid (IO x) where
>   mempty = pure mempty
>   mappend x y = mappend <$> x <*> y