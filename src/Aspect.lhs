> module Aspect where

> import Prelude hiding (all, minimum)
> import Control.Applicative
> import Data.Foldable
> import Control.Monad
> import Control.Arrow

> import HaLay
> import Parsley

> type Higgle = (String, [[Tok]])

> pHiggle :: P Tok Higgle
> pHiggle = (,) <$ teq (KW "import") <* spc <* teq (Sym "->") <* spc
>               <*> uid <* spc <*> (outdent <$> pLay "where" pRest)

> higgle :: [Tok] -> ([Higgle], [[Tok]])
> higgle ts = case parse pHiggle ts of
>   Just h -> ([h], [])
>   Nothing -> ([], [ts])

> outdent :: [[Tok]] -> [[Tok]]
> outdent tss = map (munge nip) tss where
>   nip (NL fl : Spc ss : ts) = Just $
>     NL fl : Spc (replicate (length ss - l) ' ') : munge nip ts
>   nip _ = Nothing
>   l = skin tss
>   skin [] = 0
>   skin ([NL _, Spc ss] : (t : _) : _) | not (isSpcT t) = length ss
>   skin (_ : tss) = skin tss

> indent :: Int -> [Tok] -> [Tok]
> indent i = munge dent where
>   dent (NL fl : Spc ss : ts) = Just $
>     NL fl : Spc (replicate i ' ' ++ ss) : munge dent ts
>   dent _ = Nothing

> dropIn :: [Higgle] -> Int -> String -> [[Tok]]
> dropIn higs i s = do
>   (s', tss) <- higs
>   guard (s == s')
>   indent i <$> tss

> higOut :: Higgle -> [[Tok]]
> higOut (s, tss) =
>   [ [NL ("Dunno.lhs", 0)]
>   , [KW "import", Spc " ", Sym "->", Spc " ", Uid s, Spc " ", L "where" (indent 4 <$> tss)]
>   ]

> pDent :: P Tok Int
> pDent = pNL *> grok spcl next <|> 0 <$ pNL where
>   spcl (Spc ss) = Just (length ss)
>   spcl _ = Nothing

> pPiggle :: P Tok (String, [Tok])
> pPiggle = (,) <$ teq (KW "import") <* spc <* teq (Sym "<-") <* spc
>               <*> uid <* spc <*> pRest

> piggle :: [Higgle] -> [[Tok]] -> [[Tok]]
> piggle higs = mungeLines lp ep where
>   lp (k : l : tss) = do
>     i <- parse pDent k
>     (s, _) <- parse pPiggle l
>     return $
>       k : dashOut l : k : dropIn higs i s ++ mungeLines lp ep tss
>   lp _ = Nothing
>   ep ts = do
>     (i, (s, ts)) <- parse ((,) <$> pDent <*> pPiggle) ts
>     return $
>       NL ("Dunno.lhs", 0): Spc (replicate i ' ') : join (dropIn higs i s) ++ ts
