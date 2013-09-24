> module IdiomBrackets where

> import Control.Applicative

> import HaLay
> import Parsley

> pIBAlts :: P Tok [[Tok]]
> pIBAlts = teq (Sym "|") *> many (many (tok (/= Sym "|")) <* teq (Sym "|"))

> pInfA :: P Tok ([Tok], Tok, [Tok])
> pInfA = (,,) <$> many (tok (not . infSym)) <*> tok infSym <*> pRest where
>   infSym (Sym s) = not (elem s ["~", "@"])
>   infSym _ = False

> ia :: Tok -> String -> Tok -> Tok
> ia x o y = B Rnd [x, Spc " ", Sym o, Spc " ", y]

> ip :: Tok -> Tok
> ip t = B Rnd [Lid "pure", Spc " ", t]

> iGrok :: Tok -> [Tok] -> Tok
> iGrok f [] = f
> iGrok f (s : ts) | isSpcT s = iGrok f ts
> iGrok f (Sym "@" : ts) = iGrok (B Rnd [Lid "join", Spc " ", f]) ts
> iGrok f (Sym "~" : Spc _ : ts) = iGrok f (Sym "~" : ts)
> iGrok f (Sym "~" : t : ts) = iGrok (ia f "<*>" (ip t)) ts
> iGrok f (B Rnd (Sym "%" : us) : ts) = iGrok (ia f "<*" (iGnore "%" us)) ts
> iGrok f (B Rnd (Sym "-" : us) : ts) = iGrok (ia f "<*" (iGnore "-" us)) ts
> iGrok f (t : ts) = iGrok (ia f "<*>" t) ts

> iGnore :: String -> [Tok] -> Tok
> iGnore z ts = case parse (pSep (teq Semi) (many (tok notSep)) <* teq (Sym z)) ts of
>     Nothing -> ip (B Rnd [])
>     Just [] -> ip (B Rnd [])
>     Just [ts] -> B Rnd ts
>     Just (ts : tss) -> foldl (\ a bs -> ia a "<*" (B Rnd bs)) (B Rnd ts) tss
>   where
>     notSep Semi = False
>     notSep (Sym x) | x == z = False
>     notSep _ = True

> iStart :: [Tok] -> Tok
> iStart [] = ip (B Rnd [])
> iStart (s : ts) | isSpcT s = iStart ts
> iStart (B Rnd (Sym "%" : us) : ts) = ia (iGnore "%" us) "*>" (iStart ts)
> iStart (B Rnd (Sym "-" : us) : ts) = ia (iGnore "-" us) "*>" (iStart ts)
> iStart (t : ts) = iGrok (ip t) ts

> iIA :: Tok -> [Tok] -> Tok
> iIA f [] = f
> iIA f (s : ts) | isSpcT s = iIA f ts
> iIA f (Sym "@" : ts) = iIA (B Rnd [Lid "join", Spc " ", f]) ts
> iIA f (Sym "~" : Spc _ : ts) = iIA f (Sym "~" : ts)
> iIA f (Sym "~" : t : ts) = iIA (ia f "<*>" (ip t)) ts
> iIA f (B Rnd (Sym "%" : us) : ts) = iIA (ia f "<*" (iGnore "%" us)) ts
> iIA f (B Rnd (Sym "-" : us) : ts) = iIA (ia f "<*" (iGnore "-" us)) ts
> iIA f (t : ts) = let (us, vs) = span yum ts in iIA (ia f "<*>" (B Rnd (t : us))) vs
>   where yum (B Rnd (Sym "%" : _)) = False
>         yum (Sym "@") = False
>         yum _ = True

> iPro :: [Tok] -> Tok
> iPro ts = case parse pInfA ts of
>   Nothing -> iStart ts
>   Just (as, o, bs) -> iIA (iIA (ip (B Rnd [o])) as) bs

> rejig :: [[Tok]] -> Tok
> rejig [] = Lid "empty"
> rejig [ts] = iPro ts
> rejig (ts : tss) = ia (iPro ts) "<|>" (rejig tss)

> idiomBrackets :: [Tok] -> [Tok]
> idiomBrackets = munge ibMu where
>   ibMu (B Rnd us : ts) = case parse pIBAlts us of
>     Just uss -> Just $ rejig (map (munge ibMu) uss) : munge ibMu ts
>     Nothing -> Nothing
>   ibMu _ = Nothing
