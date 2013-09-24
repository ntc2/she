> module DeBruijn where

> import Data.List
> import Control.Applicative

> import Parsley
> import HaLay

> deBruijnMu :: [String] -> [Tok] -> Maybe [Tok]
> deBruijnMu xs (B Sqr ss : ts)
>   = (:) <$> parse jig ss <*> pure (munge (deBruijnMu xs) ts) where
>     jig :: P Tok Tok
>     jig = teq (Sym ".") *>
>           (shf xs <$> some (lid <* teq (Sym ".")) <*> pRest)
>     shf xs [] ts = B Rnd (wang xs : KW "in" : Spc " " :
>                           munge (deBruijnMu xs) ts)
>     shf xs (y : ys) ts = shf (y : xs) ys ts
>     wang xs = L "let"  ([Ope Crl] :
>                        intersperse [Semi] (zipWith dang xs [0..]) ++
>                        [[Clo Crl]])
>     dang x i = [Lid x, Sym "=", Lit (show i), Sym "::", T Ty [Uid "Int"]]
> deBruijnMu _ _ = Nothing

> deBruijn :: [[Tok]] -> [[Tok]]
> deBruijn = map (munge (deBruijnMu []))
