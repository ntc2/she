> module Patsy where

> import Data.Char
> import Data.List
> import Control.Applicative

> import Parsley
> import HaLay

> type Patsy = (String, ([String], [Tok]))

> pPatsy :: P Tok Patsy
> pPatsy = teq (KW "pattern") *> spc *>
>   ((,) <$> uid
>        <*> ((,) <$> many (spc *> lid) <* (spc <* teq (Sym "=") <* spc)
>                 <*> pRest))

> getPatsy :: [Tok] -> (([Patsy], [[Tok]]), [Tok])
> getPatsy cs = case parse pPatsy cs of
>   Just p -> (([p], [cs,[NL ("Dunno.lhs", 0)]]), jigger p)
>   _      -> (([], []), cs)

> jigger :: Patsy -> [Tok]
> jigger (s, (as, cs)) =
>   Lid ("-- patsy" ++ s) : (as >>= \a -> [Spc " ", Lid a]) ++
>   [Spc " ", Sym "=", Spc " "] ++ cs

> subst :: [(String, Tok)] -> Tok -> Tok
> subst xcs (Lid x) = case lookup x xcs of
>   Just c -> c
>   Nothing -> Lid x
> subst xcs s | isSpcT s = Spc " "
> subst xcs (B Rnd cs) = B Rnd (map (subst xcs) cs)
> subst xcs (B Sqr cs) = B Sqr (map (subst xcs) cs)
> subst xcs (B Crl cs) = B Crl (recs False cs) where
>   recs _ [] = []
>   recs False (Sym "=" : cs) = Sym "=" : recs True cs
>   recs True (Sym "," : cs) = Sym "," : recs False cs
>   recs False (c : cs) = c : recs False cs
>   recs True (c : cs) = subst xcs c : recs True cs
> subst xcs c = c

> args :: [Patsy] -> [String] -> [Tok] ->
>         ([(String, Tok)], [String], [Tok])
> args ps [] cs = ([], [], cs)
> args ps xs [] = ([], xs, [])
> args ps xs (c : cs) | isSpcT c = args ps xs cs
> args ps (x : xs) (Lid a : Sym "@" : B Rnd cs : ds) =
>   let (xcs, ys, cs') = args ps xs ds
>   in  ((x, B Rnd [Lid a, Sym "@", B Rnd (processes ps cs)]) : xcs, ys, cs')
> args ps (x : xs) (c : cs) = case process ps c of
>   Just c' -> let (xcs, ys, cs') = args ps xs cs in ((x, c') : xcs, ys, cs')
>   _ -> ([], x : xs, c : cs)

> abstract :: [String] -> [Tok] -> Tok
> abstract [] [Lid y] = Lid y
> abstract [] cs =  B Rnd cs
> abstract xs cs =  B Rnd $
>     [Sym "\\", Spc " "] ++
>     concat [[Lid x, Spc " "] | x <- xs] ++
>     [Sym "->", Spc " "] ++ cs

> process :: [Patsy] -> Tok -> Maybe Tok
> process ps (Lid y) = Just (Lid y)
> process ps (Uid c) = case lookup c ps of
>   Just (xs, cs) -> Just (abstract xs (map (subst []) cs))
>   Nothing -> Just (Uid c)
> process ps (B Rnd cs) = Just $ B Rnd (processes ps cs)
> process ps (B Sqr cs) = Just $ B Sqr (processes ps cs)
> process ps (Lit s) = Just (Lit s)
> process ps c = Nothing

> processes :: [Patsy] -> [Tok] -> [Tok]
> processes ps = munge big where
>   big (Uid c : cs) = case lookup c ps of
>     Just (xs, bs) ->
>       let (xcs, ys, ds) = args ps xs cs
>       in  Just $ abstract ys (map (subst xcs) bs) : munge big ds
>     _ -> Nothing
>   big (Lid l : cs) = Just $ Lid l : wee cs
>   big (B b cs : ds) = Just $ B b (munge big cs) : wee ds
>   big (T Ty cs : ds) = Just $ T Ty (munge off cs) : munge big ds
>   big _ = Nothing
>   wee [] = []
>   wee (c : cs) | isSpcT c = c : wee cs
>   wee (c : cs) = case process ps c of
>     Just c' -> c' : wee cs
>     Nothing -> munge big (c : cs)
>   off (T Ex cs : ds) = Just $ T Ex (munge big cs) : munge off ds
>   off _ = Nothing

> freeCons :: Tok -> [String]
> freeCons (Uid s) = [s]
> freeCons (B _ cs) = nub (cs >>= freeCons)
> freeCons _ = []

> topSort :: Eq x => (y -> [x]) -> [(x, y)] -> Maybe [(x, y)]
> topSort fr [] = Just []
> topSort fr (xy : xys) =
>   do xys <- topSort fr xys
>      topInsert xy xys
>   where
>     topInsert (x, y) xys@(h@(_, y') : t)
>       | any ((`elem` (fr y)) . fst) xys
>       = if elem x (fr y') then Nothing else (h :) <$> topInsert (x, y) t
>     topInsert xy xys = Just (xy : xys)

> prep1 :: [Patsy] -> Patsy -> [Patsy]
> prep1 ps (p, (xs, cs)) = (p, (xs, processes ps cs)) : ps

> prepare :: [Patsy] -> Maybe [Patsy]
> prepare ps0 = do
>   ps1 <- topSort ((>>= freeCons) . snd) ps0
>   return $ foldl prep1 [] ps1

