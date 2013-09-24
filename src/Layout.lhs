> {-# LANGUAGE TypeSynonymInstances #-}

> module Layout where

> import Control.Applicative
> import Data.Char
> import Data.List
> import Data.Traversable
> import Control.Monad
> import Control.Monad.State

> import Parsley

> type L = StateT (Int, String) Maybe

> fileLines :: [(Int, Chunk)] -> (Int, [[Chunk]])
> fileLines jcs = case eatSpace jcs of
>   (cs, []) -> (0, [eCook cs])
>   (cs, jcs@((j, _) : _)) ->
>    (j-1, map eCook (cs : fst (layLines (Layout Where j True) jcs)))

> ready :: String -> (Int, [[Chunk]])
> ready = fileLines . colChunks

> finished :: [[Chunk]] -> String
> finished = (>>= blows)

> instance Alternative L where
>   empty = StateT $ \ is -> empty
>   p <|> q = StateT $ \ is -> runStateT p is <|> runStateT q is

> instance Applicative L where
>   pure = return
>   (<*>) = ap

> cha :: L Char
> cha = StateT moo where
>   moo (i, []) = Nothing
>   moo (i, c : s) | isNL c = Just (c, (1, s))
>                  | otherwise = Just (c, (i + 1, s))

> stol :: L ()
> stol = do
>   i <- gets fst
>   guard (i == 1)

> chk :: (t -> Bool) -> L t -> L t
> chk p l = do t <- l ; if p t then return t else empty

> ch :: Char -> L Char
> ch c = chk (== c) cha

> spa :: (Char -> Bool) -> L String
> spa p = (:) <$> chk p cha <*> spa p  <|> pure []

> sym :: L String
> sym = StateT $ \ (i, s) -> case h s of
>   ("", _) -> Nothing
>   ("--", _) -> Nothing
>   (s, t) -> Just (s, (i + length s, t))
>  where
>   h (s@('-':'}':_)) = ("", s)
>   h (c : s) | isInfy c = fcon c (h s)
>   h s = ([], s)

> data Mood = Happy | Angry | Hungry deriving (Show, Eq)

> data Chunk
>   = SLit String
>   | Ope BK
>   | Clo BK
>   | Br BK [Chunk]
>   | Lay LayT [Chunk] [[Chunk]]
>   | LayB LayT [Chunk] [[Chunk]] Mood
>   | Type [Chunk]
>   | Kind [Chunk]
>   | Uid String
>   | Lid String
>   | KW String
>   | LK LayT
>   | Sym String
>   | Num String
>   | Comma
>   | Semi
>   | Spc String
>   | HLine String
>   | Dashed String
>   | Urk Char
>   deriving (Show, Eq)

> chunk :: L Chunk
> chunk = SLit <$> ((:) <$> ch '\"' <*> slit)
>     <|> Clo Com <$ ch '-' <* ch '}'
>     <|> HLine <$ stol <* ch '#' <*> spa (not . isNL)
>     <|> Sym <$>  sym
>     <|> Dashed <$> ((++) <$> traverse ch "--" <*> spa (not . isNL))
>     <|> Ope Rnd <$ ch '('
>     <|> Clo Rnd <$ ch ')'
>     <|> Ope Sqr <$ ch '['
>     <|> Clo Sqr <$ ch ']'
>     <|> Ope Com <$ ch '{' <* ch '-'
>     <|> Ope Crl <$ ch '{'
>     <|> Clo Crl <$ ch '}'
>     <|> Comma <$ ch ','
>     <|> Semi <$ ch ';'
>     <|> Num <$> ((:) <$> chk isDigit cha <*> spa isDigit)  -- sod FP now
>     <|> Uid <$> ((:) <$> chk isUpper cha <*> spa isIddy)
>     <|> klid <$> ((:) <$> chk isLower cha <*> spa isIddy)
>     <|> Spc  <$> ((:) <$> chk isSpace cha <*> spa isSpace)
>     <|> Urk <$> cha
>  where
>   slit = (:) <$> ch '\\' <*> ((:) <$> cha <*> slit)
>      <|> return <$> ch '\"'
>      <|> (:) <$> cha <*> slit

> colChunk :: L (Int,Chunk)
> colChunk = (,) <$> gets fst <*> chunk

> colChunks :: String -> [(Int, Chunk)]
> colChunks s = unfoldr (runStateT colChunk) (1, s)

> data LayT = Where | Of | Do | Let deriving (Show, Eq)

> data BK = Rnd | Sqr | Crl | Com deriving (Show, Eq)

> data Mode
>   = Layout LayT Int Bool -- 0 means bracey, flag is "start of line"
>   | Bracket BK
>   | TopLevel
>   deriving (Show, Eq)

> eatSpace ::  [(Int,Chunk)] -> ([Chunk], [(Int, Chunk)])
> eatSpace [] = ([], [])
> eatSpace ((_, c@(Spc _)) : jcs) = fcon c (eatSpace jcs)
> eatSpace ((_, c@(Dashed _)) : jcs) = fcon c (eatSpace jcs)
> eatSpace ((_, c@(HLine _)) : jcs) = fcon c (eatSpace jcs)
> eatSpace ((_, c@(Ope Com)) : jcs) = case grp (Bracket Com) jcs of
>   (cs, (h, jcs)) -> case eatSpace jcs of
>     (ds, jcs) -> case h of
>       Happy  -> (Br Com cs : ds, jcs)
>       _      -> (Ope Com : cs ++ ds, jcs)
> eatSpace jcs = ([], jcs)

> topLevel :: Mode
> topLevel = Layout Where 1 True

> layLines :: Mode ->  [(Int,Chunk)] -> ([[Chunk]], (Mood, [(Int, Chunk)]))
> layLines m jcs = case grp m jcs of
>   (cs, (Hungry, jcs)) -> fcon cs (layLines m jcs)
>   (cs, (m, jcs)) -> ([cs], (m, jcs))

> grp :: Mode -> [(Int,Chunk)] -> ([Chunk], (Mood, [(Int, Chunk)]))
> grp (Layout _ _ _) [] = ([], (Happy, []))
> grp _ [] = ([], (Angry, []))
> grp m jcs@((j, c) : jcs') = case (m, c) of
>   (m, Sym "#") | j == 1 -> goon
>   (m, Spc _) -> goon
>   (m, Dashed _) -> goon
>   (m, HLine _) -> goon
>   -- downers
>   (Bracket b, Clo b')
>     | b == b'    -> ([], (Happy, jcs'))
>     | otherwise  -> ([], (Angry, jcs))
>   (Layout _ 0 _, Semi) ->
>     let (cs, jcs'') = eatSpace jcs' in (c : cs, (Hungry, jcs''))
>   (Layout _ 0 _, Clo Crl) -> ([], (Happy, jcs'))
>   (Layout _ 0 _, Clo _) -> ([], (Angry, jcs))
>   (Layout _ i b, _)
>     | j == i && not b -> ([], (Hungry, jcs))
>     | j < i  -> ([], (Happy, jcs))
>   (Layout _ i _, Clo _) -> ([], (Happy, jcs))
>   (Layout _ i _, KW "in") -> ([], (Happy, jcs))
>   (Layout Of i _, LK Where) -> ([], (Happy, jcs))
>   (Layout Do i _, LK Where) -> ([], (Happy, jcs))
>   -- uppers
>   (m, Ope b) | m /= Bracket Com || b == Com ->
>     let (cs, (h, jcs'')) = grp (Bracket b) jcs'
>     in  case h of
>           Happy -> fcon (Br b cs) (grp m jcs'')
>           _ -> let (ds, jds) = grp m jcs'' in (Ope b : cs ++ ds, jds)
>   -- comments keep on trucking
>   (Bracket Com, _) -> goon
>   (m, LK l) -> case (m, layMo jcs') of
>     (Layout _ i _, (cs, (j, jcs))) | 0 < j && j <= i ->
>       ([Lay l cs []], (Hungry, jcs))
>     (m, (cs, (j, jcs))) -> case layLines (Layout l j True) jcs of
>       (css, (h, jcs)) ->
>         fcon (if j == 0 then LayB l cs css h else Lay l cs css)
>           (grp m jcs)
>   -- sideways
>   _ -> goon
>  where goon = fcon c (grp (move m) jcs')
>        move (Layout l i _) = Layout l i False
>        move m = m

> layMo :: [(Int,Chunk)] -> ([Chunk], (Int, [(Int, Chunk)]))
> layMo jcs = case eatSpace jcs of
>   (cs, []) -> ([], (1, []))
>   (cs, ((_, Ope Crl) : jcs)) ->
>     let (cs', jcs') = eatSpace jcs in (cs ++ Ope Crl : cs', (0, jcs'))
>   (cs, jcs@((i, _) : _)) -> (cs, (i, jcs))

> eCook :: [Chunk] -> [Chunk]
> eCook [] = []
> eCook (c : cs)
>   | elem c [Sym "::", KW "class", KW "instance"]
>   = c : Type ds : eCook es
>   | elem c [KW "data", KW "newtype", KW "type"]
>   = case es of
>       (Sym "=" : fs) -> c : Type ds : Sym "=" : [Type fs] -- hack!
>       _ -> c : Type ds : eCook es
>   where (ds, es) = tCook True cs
> eCook (Lay l ds dss : cs) = Lay l ds (map eCook dss) : eCook cs
> eCook (LayB l ds dss h : cs) = LayB l ds (map eCook dss) h : eCook cs
> eCook (c@(Br Com ds) : cs) = c : eCook cs
> eCook (Br b ds: cs) = Br b (eCook ds) : eCook cs
> eCook (c : cs) = c : eCook cs

> tenders :: [Chunk]
> tenders = [Semi, Sym "=", Sym ",", Sym "|", Sym ".", KW "in"]

> tCook :: Bool -> [Chunk] -> ([Chunk], [Chunk])
> tCook _ [] = ([], [])
> tCook True (cs@(Sym "," : _)) = ([], cs)
> tCook _ (cs@(c : _))
>   | elem c tenders ||
>     (case c of { Lay _ _ _ -> True ; LayB _ _ _ _ -> True ; _ -> False })
>   = ([], cs)
> tCook b (KW "forall" : cs) =
>   let (ds, es) = tCook False cs
>   in  case es of
>         Sym "." : es ->
>           let (fs, fs') = tCook b es
>           in  (KW "forall" : ds ++ Sym "." : fs, fs')
>         _ -> (KW "forall" : ds, es) -- weird
> tCook b (Br Rnd cs : es) =
>   let (ds, ds') = tCook False cs
>   in  fcon (Br Rnd (ds ++ ds')) (tCook b es)
> tCook b (Br Sqr cs : es) =
>   let (ds, ds') = tCook False cs
>   in  fcon (Br Sqr (ds ++ ds')) (tCook b es)
> tCook b (Br Crl ds : es) = fcon (Br Crl (eCook ds)) (tCook b es)
> tCook b (c@(Sym "::") : cs) =
>   let (ds, es) = kCook b cs in ([c, Kind ds], es)
> tCook b (c : cs) = fcon c (tCook b cs)

> kCook :: Bool -> [Chunk] -> ([Chunk], [Chunk])
> kCook _ [] = ([], [])
> kCook True (cs@(Sym "," : _)) = ([], cs)
> kCook _ (cs@(c : _))
>   | elem c tenders ||
>     (case c of { Lay _ _ _ -> True ; LayB _ _ _ _ -> True ; _ -> False })
>   = ([], cs)
> kCook b (KW "forall" : cs) =
>   let (ds, es) = tCook False cs
>   in  case es of
>         Sym "." : es ->
>           let (fs, fs') = kCook b es
>           in  (KW "forall" : ds ++ Sym "." : fs, fs')
>         _ -> (KW "forall" : ds, es) -- weird
> kCook b (Br Rnd cs : es) =
>   let (ds, ds') = kCook False cs
>   in  fcon (Br Rnd (ds ++ ds')) (kCook b es)
> kCook b (Br Crl cs : es) =
>   let (ds, ds') = tCook False cs
>   in  fcon (Br Crl [Type (ds ++ ds')]) (kCook b es)
> kCook b (c : cs) = fcon c (kCook b cs)

> blows :: [Chunk] -> String
> blows = (>>= blow)

> blow :: Chunk -> String
> blow (SLit s) = s
> blow (Ope bk) = ope bk
> blow (Clo bk) = clo bk
> blow (Br bk cs) = ope bk ++ blows cs ++ clo bk
> blow (Lay l cs css) = blow (LK l) ++ blows cs ++ (css >>= blows)
> blow (LayB l cs css m) = blow (LK l) ++ blows cs ++ (css >>= blows) ++
>   if m == Happy then "}" else ""
> blow (Type cs) = blows cs
> blow (Kind cs) = blows cs
> blow (Uid s) = s
> blow (Lid s) = s
> blow (KW s) = s
> blow (LK l) = case lookup l (map (\ (x, y) -> (y, x)) lkAL) of
>   Just s -> s
> blow (Sym s) = s
> blow (Num s) = s
> blow (Spc s) = s
> blow (Dashed s) = s
> blow (HLine s) = '#' : s
> blow Comma = ","
> blow Semi = ";"
> blow (Urk c) = [c]

> ope :: BK -> String
> ope Rnd = "("
> ope Sqr = "["
> ope Crl = "{"
> ope Com = "{-"

> clo :: BK -> String
> clo Rnd = ")"
> clo Sqr = "]"
> clo Crl = "}"
> clo Com = "-}"

> keywords :: [String]
> keywords = ["module", "import", "type", "data", "newtype", "pattern", "kind",
>             "let", "in", "case", "of", "do", "forall", "class", "instance",
>             "family", "where"]

> lkAL :: [(String, LayT)]
> lkAL = [("where", Where), ("do", Do), ("of", Of), ("let", Let)]

> klid :: String -> Chunk
> klid s | elem s keywords  = case lookup s lkAL of
>                               Just l  -> LK l
>                               _       -> KW s
>        | otherwise        = Lid s

> fcon :: x -> ([x], t) -> ([x], t)
> fcon x ~(xs, t) = (x : xs, t)

> scon :: x -> (t, [x]) -> (t, [x])
> scon x ~(t, xs) = (t, x : xs)

> isIddy :: Char -> Bool
> isIddy b = isAlphaNum b || elem b "_'"

> isInfy :: Char -> Bool
> isInfy b = elem b "!#$%&*+-.:/<=>?@\\^|~"

> isNL :: Char -> Bool
> isNL b = elem b "\r\n"

> spc :: P Chunk ()
> spc = () <$ pStar (grok (ok isSpc) next)

> isSpc :: Chunk -> Bool
> isSpc (Spc _) = True
> isSpc (Dashed _) = True
> isSpc (Br Com _) = True
> isSpc (HLine _) = True
> isSpc _ = False

> cleanEnd :: [Chunk] -> [Chunk]
> cleanEnd [] = [Spc "\n"]
> cleanEnd [Spc _] = [Spc "\n"]
> cleanEnd (c : cs) = c : cleanEnd cs

> uid :: P Chunk String
> uid = grok h next where
>   h (Uid s) = Just s
>   h _ = Nothing

> lid :: P Chunk String
> lid = grok h next where
>   h (Lid s) = Just s
>   h _ = Nothing

> infC :: P Chunk String
> infC = grok h next where
>   h (Sym (':' : s)) = Just (':' : s)
>   h _ = Nothing

> pType :: P Chunk x -> P Chunk x
> pType p = grok pt next where
>   pt (Type cs) = parse p cs
>   pt _ = Nothing

> pBr :: BK -> P Chunk x -> P Chunk x
> pBr k p = grok pb next where
>   pb (Br j cs) | k == j = parse p cs
>   pb _ = Nothing

> whack :: (Chunk -> Maybe Chunk) -> Chunk -> Chunk
> whack f c = case f c of
>   Just c -> c
>   Nothing -> case c of
>     Type cs -> Type (map (whack f) cs)
>     Kind cs -> Kind (map (whack f) cs)
>     Br Com cs -> Br Com cs
>     Br b cs -> Br b (map (whack f) cs)
>     Lay l cs css -> Lay l cs (map (map (whack f)) css)
>     LayB l cs css m -> LayB l cs (map (map (whack f)) css) m
>     c -> c
