> module TypesToKinds where

> import Control.Applicative
> import Data.List

> import HaLay
> import Parsley

> dataGrok :: [Tok] -> [[Tok]]
> dataGrok cs@(KW "newtype" : T Ty _ : ds)
>    = map blat (fillet ds)
> dataGrok cs@(KW "data" : T Ty _ : ds)
>    = map blat (fillet ds)
> dataGrok cs = []

> blat :: (Tok, Int) -> [Tok]
> blat (c, j) =
>     [KW "data", T Ty (cxs id ++ [Spc " "]), Sym "="] ++ cxs ty where
>   cxs f = Spc " " : c :
>           ([1 .. j] >>= (\k -> [Spc " ", f (Lid ("x" ++ show k))]))
>   ty t = T Ty [t]

> fillet :: [Tok] -> [(Tok, Int)]
> fillet [] = []
> fillet (Sym "=" : cs) =
>   case parse (pSep (spc *> teq (Sym "|")) (spc *> pOldSyn)) cs of
>     Just sis -> sis
>     _ -> []
> fillet (L "where" css : _)     = css >>= gadtSyn
> fillet (_ : cs) = fillet cs

> pOldSyn :: P Tok (Tok, Int)
> pOldSyn = (\s -> (jig s, 2)) <$ pArg <* spc <*> infC <* pArg
>       <|> (,) <$> pCId
>               <* spc
>               <*> (pBr Crl pFields <|> length <$> many pArg)
>               <* spc

> jig :: String -> Tok
> jig s = (B Rnd [Sym (":$#$#$#" ++ s)])

> pArg :: P Tok ()
> pArg = spc <* (
>   (() <$ pTag Ty pRest) <|>
>   teq (Sym "!") *> pArg
>   )

> pFields :: P Tok Int
> pFields = 0 <$ pEnd
>       <|> (1 +) <$ lid <*> pFields  -- right, assuming types are chunked
>       <|> next *> pFields

> gadtSyn :: [Tok] -> [(Tok, Int)]
> gadtSyn cs = case parse pGDecl cs of
>   Just (ss, i) -> map (flip (,) i) ss
>   _ -> []

> pGDecl :: P Tok ([Tok], Int)
> pGDecl = (,) <$> pSep (spc *> teq (Sym ",")) pCId <* spc <* teq (Sym "::")
>              <*> pTag Ty pArity <* pRest

> pCId :: P Tok Tok
> pCId = Uid <$> (("SheTy" ++) <$> uid)
>    <|> jig <$> pBr Rnd (spc *> infC <* spc)

> pArity :: P Tok Int
> pArity = 0 <$ pEnd
>      <|> (1 +) <$ teq (Sym "->") <*> pArity
>      <|> next *> pArity

> pTele :: P Tok ([(String, [Tok])], [Tok])
> pTele = (,)  <$> some (spc *> pBr Rnd piB) <* spc <* teq (Sym ".")
>              <*> pRest
>              where
>    piB :: P Tok (String, [Tok])
>    piB = (,) <$ spc <*> lid <* spc <* teq (Sym "::") <* spc <*> pRest

> tyTTK :: [Tok] -> Maybe [Tok]
> tyTTK (B Crl [T Ex (Sym ":" : us)] : ts) = case parse pProxyRequest us of
>   Just (tm, ty) -> Just $
>     [Uid "SheChecks", Spc " ", B Rnd ty, Spc " ", B Rnd (munge exTTK tm)]
>     ++ munge tyTTK ts
>   _ -> Nothing
> tyTTK (B Crl [T Ex us] : ts)  = Just $ B Rnd (munge exTTK us) : munge tyTTK ts
> tyTTK (B Rnd us : ts) = (: munge tyTTK ts) <$> (sing <$> parse pSing us) where
>   sing t = B Rnd [Uid "SheSingleton", Spc " ", B Rnd t]
>   pSing = spc *> teq (Sym "::") *> pTag Ki pRest
> tyTTK (Lid "pi" : ts)         = pity <$> parse pTele ts where
>   pity :: ([(String, [Tok])], [Tok]) -> [Tok]
>   pity (xss, ts) =
>     [KW "forall", Spc " "] ++
>     (xss >>= \ (x, _) -> [Lid x, Spc " "]) ++
>     [Sym ".", Spc " "] ++
>     (xss >>= \ (x, ss) ->
>       [Uid "SheSingleton", Spc " ", B Rnd (munge tyTTK ss), Spc " ", Lid x,
>        Spc " ", Sym "->", Spc " "]) ++
>     munge tyTTK ts
> tyTTK (T Ki us : ts)          = Just $ T Ki (munge kiTTK us) : munge tyTTK ts
> tyTTK _ = Nothing

> kiTTK :: [Tok] -> Maybe [Tok]
> kiTTK (B Crl [T Ty us] : ts)  = Just $ Sym "*" : munge kiTTK ts
> kiTTK (Lid "pi" : ts)         = piki <$> parse pTele ts where
>   piki :: ([(String, [Tok])], [Tok]) -> [Tok]
>   piki (xks, ts) =
>     (xks >>= \ (_, ks) ->
>       [B Rnd (munge kiTTK ks), Spc " ", Sym "->", Spc " "]) ++
>     munge kiTTK ts
> kiTTK (KW "forall" : ts)      = case span (/= Sym ".") ts of
>   (_, _ : ts) -> Just $ munge kiTTK ts
>   _ -> Nothing
> kiTTK _ = Nothing

> ttkMu :: [Tok] -> Maybe [Tok]
> ttkMu (T Ty us : ts) = Just $ T Ty (munge tyTTK us) : munge ttkMu ts
> ttkMu (B Crl (Sym ":" : us) : ts) = case parse pProxyRequest us of
>   Just (tm, ty) -> Just $ proxyRequest tm ty : munge ttkMu ts
>   _ -> Nothing
> ttkMu (B Crl us : ts)
>   | piArg us = Just $ B Rnd (munge witMu us) : munge ttkMu ts
>   where
>     piArg xs | elem (Sym "=") xs = False
>     piArg xs | elem (Sym "::") xs = False
>     piArg _ = True
> ttkMu _ = Nothing

> pProxyRequest :: P Tok ([Tok], [Tok])
> pProxyRequest = (,) <$> some (tok (/= Sym "::")) <* teq (Sym "::") <* spc
>                     <*> pTag Ty (some (tok (/= Sym ":")) <* teq (Sym ":"))

> witMu :: [Tok] -> Maybe [Tok]
> witMu (B Sqr us : ts) = Just $ mkL (munge witMu us) : munge witMu ts where
>   mkL [] = Uid "SheSpecialWitNil"
>   mkL ts = case span (/= Sym ",") ts of
>     (ss, []) ->
>       B Rnd [B Rnd ss, Spc " ", Sym ":$%$%$%:", Spc " ", Uid "SheSpecialNil"]
>     (ss, _ : ts) ->
>       B Rnd [B Rnd ss, Spc " ", Sym ":$%$%$%:", Spc " ", mkL ts]
> witMu (Uid s : ts) = Just $ Uid ("SheWit" ++ s) : munge witMu ts
> witMu (Sym (':' : s) : ts) = Just $ Sym (":$%$%$%:" ++ s) : munge witMu ts
> witMu _ = Nothing

> exTTK :: [Tok] -> Maybe [Tok]
> exTTK (B Sqr us : ts) = Just $ mkL (munge exTTK us) : munge exTTK ts where
>   mkL [] = Uid "SheSpecialNil"
>   mkL ts = case span (/= Sym ",") ts of
>     (ss, []) ->
>       B Rnd [B Rnd ss, Spc " ", Sym ":$#$#$#:", Spc " ", Uid "SheSpecialNil"]
>     (ss, _ : ts) ->
>       B Rnd [B Rnd ss, Spc " ", Sym ":$#$#$#:", Spc " ", mkL ts]
> exTTK (Uid s : ts) = Just $ Uid ("SheTy" ++ s) : munge exTTK ts
> exTTK (Sym (':' : s) : ts) = Just $ Sym (":$#$#$#:" ++ s) : munge exTTK ts
> exTTK _ = Nothing

> typesToKinds :: [[Tok]] -> [[Tok]]
> typesToKinds = map (munge ttkMu)

> data ConTy = ConTy
>   { cName     :: String
>   , cForall   :: [Tok]
>   , cInst     :: [Tok]
>   , cArgs     :: [[Tok]]
>   , cFam      :: [Tok]
>   , cIndices  :: [Tok]
>   } deriving (Show)

> sing :: ConTy -> ConTy
> sing (ConTy
>   { cName     = c
>   , cForall   = xs
>   , cInst     = is
>   , cArgs     = as
>   , cFam      = ds
>   , cIndices  = ss
>   }) = ConTy
>   { cName     = "SheWit" ++ c
>   , cForall   = xs ++ (vs >>= (\v -> [Spc " ", Lid v]))
>   , cInst     = is 
> --  , cInst     = cook is (zipWith constra as vs)
>   , cArgs     = zipWith singa as vs
>   , cFam      = [B Rnd ([Uid "SheSingleton", Spc " "] ++ ds ++ Spc " " : ss)]
>   , cIndices  = ss  ++ [Spc " ",
>                     B Rnd  (mkPref (sheTy c) :
>                            (vs >>= (\ v -> [Spc " ", Lid v])))]
>   } where
>   vs = zipWith (\ _ i -> "sha" ++ show i) as [0..]
>   singa ts v = [Uid "SheSingleton", Spc " ", B Rnd ts, Spc " ", Lid v]
>   constra ts v = [Uid "SheChecks", Spc " ", B Rnd ts, Spc " ", Lid v]
>   cook is [] | all isSpcT is = is
>   cook (B Rnd is : _) cs = cook is cs
>   cook is cs
>     | all isSpcT is = [B Rnd (intercalate [Sym ",", Spc " "] cs)]
>     | otherwise = [B Rnd (intercalate [Sym ",", Spc " "] (is : cs))]

> conTyOut :: ConTy -> [Tok]
> conTyOut (ConTy
>   { cName     = c
>   , cForall   = xs
>   , cInst     = is
>   , cArgs     = as
>   , cFam      = ds
>   , cIndices  = ss
>   }) = [Uid c, Spc " ", Sym "::", Spc " ", T Ty (
>        fao xs ++
>        cio is ++
>        (as >>= \ ts -> ts ++ [Spc " ", Sym "->", Spc " "]) ++
>        ds ++ Spc " " : ss)]
>   where
>     fao xs | all isSpcT xs = xs
>            | otherwise = [KW "forall", Spc " "] ++ xs ++ [Sym ".", Spc " "]
>     cio xs | all isSpcT xs = xs
>            | otherwise = xs ++ [Spc " ", Sym "=>", Spc " "]

> pGConTy :: P Tok ConTy
> pGConTy = (ConTy <$ spc <*> uid <* spc <* teq (Sym "::") <* spc) >>= \f ->
>           pTag Ty (f <$> pFA <*> pCI <*> many pARG <* spc <*> (((:[]). Uid) <$> uid)
>                    <*> pRest)
>   where
>     pFA = spc *> (
>             teq (KW "forall") *> some (tok (/= Sym ".")) <* teq (Sym ".")
>             <|> [] <$ spc)
>     pCI = some (tok (/= Sym "=>")) <* teq (Sym "=>")
>           <|> [] <$ spc
>     pARG = some (tok (/= Sym "->")) <* teq (Sym "->")

> mkPref :: Tok -> Tok
> mkPref t@(Sym _) = B Rnd [t]
> mkPref t = t

> sheTy :: String -> Tok
> sheTy (':' : s) = Sym (":$#$#$#:" ++ s)
> sheTy s = Uid ("SheTy" ++ s)

> pGADT :: P Tok ((String, Int), ([ConTy], [String]))
> pGADT = (,)
>   <$   tok (`elem` [KW "data", KW "newtype"]) <* spc
>   <*>  pTag Ty ((,) <$ spc <*> uid <* spc <*>  pGArity <* pRest) <* spc
>   <*>  pLay "where" ((,) <$> pGCons <*> pDer)
>   <*   pRest
>   where
>     pGArity = length <$> many (lid <* spc)
>               <|> (teq (Sym "::") *> spc *> pTag Ki pArity)
>     pGCons = tok (all isSpcT) *> pGCons
>              <|> (:) <$> grok (parse pGConTy) next <*> pGCons
>              <|> pure []
>     pDer = grok (parse pDeriving) next <* pRest <|> pure []

> pDeriving :: P Tok [String]
> pDeriving = teq (KW "deriving") *> spc *>
>             (pure <$> uid <|>
>             pBr Rnd (spc *> pSep (spc *> teq (Sym ",") *> spc) uid <* spc))

 singGrok :: [Tok] -> [[Tok]]
 singGrok ts = case parse pGADT ts of
   Just ((s, i), (cs, ds)) | elem "SheSingleton" ds ->
    let vs = [1..i] >>= \v -> [Spc " ", Lid ("x" ++ show i)] in
    [[KW "type",
      T Ty [Spc " ", KW "instance", Spc " ", Uid "SheSingleton", Spc " ",
           B Rnd (Uid s : vs), Spc " "],
      Sym "=",
      T Ty (Spc " " : Uid ("SheSing" ++ s) : vs)],[NL ("Dunno.lhs",0)],
     [KW "data", Spc " ", Uid ("SheSing" ++ s), Spc " ", Sym "::",
      T Ki (concat (replicate i [Spc " ", Sym "*", Spc " ", Sym "->"]) ++
           [Spc " ", Sym "*", Spc " ", Sym "->", Spc " ", Sym "*", Spc " "]),
      L "where" (cs >>= \ c -> [[NL ("Dunno.lhs",0), Spc "    "], conTyOut (sing c)])],[NL ("Dunno.lhs",0)]] ++
     (cs >>= \ c -> [checkI c, [NL ("Dunno.lhs",0)]])
   _ -> []

> singGrok :: [Tok] -> [[Tok]]
> singGrok ts = case parse pGADT ts of
>   Just ((s, i), (cs, ds)) | elem "SheSingleton" ds ->
>    let vs = [1..i] >>= \v -> [Spc " ", Lid ("x" ++ show i)] in
>     [[KW "data",
>       T Ty [Spc " ", KW "instance", Spc " ", Uid "SheSingleton", Spc " ",
>            B Rnd (Uid s : vs), Spc " ", Uid "dummy", Spc " ",
>       L "where" (cs >>= \ c -> [[NL ("Dunno.lhs",0), Spc "    "], conTyOut (sing c)])]],
>      [NL ("Dunno.lhs",0)]] ++
>      (cs >>= \ c -> [checkI c, [NL ("Dunno.lhs",0)]])
>   _ -> []

> checkI :: ConTy -> [Tok]
> checkI  (ConTy
>   { cName     = c
>   , cForall   = xs
>   , cInst     = is
>   , cArgs     = as
>   , cFam      = ds
>   , cIndices  = ss
>   }) =
>   [KW "instance",
>    T Ty (Spc " " : prems ais ++
>     [Uid "SheChecks", Spc " ", B Rnd (ds ++ Spc " " : ss), Spc " ",
>      B Rnd (mkPref (sheTy c) : (ais >>= (\ (_, v) -> [Spc " ", Lid v]))),
>      Spc " "]),
>    L "where" [[Spc " "],
>      [Lid "sheTypes", Spc " ", Lid "_", Spc " ", Sym "=", Spc " ",
>       Uid ("SheWit" ++ c)] ++ (ais >>= poxy)
>      ]
>   ]
>   where
>     ais = zipWith (\ty i -> (ty, "sha" ++ show i)) as [0..]
>     prems [] = []
>     prems [tyv] = cnstr tyv ++ [Spc " " , Sym "=>", Spc " "]
>     prems (tyv : tyvs) =
>       [B Rnd (cnstr tyv ++
>               (tyvs >>= \tyv -> [Sym ",", Spc " "] ++ cnstr tyv)),
>        Spc " " , Sym "=>", Spc " "]
>     cnstr (ty, v) = [Uid "SheChecks", Spc " ", B Rnd ty, Spc " ", Lid v]
>     poxy (ty, v) = [Spc " ", proxyRequest [Lid v] ty]

> proxyRequest :: [Tok] -> [Tok] -> Tok
> proxyRequest tm ty = B Rnd [
>       Lid "sheTypes", Spc " ", B Rnd [
>         Uid "SheProxy", Spc " ", Sym "::", T Ty [Spc " ",
>           Uid "SheProxy", Spc " ", B Rnd ty, Spc " ", B Rnd (munge exTTK tm)
>       ]]]

> noDerSing :: [[Tok]] -> [[Tok]]
> noDerSing = map (munge ndsMu) where
>   ndsMu ts = case parse pDeriving ts of
>     Just xs -> Just $ mkDer (filter (/= "SheSingleton") xs)
>     _ -> Nothing
>   mkDer [] = []
>   mkDer [x] = [KW "deriving", Spc " ", Uid x]
>   mkDer (x : xs) =
>     [KW "deriving", Spc " ",
>      B Rnd (Uid x : (xs >>= \x -> [Sym ",", Spc " ", Uid x]))]
