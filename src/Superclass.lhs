> module Superclass where

> import Data.List
> import Control.Applicative
> import Data.Foldable hiding (elem, any)

> import Parsley
> import HaLay

In a .hers file, the stored class info will look like

  class C x1 .. xn where
    meth1 .. methn  -- method names
    instance CXT1 => S t1 .. tm where   -- default superclass instances
      stuff -- classified by method

In the source, whenever we see

  instance CXT0 => C s1 .. sn where
    f1
    ...
    fn
    hiding instance S' -- just the name

we should determine which instance declarations to generate: certainly

  instance CXT0 => C s1 .. sn where
    fi  -- whichever belong to the class

preceded by whichever of the Ss are not hidden. For each such

  instance CXT1 => S t1 .. tm where
    stuff

process

  instance (CXT0, [si/xi]CXT1) => S [si/xi]t1 .. where
    [si/xi]stuff

in the same way.

Whenever we see

  class CXT => C x1 .. xn where
    meth1
    ...
    methn
    instance S y1 .. ym where  -- the ys are a subset of the xs
      stuff

we must generate Haskell code

  class CXT => C x1 .. xn where
    meth1
    ...
    methn

and an entry in the .hers file

  class C x1 .. xn where
    meth1 .. methn
    instance S y1 .. ym where
      stuff

Note that (contra to email proposal), that separates instance subordination
from superclass status.

> data ClassInfo = ClassInfo
>   {  classParams :: [String]
>   ,  classCxt :: [Tok]
>   ,  classStuff :: [String]
>   ,  classInstances :: [Instance]
>   }  deriving Show

> data Instance = Instance
>   {  instanceRawHeader :: [Tok]
>   ,  instanceClass :: String
>   ,  instanceArgs :: [Tok]
>   ,  instanceCxt :: [Tok]
>   ,  instanceMethods :: [[Tok]]
>   ,  instanceHiding :: [String]
>   }  deriving Show

> type Classes = [(String, ClassInfo)]

> isNool :: [Tok] -> Bool
> isNool (NL _ : _) = True
> isNool _ = False

> classy :: [Tok] -> (([(String, ClassInfo)], [[Tok]]), [[Tok]])
> classy ts@(KW "class" : T Ty h : L "where" tss : rest) = case parse pHdr h of
>     Nothing -> (([], []), [ts])
>     Just (cx, cl, xs) -> (([(cl, ClassInfo
>       {  classParams = xs
>       ,  classCxt = cx
>       ,  classStuff = residue >>= declares
>       ,  classInstances = filtrate >>= instancy
>       })], [KW "class" : T Ty ([Spc " ", Uid cl, Spc " "] ++
>               foldMap (\s -> [Lid s, Spc " "]) xs) :
>               L "where" (nl : dstuff : nl : filtrate) : rest,
>             [NL ("Dunno.lhs", 0)]])
>          , [KW "class" : T Ty h : L "where" residue : rest])
>   where
>     pHdr = (,,) <$> pCxt <* spc <*> uid <*> some (spc *> lid) <* pRest
>     pCxt = some (tok (/= Sym "=>")) <* teq (Sym "=>")
>           <|> [] <$ spc
>     nl = nool tss
>     nool [] = []
>     nool (t : _) | isNool t = t
>     nool (_ : tss) = nool tss
>     filtrate = filter (\ ts -> isNool ts || isInstance ts) tss
>     residue = filter (not . isInstance) tss
>     stuff = residue >>= declares
>     dstuff = intercalate [Sym ",", Spc " "] (map (return . Lid) stuff)
>     isInstance (KW "instance" : _) = True
>     isInstance _ = False
> classy ts = (([], []), [ts])

> declares :: [Tok] -> [String]
> declares ts = case inf ts of
>     Just s -> [s]
>     Nothing -> thug ts
>   where
>     inf (Sym s : _) | elem s [",", "=", "::"] = Nothing
>     inf (Sym (':' : _) : ts) = inf ts
>     inf (Sym s : _) = Just ("(" ++ s ++ ")")
>     inf (Uid _ : ts) = inf ts
>     inf (Lid _ : ts) = inf ts
>     inf (t : ts) | isSpcT t = inf ts
>     inf (Com _ : ts) = inf ts
>     inf (B _ _ : ts) = inf ts
>     inf _ = Nothing
>     thug (t : ts) | isSpcT t = thug ts
>     thug (Lid f : ts) = f : commamore ts
>     thug (B Rnd [Sym s] : ts) = ("(" ++ s ++ ")") : commamore ts
>     thug _ = []
>     commamore (Sym "," : ts) = thug ts
>     commamore (t : ts) | isSpcT t = commamore ts
>     commamore _ = []

> instancy :: [Tok] -> [Instance]
> instancy (KW "instance" : T Ty h : L "where" tss : _) = case parse pHdr h of
>     Nothing -> []
>     Just (cx, cl, as) -> [Instance
>       {  instanceRawHeader = h
>       ,  instanceClass = cl
>       ,  instanceArgs = as
>       ,  instanceCxt = cx
>       ,  instanceMethods = filter (not . isHiding) tss
>       ,  instanceHiding = tss >>= hidings
>       }]
>   where
>     pHdr = (,,) <$> pCxt <* spc <*> uid <*> some (spc *> tok argT) <* spc
>     argT (Lid _) = True
>     argT (Uid _) = True
>     argT (B _ _) = True
>     argT _ = False
>     pCxt = some (tok (/= Sym "=>")) <* teq (Sym "=>")
>           <|> [] <$ spc
>     isHiding (KW "hiding" : _) = True
>     isHiding _ = False
>     hidings (KW "hiding" : ts) =
>       case parse (spc *> teq (KW "instance") *> spc *> 
>                    pTag Ty (spc *> uid <* pRest) <* pRest) ts of
>         Nothing -> []
>         Just s -> [s]
>     hidings _ = []
> instancy _ = []

> splinstance :: [(String, ClassInfo)] -> Instance -> [Instance]
> splinstance cs i = case lookup (instanceClass i) cs of
>   Nothing -> [i]
>   Just c -> let hs = instanceHiding i
>                 sups = filter (\ j -> not (elem (instanceClass j) hs))
>                          (classInstances c)
>                 sbst = zip (classParams c) (instanceArgs i)
>                 (generated, residue) =
>                    growDefaults cs hs (instanceCxt i) sbst sups (instanceMethods i)
>             in  i {instanceMethods = residue, instanceHiding = []} : generated

> growDefaults :: [(String, ClassInfo)] -> [String] ->
>                 [Tok] -> [(String, Tok)] -> [Instance] -> [[Tok]] ->
>                 ([Instance], [[Tok]])
> growDefaults cs hs cxt sbst [] tss = ([], tss)
> growDefaults cs hs cxt sbst (i : is) tss0 =
>   let (j, tss1) = growDefault cs hs cxt sbst i tss0
>       (js, tss2) = growDefaults cs hs cxt sbst is tss1
>   in  (j ++ js, tss2)

> owns :: [(String, ClassInfo)] -> String -> [String]
> owns cs c = case lookup c cs of
>       Nothing -> []
>       Just c' -> classStuff c' ++ (classInstances c' >>= (owns cs . instanceClass))
>

> growDefault :: [(String, ClassInfo)] -> [String] ->
>                [Tok] -> [(String, Tok)] -> Instance -> [[Tok]] ->
>                ([Instance], [[Tok]])
> growDefault cs hs cx0 sbst (Instance
>   {  instanceClass = c
>   ,  instanceArgs = as
>   ,  instanceCxt = cx1
>   ,  instanceMethods = uss
>   ,  instanceHiding = h
>   }) tss = (is, wss) where
>     want = owns cs c
>     relevant w ts = any (\ s -> elem s w) (declares ts)
>     vss = filter (\ ts -> isNool ts || relevant want ts) tss
>     explicit = vss >>= declares
>     wss = filter (\ ts -> isNool ts || not (relevant want ts)) tss
>     uss' = map (munge (sbMu sbst)) $
>             filter (\ us -> isNool us || not (relevant explicit us)) uss
>     is = splinstance cs $ Instance
>       {  instanceClass = c
>       ,  instanceRawHeader = []
>       ,  instanceArgs = munge (sbTyMu sbst) as
>       ,  instanceCxt = [B Rnd (mergeCxt cx0 (munge (sbTyMu sbst) cx1))]
>       ,  instanceMethods = (uss' ++ vss)
>       ,  instanceHiding = hs ++ h
>       }

> mergeCxt :: [Tok] -> [Tok] -> [Tok]
> mergeCxt [] us = us
> mergeCxt (t : ts) us | isSpcT t = mergeCxt ts us
> mergeCxt (B Rnd ts : _) us = glomCxt ts us
> mergeCxt ts us = glomCxt ts us

> glomCxt :: [Tok] -> [Tok] -> [Tok]
> glomCxt ts [] = ts
> glomCxt ts (u : us) | isSpcT u = mergeCxt ts us
> glomCxt ts (B Rnd us : _) = ts ++ [Sym ",", Spc " "] ++ us
> glomCxt ts us = ts ++ [Sym ",", Spc " "] ++ us

> sbMu :: [(String, Tok)] -> [Tok] -> Maybe [Tok]
> sbMu sb (T Ty us : ts) = Just $ T Ty (munge (sbTyMu sb) us) : munge (sbMu sb) ts
> sbMu sb _ = Nothing

> sbTyMu :: [(String, Tok)] -> [Tok] -> Maybe [Tok]
> sbTyMu sb (Lid s : ts) = (:) <$> lookup s sb <*> Just (munge (sbTyMu sb) ts)
> sbTyMu sb _ = Nothing

> blatInstance :: Instance -> [[Tok]]
> blatInstance (Instance
>   {  instanceRawHeader = h
>   ,  instanceClass = c
>   ,  instanceArgs = as
>   ,  instanceCxt = cx
>   ,  instanceMethods = tss
>   }) =
>   [KW "instance" : T Ty hdr : [L "where" (redent (dental tss) tss)] , dental []]
>   where
>     hdr = case h of
>       [] -> Spc " " : cx ++ [Spc " ", Sym "=>", Spc " ", Uid c] ++
>               foldMap (\ t -> [Spc " ", t]) as ++ [Spc " "]
>       _ -> h

> makeInstances :: [Tok] -> [(String, ClassInfo)] -> [[Tok]] -> [[Tok]]
> makeInstances nl cs [] = []
> makeInstances nl cs (ts : tss) = case instancy ts of
>   [] -> ts : makeInstances nl cs tss
>   is -> redent nl (is >>= splinstance cs >>= blatInstance)
>          ++ makeInstances nl cs tss

> superclass :: [Tok] -> [[Tok]] -> [[Tok]] -> ([[Tok]], [[Tok]])
> superclass nl hers hs0 = (makeInstances nl (cs ++ cs') hs1, nh) where
>   ((cs', nh), hs1) = foldMap classy hs0
>   ((cs, _), _) = foldMap classy hers

