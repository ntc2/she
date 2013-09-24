> {-# OPTIONS_GHC -F -pgmF she #-}
> {-# LANGUAGE TypeOperators #-}

> module Tree where

> import Fix

> type TreeF x = K () :+: (I :*: K x :*: I)
> type Tree x = Fix (TreeF x)

> pattern LeafF = Plus (Left (K ()))
> pattern NodeF l x r = Plus (Right (Times (I l, Times (K x, I r))))
> pattern Leaf = In LeafF
> pattern Node l x r = In (NodeF l x r)

> spiny :: List x -> Tree x
> spiny Nil = Leaf
> spiny (Cons x xs) = Node Leaf x (spiny xs)

> insert :: Ord x => x -> Tree x -> Tree x
> insert x = paraFix g where
>   g LeafF = Node Leaf x Leaf
>   g (NodeF (l, l') y (r, r'))
>     | x <= y     = Node l' y r
>     | otherwise  = Node l y r'

> mkTree :: Ord x => List x -> Tree x
> mkTree = foldFix alg where
>   alg NilF = Leaf
>   alg (ConsF x xs) = insert x xs

> flatTree :: Tree x -> List x
> flatTree = foldFix phi where
>   phi LeafF = Nil
>   phi (NodeF l x r) = l +++ (Cons x Nil) +++ r

> treeSort :: Ord x => List x -> List x
> treeSort = flatTree . mkTree
