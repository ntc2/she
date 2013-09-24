> module ShesHers where

> import HaLay

> bigString :: String
> bigString = unlines
>   [ "module ShesHers where"
>   , "  class Functor f where"
>   , "    fmap"
>   , ""
>   , "  class Applicative f where"
>   , "    pure, (<*>)"
>   , "    instance Functor f where"
>   , "      fmap = (<*>) . pure"
>   , ""
>   , "  class Monad f where"
>   , "    return, (>>=), (>>), fail where"
>   , "    instance Applicative f where"
>   , "      pure = return"
>   , "      ff <*> sf = ff >>= \\ f -> sf >>= \\ s -> return (f s)"
>   , ""
>   , "  class Foldable f where"
>   , "    fold, foldMap, foldr, foldl, foldr1, foldl1"
>   , ""
>   , "  class Traversable f where"
>   , "    traverse, sequenceA, mapM, sequence"
>   , "    instance Foldable f where"
>   , "      foldMap = foldMapDefault"
>   , "    instance Functor f where"
>   , "      fmap = fmapDefault"
>   , ""
>   , "  class Alternative f where"
>   , "    empty, (<|>)"
>   , ""
>   , "  class MonadPlus f where"
>   , "    mzero, mplus"
>   , "    instance Alternative f where"
>   , "      empty = mzero"
>   , "      (<|>) = mplus"
>   , ""
>   ]

> shesHers :: [[Tok]]
> shesHers = ready "ShesHers.hers" bigString
