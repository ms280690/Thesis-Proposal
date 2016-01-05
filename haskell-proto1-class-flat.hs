    instance Functor (FlatTerm) where
      fmap = T.fmapDefault

    instance Foldable (FlatTerm) where
      foldMap = T.foldMapDefault

    instance Traversable (FlatTerm) where
       traverse f (Struct atom x)   =   Struct atom <$>
          sequenceA (Prelude.map f x)
       traverse _ (Var v)   =   pure (Var v)
       traverse _ Wildcard  =   pure (Wildcard)
       traverse _ (Cut i)   =    pure (Cut i)

    instance Applicative (FlatTerm) where
       pure x = Struct "" [x]
       _ <*> Wildcard  =   Wildcard
       _ <*> (Cut i)   =   Cut i
       _ <*> (Var v)   =   (Var v)
       (Struct a fs) <*> (Struct b xs)
                       = Struct (a ++ b) [f x | f <- fs, x <- xs]
