instance Functor (FTS) where
  fmap                      = T.fmapDefault
instance Foldable (FTS) where
  foldMap                   = T.foldMapDefault
instance Traversable (FTS) where
    traverse f (FS atom xs) =   FS atom <$> sequenceA (Prelude.map f xs)
    traverse _ (FV v)       =   pure (FV v)
    traverse _ FW           =   pure (FW)
    traverse _ (FC i)       =   pure (FC i)
instance Unifiable (FTS) where
  zipMatch (FS al ls) (FS ar rs) =
        if (al == ar) && (length ls == length rs)
        then FS al <$> pairWith (\l r -> Right (l,r)) ls rs else Nothing
  zipMatch FW _                 = Just FW
  zipMatch _ FW                 = Just FW
  zipMatch (FC i1) (FC i2)      =
         if (i1 == i2) then Just (FC i1) else Nothing
instance Applicative (FTS) where
  pure x                  =   FS "" [x]
  _         <*>   FW      =   FW
  _       <*>   (FC i)    =   FC i
  _       <*>   (FV v)    = (FV v)
  (FS a fs) <*> (FS b xs) = FS (a ++ b) [f x | f <- fs, x <- xs]
