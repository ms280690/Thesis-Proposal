instance Unifiable (FlatTerm) where
   zipMatch (Struct al ls) (Struct ar rs) =
     if (al == ar) && (length ls == length rs)
     then Struct al <$>
       pairWith (\l r -> Right (l,r)) ls rs
     else Nothing
   zipMatch Wildcard _ = Just Wildcard
   zipMatch _ Wildcard = Just Wildcard
   zipMatch (Cut i1) (Cut i2) = if (i1 == i2)
     then Just (Cut i1)
     else Nothing
