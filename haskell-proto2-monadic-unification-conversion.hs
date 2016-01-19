termFlattener :: Term -> Fix FTS
termFlattener (Var v)           =   DFF.Fix $ FV v
termFlattener (Wildcard)        =   DFF.Fix FW
termFlattener (Cut i)           =   DFF.Fix $ FC i
termFlattener (Struct a xs)     =   DFF.Fix $ FS a (Prelude.map termFlattener xs)

unFlatten :: Fix FTS -> Term
unFlatten (DFF.Fix (FV v))      =   Var v
unFlatten (DFF.Fix FW)          =   Wildcard
unFlatten (DFF.Fix (FC i))      =   Cut i
unFlatten (DFF.Fix (FS a xs))   =   Struct a (Prelude.map unFlatten xs)


variableExtractor :: Fix FTS -> [Fix FTS]
variableExtractor (Fix x) = case x of
  (FS _ xs)   ->  Prelude.concat $ Prelude.map variableExtractor xs
  (FV v)     ->  [Fix $ FV v]
  _       ->  [] 

variableNameExtractor :: Fix FTS -> [VariableName]
variableNameExtractor (Fix x) = case x of
  (FS _ xs) -> Prelude.concat $ Prelude.map variableNameExtractor xs
  (FV v)     -> [v]
  _         -> [] 

variableSet :: [Fix FTS] -> S.Set (Fix FTS)
variableSet a = S.fromList a

variableNameSet :: [VariableName] -> S.Set (VariableName)
variableNameSet a = S.fromList a

varsToDictM :: (Ord a, Unifiable t) =>
    S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
varsToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- freeVar
    return $! Map.insert sv iv dict


