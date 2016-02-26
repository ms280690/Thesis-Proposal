termFlattener :: Term -> Fix FTS
termFlattener = DFF.ana oneLevel where
  oneLevel :: Term -> FTS Term
  oneLevel x = case x of
    { Var v ->   FV v ; Wildcard -> FW ; Cut i -> FC i ;
      Struct a xs ->  FS a xs }

unFlatten :: Fix FTS -> Term
unFlatten = DFF.cata levelOne where
  levelOne :: FTS Term -> Term
  levelOne x = case x of
    { FV v -> Var v ; FW -> Wildcard ; FC i -> Cut i ;
      FS a xs -> Struct a xs }


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


