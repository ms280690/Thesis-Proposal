variableExtractor :: Fix FlatTerm -> [Fix FlatTerm]
variableExtractor (Fix x) = case x of
   (Struct _ _) ->  Foldable.foldMap variableExtractor x
   (Var v)      ->  [Fix $ Var v]
   _            ->  []

variableNameExtractor :: Fix FlatTerm -> [VariableName]
variableNameExtractor (Fix x) = case x of
   (Struct _ _)    -> Foldable.foldMap variableNameExtractor x
   (Var v)         -> [v]
   _               -> []

variableSet :: [Fix FlatTerm] -> S.Set (Fix FlatTerm)
variableSet a = S.fromList a

variableNameSet :: [VariableName] -> S.Set (VariableName)
variableNameSet a = S.fromList a

varsToDictM :: (Ord a, Unifiable t) =>
    S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
varsToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- freeVar
    return $! Map.insert sv iv dict
