vTermify :: Map Int VariableName ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)) ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm))
vTermify dict t1 = case t1 of
  UT.UVar x  -> maybe (error "logic") (UT.UTerm . Var) $
                         Map.lookup (UT.getVarID x) dict
  UT.UTerm r ->
    case r of
      Var iv   -> t1
      _         -> UT.UTerm . fmap (vTermify dict) $ r

translateFromUTerm ::
    Map VariableName (ST.STVar s (FlatTerm)) ->
    UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)) -> Prolog
translateFromUTerm dict uTerm =
  P .  maybe (error "Logic") id . freeze . vTermify varIdDict $ uTerm where
    rot3 f a k v = f k v a
    inserter k v = Map.insert (UT.getVarID v) k
    forKV dict initial fn = Map.foldlWithKey' (rot3 fn) initial dict
    varIdDict = forKV dict Map.empty inserter
