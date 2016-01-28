type USTerm t s = UTerm t (ST.STVar s t)

uTermify :: Map VariableName (ST.STVar s FTS) -> USTerm FTS s -> UTerm FTS s
uTermify varMap ux = case ux of
  UT.UVar _         -> ux
  UT.UTerm (FV v)   -> maybe (error "bad map") UT.UVar $ Map.lookup v varMap
  UT.UTerm t        -> UT.UTerm $! fmap (uTermify varMap) t

translateToUTerm ::
    Fix FTS -> ST.STBinding s
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
translateToUTerm e1Term = do
  let vs = variableNameSet $ variableNameExtractor e1Term
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . unfreeze $ e1Term
  return (t2,varMap)


-- | vTermify recursively converts @UVar x@ into @UTerm (VarA x).
-- This is a subroutine of @ translateFromUTerm @.  The resulting
-- term has no (UVar x) subterms.

helper :: Map Int VariableName -> ST.STVar s FTS -> USTerm FTS s
helper dict v  = maybe (error "logic") (UT.UTerm . FV) $
                 Map.lookup (UT.getVarID v) dict

vTermify :: Map Int VariableName -> USTerm FTS s -> USTerm FTS s
vTermify dict t1 = vTermify2 (helper dict) t1 where
  vTermify2 f t1 = case t1 of
    UT.UVar x  -> f x
    UT.UTerm r -> UT.UTerm . fmap (vTermify2 f) $ r

reverseDict :: Map VariableName (SVar s) -> Map Int VariableName
reverseDict dict = varIdDict where
  forKV dict initial fn = Map.foldlWithKey' (\a k v -> fn k v a) initial dict
  varIdDict = forKV dict Map.empty $ \ k v -> Map.insert (UT.getVarID v) k

translateFromUTerm ::
    Map VariableName (ST.STVar s (FTS)) -> USTerm FTS s -> Prolog
translateFromUTerm dict =
  P .  maybe (error "Logic") id . freeze . vTermify (reverseDict dict)

