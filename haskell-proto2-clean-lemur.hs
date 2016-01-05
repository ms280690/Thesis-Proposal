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


uTermify
  :: Map VariableName (ST.STVar s (FTS))
  -> UTerm FTS (ST.STVar s (FTS))
  -> UTerm FTS (ST.STVar s (FTS))
uTermify varMap ux = case ux of
  UT.UVar _             -> ux
  UT.UTerm (FV v)       -> maybe (error "bad map") UT.UVar $ Map.lookup v varMap
 -- UT.UTerm t            -> UT.UTerm $! fmap (uTermify varMap) t
  UT.UTerm (FS a xs)    -> UT.UTerm $ FS a $! fmap (uTermify varMap) xs
  UT.UTerm (FW)         -> UT.UTerm FW
  UT.UTerm (FC i)       -> UT.UTerm (FC i)

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

vTermify :: Map Int VariableName ->
            UT.UTerm (FTS) (ST.STVar s (FTS)) ->
            UT.UTerm (FTS) (ST.STVar s (FTS))
vTermify dict t1 = case t1 of
  UT.UVar x  -> maybe (error "logic") (UT.UTerm . FV) $ Map.lookup (UT.getVarID x) dict
  UT.UTerm r ->
    case r of
      FV iv   -> t1
      _       -> UT.UTerm . fmap (vTermify dict) $ r

translateFromUTerm ::
    Map VariableName (ST.STVar s (FTS)) ->
    UT.UTerm (FTS) (ST.STVar s (FTS)) -> Prolog
translateFromUTerm dict uTerm =
  P .  maybe (error "Logic") id . freeze . vTermify varIdDict $ uTerm where
    forKV dict initial fn = Map.foldlWithKey' (\a k v -> fn k v a) initial dict
    varIdDict = forKV dict Map.empty $ \ k v -> Map.insert (UT.getVarID v) k


-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
--
-- NB !!!!
-- The current interface assumes that the variables in t1 and t2 are
-- disjoint.  This is likely a mistake that needs fixing

unifyTerms :: Fix FTS -> Fix FTS -> Maybe (Map VariableName (Prolog))
unifyTerms t1 t2 = ST.runSTBinding $ do
  answer <- runExceptT $ unifyTermsX t1 t2
  return $! either (const Nothing) Just answer

-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
--
-- This routine works in the unification monad

unifyTermsX ::
    (Fix FTS) -> (Fix FTS) ->
    ExceptT  (UT.UFailure (FTS) (ST.STVar s (FTS)))
        (ST.STBinding s)
        (Map VariableName (Prolog))
unifyTermsX t1 t2 = do
    (x1,d1) <- lift . translateToUTerm $ t1
    (x2,d2) <- lift . translateToUTerm $ t2
    _ <- U.unify x1 x2
    makeDicts $ (d1,d2)

mapWithKeyM :: (Ord k,Applicative m,Monad m)
               => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM = Map.traverseWithKey


makeDict ::
            Map VariableName (ST.STVar s (FTS)) -> ST.STBinding s (Map VariableName (Prolog))
makeDict sVarDict =
    flip mapWithKeyM sVarDict $ \ _ -> \ iKey -> do
        Just xx <- UT.lookupVar $ iKey
        return $! (translateFromUTerm sVarDict) xx


-- | recover the bindings for the variables of the two terms
-- unified from the monad.

makeDicts ::
    (Map VariableName (ST.STVar s (FTS)), Map VariableName (ST.STVar s (FTS))) ->
    ExceptT  (UT.UFailure (FTS) (ST.STVar s (FTS)))
    (ST.STBinding s) (Map VariableName (Prolog))
makeDicts (svDict1, svDict2) = do
  let svDict3 = (svDict1 `Map.union` svDict2)
  let ivs = Prelude.map UT.UVar . Map.elems $ svDict3
  applyBindingsAll ivs
  -- the interface below is dangerous because Map.union is left-biased.
  -- variables that are duplicated across terms may have different
  -- bindings because `translateToUTerm` is run separately on each
  -- term.
  lift . makeDict $ svDict3
