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
  lift . makeDict $ svDict3

