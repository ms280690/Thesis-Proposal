-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
--
-- NB !!!!  The current interface assumes that the variables
-- in t1 and t2 are disjoint.  This likely needs fixing.
type SVar s = ST.STVar s (FTS)
type PrologMap = Map VariableName Prolog
type UTExcept t v m r = ExceptT (UT.UFailure t v) m r

unifyTerms :: Fix FTS -> Fix FTS -> Maybe PrologMap
unifyTerms t1 t2 = ST.runSTBinding $ do
  answer <- runExceptT $ unifyTermsX t1 t2
  return $! either (const Nothing) Just answer

-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
-- This routine works in the unification monad
unifyTermsX :: (Fix FTS) -> (Fix FTS)
               -> UTExcept FTS (SVar s) (ST.STBinding s) PrologMap
unifyTermsX t1 t2 = do
    (x1,d1) <- lift . translateToUTerm $ t1
    (x2,d2) <- lift . translateToUTerm $ t2
    _ <- U.unify x1 x2
    makeDicts $ (d1,d2)

mapWithKeyM :: (Ord k,Applicative m,Monad m)
               => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM = Map.traverseWithKey
makeDict :: Map VariableName (SVar s) -> ST.STBinding s PrologMap
makeDict sVarDict =
    flip mapWithKeyM sVarDict $ \ _ -> \ iKey -> do
        Just xx <- UT.lookupVar $ iKey
        return $! (translateFromUTerm sVarDict) xx

-- | recover the bindings for the variables of the two terms
-- unified from the monad.
makeDicts :: (Map VariableName (SVar s), Map VariableName (SVar s))
             -> UTExcept FTS (SVar s) (ST.STBinding s) PrologMap
makeDicts (svDict1, svDict2) = do
  let svDict3 = (svDict1 `Map.union` svDict2)
  let ivs = Prelude.map UT.UVar . Map.elems $ svDict3
  applyBindingsAll ivs
  lift . makeDict $ svDict3

