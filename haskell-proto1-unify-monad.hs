monadicUnification ::
  (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s))
  => (forall s. ((Fix FlatTerm) -> (Fix FlatTerm)
  ->  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
             (ST.STBinding s) (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
                               Map VariableName (ST.STVar s (FlatTerm)))))
monadicUnification t1 t2 = do
--  let
--    t1f = termFlattener t1
--    t2f = termFlattener t2
  (x1,d1) <- lift . translateToUTerm $ t1
  (x2,d2) <- lift . translateToUTerm $ t2
  x3 <- U.unify x1 x2
  --get state from somehwere, state -> dict
  return $! (x3, d1 `Map.union` d2)
