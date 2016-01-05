monadicUnification :: (BindingMonad FTS (STVar s FTS)
(ST.STBinding s))
=> (forall s. ((Fix FTS) -> (Fix FTS) ->
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)),
            Map VariableName (ST.STVar s (FTS)))))
monadicUnification t1 t2 = do
--  let
--    t1f = termFlattener t1
--    t2f = termFlattener t2
  (x1,d1) <- lift . translateToUTerm $ t1
  (x2,d2) <- lift . translateToUTerm $ t2
  x3 <- U.unify x1 x2
  --get state from somehwere, state -> dict
  return $! (x3, d1 `Map.union` d2)


goUnify ::
  (forall s. (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  =>
      (ErrorT
          (UT.UFailure FTS (ST.STVar s FTS))
          (ST.STBinding s)
          (UT.UTerm FTS (ST.STVar s FTS),
             Map VariableName (ST.STVar s FTS)))
     )
  -> [(VariableName, Prolog)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test --ERROR
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> f1 dict


f1 ::
  (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  => (forall s. Map VariableName (STVar s FTS)
      -> (ST.STBinding s [(VariableName, Prolog)])
     )
f1 dict = do
  let ld1 = Map.toList dict
  ld2 <- Control.Monad.Error.sequence
  [ v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3,
      let v = translateFromUTerm dict v2 ]
  return ld4
  unifierConvertor :: [(VariableName, Prolog)] -> Unifier
  unifierConvertor xs = Prelude.map (\(v, p) -> (v, (unFlatten $ unP $ p))) xs

unify :: MonadPlus m => Term -> Term -> m Unifier
unify t1 t2 = unifierConvertor (goUnify (monadicUnification (termFlattener t1) (termFlattener t2)))

