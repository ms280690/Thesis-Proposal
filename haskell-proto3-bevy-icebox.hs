monadicUnification :: (BindingMonad FTS (STVar s FTS) (ST.STBinding s)) => 
  (forall s. (Term -> Term -> ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
      (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)),
          Map Id (ST.STVar s (FTS)))))
monadicUnification t1 t2 = do
  let
    t1f = termFlattener t1
    t2f = termFlattener t2
  (x1,d1) <- lift . translateToUTerm $ t1f
  (x2,d2) <- lift . translateToUTerm $ t2f
  x3 <- U.unify x1 x2
  return $! (x3, d1 `Map.union` d2)

goUnify ::
  (forall s. (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  => (ErrorT
        (UT.UFailure FTS (ST.STVar s FTS))
        (ST.STBinding s)
        (UT.UTerm FTS (ST.STVar s FTS),
            Map Id (ST.STVar s FTS))))
  -> [(Id, Prolog)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test --ERROR
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> f1 dict

f1 :: (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  => (forall s. Map Id (STVar s FTS)
      -> (ST.STBinding s [(Id, Prolog)]))
f1 dict = do
  let ld1 = Map.toList dict
  ld2 <- sequence [ v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3, let v = translateFromUTerm dict v2 ]
  return ld4

substConvertor :: [(Id, Prolog)] -> [Subst]
substConvertor xs = Prelude.map (\(varId, p) -> (->-) varId 
                              (unFlatten $ unP $ p)) xs 

unify t1 t2 = substConvertor (goUnify (monadicUnification t1 t2))

varX :: Term
varX = Var (0,"x")

varY :: Term
varY = Var (1,"y")


