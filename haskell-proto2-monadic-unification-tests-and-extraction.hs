instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}

test1 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test1 = do
    let
        t1a = (Fix $ FV $ VariableName 0 "x")
        t2a = (Fix $ FV $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)

test2 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test2 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ VariableName 0 "x"])
        t2a = (Fix $ FV $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)

test3 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test3 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ VariableName 0 "x"])
        t2a = (Fix $ FV $ VariableName 0 "x")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)

test4 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test4 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ VariableName 0 "x"])
        t2a = (Fix $ FV $ VariableName 0 "x")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unifyOccurs x1 x2
    return (x3, d1 `Map.union` d2)

test5 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test5 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ VariableName 0 "x"])
        t2a = (Fix $ FS "b" [Fix $ FV $ VariableName 0 "y"])
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)

goTest :: (Show b) => (forall s . 
  (ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS))))) -> String
goTest test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)  -> "error: " ++ show x 
    (Right y) -> "ok:    " ++ show y 

monadicUnification :: (BindingMonad FTS (STVar s FTS) (ST.STBinding s)) => 
    (forall s. ((Fix FTS) -> (Fix FTS) -> 
      ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)),
            Map VariableName (ST.STVar s (FTS)))))
monadicUnification t1 t2 = do
  (x1,d1) <- lift . translateToUTerm $ t1
  (x2,d2) <- lift . translateToUTerm $ t2
  x3 <- U.unify x1 x2
  return $! (x3, d1 `Map.union` d2)

goUnify ::
  (forall s. (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  => (ErrorT
        (UT.UFailure FTS (ST.STVar s FTS))
        (ST.STBinding s)
        (UT.UTerm FTS (ST.STVar s FTS),
            Map VariableName (ST.STVar s FTS))))
  -> [(VariableName, Prolog)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test 
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> f1 dict

f1 ::
  (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  => (forall s. Map VariableName (STVar s FTS)
      -> (ST.STBinding s [(VariableName, Prolog)]))
f1 dict = do
  let ld1 = Map.toList dict
  ld2 <- Control.Monad.Error.sequence 
          [v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3, let v = translateFromUTerm dict v2 ]
  return ld4

unifierConvertor :: [(VariableName, Prolog)] -> Unifier
unifierConvertor xs = Prelude.map (\(v, p) -> (v, (unFlatten $ unP $ p))) xs 

unify :: MonadPlus m => Term -> Term -> m Unifier
unify t1 t2 = unifierConvertor 
                (goUnify 
                  (monadicUnification 
                    (termFlattener t1) (termFlattener t2)
                  )
                )

--unify_with_occurs_check 

{--
goTest test3
"ok:    STVar -9223372036854775807 
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}


{--
goTest test4
"ok:    STVar -9223372036854775807 
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

{--
fix1 = (Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), 
  (Fix Wildcard), (Fix $ Cut 0), (Fix $ Struct "b" 
    [(Fix $ Var $ VariableName 1 "y"), (Fix Wildcard), 
    (Fix $ Cut 1), (Fix $ Struct "c" [(Fix $ Var $ VariableName 2 "z"), 
      (Fix Wildcard), (Fix $ Cut 2), (Fix $ Struct "d" [])])])])


fix2 = Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), (Fix $ Cut 0), 
    (Fix $ Wildcard)]

fix3 = (Fix $ Var $ VariableName 1 "x")

fix4 = (Fix $ Var $ VariableName 2 "y")
--}
