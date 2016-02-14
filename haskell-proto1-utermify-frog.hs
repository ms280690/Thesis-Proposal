uTermify
  :: Map VariableName (ST.STVar s (FlatTerm))
  -> UTerm FlatTerm (ST.STVar s (FlatTerm))
  -> UTerm FlatTerm (ST.STVar s (FlatTerm))
uTermify varMap ux = case ux of
  UT.UVar _                -> ux
  UT.UTerm (Var v)        -> maybe (error "bad map") UT.UVar $
                               Map.lookup v varMap
  -- UT.UTerm t            -> UT.UTerm $! fmap (uTermify varMap)
  UT.UTerm (Struct a xs)   -> UT.UTerm $ Struct a $!
                                       fmap (uTermify varMap) xs
  UT.UTerm (Wildcard)      -> UT.UTerm Wildcard
  UT.UTerm (Cut i)         -> UT.UTerm (Cut i)

translateToUTerm ::
    Fix FlatTerm -> ST.STBinding s
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
translateToUTerm e1Term = do
  let vs = variableNameSet $ variableNameExtractor e1Term
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . unfreeze $ e1Term
  return (t2,varMap)
