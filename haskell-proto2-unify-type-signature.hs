unify :: 
(BindingMonad t v m, Fallible t v e, MonadTrans em, Functor (em m), 
	MonadError e (em m))	 
	=> UTerm t v	 
    -> UTerm t v	 
    -> em m (UTerm t v)