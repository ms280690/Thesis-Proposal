instance Monad IOAction where
    return = Return
    (>>=)  = seqio