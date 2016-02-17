seqio :: IOAction a -> (a -> IOAction b) -> IOAction b
seqio (Return a) f = f a
seqio (Put s io) f = Put s (seqio io f)
seqio (Get g)    f = Get (\s -> seqio (g s) f)