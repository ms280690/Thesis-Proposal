run :: IOAction a -> IO a
run (Return a) = return a
run (Put s io) = putStrLn s >> run io
run (Get g)    = getLine >>= \s -> run (g s)