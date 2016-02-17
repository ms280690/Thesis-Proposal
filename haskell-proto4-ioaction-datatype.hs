data IOAction a = Return a
                | Put String (IOAction a)
                | Get (String -> IOAction a)