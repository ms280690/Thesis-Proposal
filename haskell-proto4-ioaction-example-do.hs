hello2 = do put "What is your name?"
            name <- get
            put "What is your age?"
            age <- get
            put ("Hello, " ++ name ++ "!")
            put ("You are " ++ age ++ " years old!")