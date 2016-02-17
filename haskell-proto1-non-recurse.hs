data FlatTerm a = Struct Atom [a]
                | Var VariableName
                | Wildcard
                | Cut Int
			deriving (Show, Eq, Ord)