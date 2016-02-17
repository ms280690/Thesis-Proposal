type Atom            = String

data VariableName    = VariableName Int String
      deriving (Eq, Data, Typeable, Ord)

data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard
          | Cut Int
          | New_Constructor_1 .........
          | New_Constructor_2 .........
      deriving (Eq, Data, Typeable)