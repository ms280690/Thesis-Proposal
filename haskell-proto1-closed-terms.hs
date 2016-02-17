data VariableName = VariableName Int String
          deriving (Eq, Data, Typeable, Ord)
  
data Atom = Atom !String | Operator !String 
          deriving (Eq, Ord, Data, Typeable)
  
data Term = Struct Atom [Term]
            | Var VariableName
            | Wildcard
            | Cut Int
          deriving (Eq, Data, Typeable)