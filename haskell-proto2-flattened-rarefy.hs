data FTS a = FS Atom [a] | FV VariableName | FW | FC Int
                          deriving (Show, Eq, Typeable, Ord)