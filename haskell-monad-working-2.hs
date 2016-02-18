-- Mehul Solanki.

-- State Monad.

-- A example using the State Monad for implementing a Memory with Locations, Variable Names and their Values.

module StateMonad(
  objectInsertUpdate
, objectRemove
, objectRetrieve
, runInsertUpdate
, runRemove
, runRetrieve
)where

import Control.Monad.State
import Data.Maybe
import Data.List
----------------------------------------------------------------------
-- Simplified Example
----------------------------------------------------------------------
type VariableName      = String
type Value             = Int
data Variable          = Variable (VariableName, Value)
                       deriving (Show, Eq)
data IntegerDictionary = ID [Variable]

(<--) = curry Variable
infix 8 <--

init_dictionary :: IntegerDictionary
init_dictionary = ID [
  ("x0" <-- 0), ("x1" <-- 1),
  ("x2" <-- 2), ("x3" <-- 3),
  ("x4" <-- 4), ("x5" <-- 5)     ]

variableName :: Variable -> VariableName
variableName (Variable (v,_)) = v

vNameEqual :: Variable -> Variable -> Bool
vNameEqual = (==) `on` variableName where
  (f2 `on` f1) a b = f2 (f1 a) (f1 b)

insertVariable :: Variable -> State IntegerDictionary Variable
insertVariable variable = do
	Control.Monad.State.modify (insertVariableHelper variable)
	return variable

insertVariableHelper :: Variable -> IntegerDictionary -> IntegerDictionary
insertVariableHelper variable (ID dictionary) =
  ID (variable : filter (not . (vNameEqual variable)) dictionary)

runInsertVariable :: IntegerDictionary -> Variable -> IntegerDictionary
runInsertVariable init_dictionary variable = snd $
    runState (insertVariable variable) init_dictionary

removeVariable :: Variable -> State IntegerDictionary Variable
removeVariable variable = do
	Control.Monad.State.modify (removeVariableHelper variable)
	return variable

removeVariableHelper :: Variable -> IntegerDictionary -> IntegerDictionary
removeVariableHelper variable (ID dictionary) =
    ID $ filter (not . (vNameEqual variable)) dictionary

runRemoveVariable :: IntegerDictionary -> Variable -> IntegerDictionary
runRemoveVariable init_dictionary variable = snd $ runState (removeVariable
	variable) init_dictionary

extractVariableValue :: Variable -> Value
extractVariableValue (Variable (_, value)) = value

exampleOperation :: Variable -> Variable -> State IntegerDictionary Variable
exampleOperation variableX variableY = do
	insertVariable variableX
        let vx = extractVariableValue variableX
	insertVariable variableY
        let vy = extractVariableValue variableY
            product = Variable ("product", vx * vy)
        insertVariable product
	return product

runExampleOperation :: IntegerDictionary -> Variable -> Variable ->
	IntegerDictionary
runExampleOperation init_dictionary variableX variableY = snd $ runState (
	exampleOperation variableX variableY) init_dictionary
{--
runExampleOperation init_dictionary (Variable ("x", 10)) (Variable ("y", 20))
[Variable ("x0",0),Variable ("x1",1),Variable ("x2",2),Variable ("x3",3),
Variable ("x4",4),Variable ("x5",5),Variable ("x",10),Variable ("y",20),
Variable ("product",200)]
--}


-- Creating Initial State of Integer Dictionary

{--
runInsertVariable init_dictionary (Variable ("x",10))
[Variable ("x0",0),Variable ("x1",1),Variable ("x2",2),Variable ("x3",3),
Variable ("x4",4),Variable ("x5",5),Variable ("x",10)]
--}

{--
runRemoveVariable init_dictionary (Variable ("x0",0))
[Variable ("x1",1),Variable ("x2",2),Variable ("x3",3),Variable ("x4",4),
Variable ("x5",5)]
--}


-- give example of x * y with insert and update
