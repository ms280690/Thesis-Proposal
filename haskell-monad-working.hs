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

-- This package allows to access the nth element from a tuple with at most 15 elements. 
import Data.Tuple.Select

import Data.Maybe
import Data.List
-- For a Test Run using the run function.
main :: IO ()
main = print $ run [("a2", 1001, (String "hello")), ("a1", 1000, (Number 26))]

run :: Memory -> Memory											
run initMem = snd $ runState (objectRetrieve "a1" 1000 (Number 26) ) initMem
-- OUTPUT
{--
main
[("a1",1000.0,Number 26.0)]
--}

-- These are the set of types of values a variable can have.
-- A variable can be a number, a string or a single character.
data Objects = Number Double | String String | Character Char deriving (Show, Eq)

-- OUTPUT
{--
Number 123
Number 123.0
String "Hello"
String "Hello"
Character 'A'
Character 'A'
--}

-- State changes from one Memory State to another Memory State.
type Memory = [(String,Double,Objects)]

-- Inserting or Updating a variable at a given location in the memory.
-- The first thing to do is to get the current state of memory using get, this then along with the parameters
-- to an auxiliary function.
-- The operations are carried out by the auxiliary function and returned back a state of memory 
-- which is then returned back using the put function. 
objectInsertUpdate :: String -> Double -> Objects -> State Memory Objects  
objectInsertUpdate varName memLoc varValue = do
										mem <- get
										put (objectInsertUpdate1 varName memLoc varValue mem 0)
										return varValue
										
-- The auxiliary function then will check if the initial memory is empty then just add an entry with the passed 
-- credentials and return it back to the function.
-- Else check the variable and the location for a match and if the nothing matches add the new one to the end.
-- If a match is found and the value of the variable is different then update and return the new memory.										
objectInsertUpdate1:: (Eq a, Eq a1, Eq a2) => a -> a1 -> a2 -> [(a, a1, a2)] -> Int -> [(a, a1, a2)]
objectInsertUpdate1 varName memLoc varValue [] _ = [(varName, memLoc, varValue)]
objectInsertUpdate1 varName memLoc varValue xs n = if ( n <= (length xs - 1) && (sel1 (xs !! n)) == varName && (sel2 (xs !! n)) == memLoc && (sel3 (xs !! n)) /= varValue)
												then (take n xs) ++ [(varName, memLoc, sel3 (xs !! n))] ++ (drop (n+1) xs)
												else if (n > length xs - 1)
													then xs ++ [(varName, memLoc, varValue)]
													else objectInsertUpdate1 varName memLoc varValue xs (n + 1)

-- A run function to pass the initial memory state to the function and the required parameter to get the result.													
runInsertUpdate :: Memory -> String -> Double -> Objects -> Memory														
runInsertUpdate initMem varName memLoc varVal = snd $ runState ( objectInsertUpdate varName memLoc varVal) initMem							

-- OUTPUT
{--
runInsertUpdate [("a2", 1001, (String "hello")), ("a1", 1000, (Number 26))] "a3" 1085 (Number 123)
[("a2",1001.0,String "hello"),("a1",1000.0,Number 26.0),("a3",1085.0,Number 123.0)]
runInsertUpdate [("a2", 1001, (String "hello")), ("a1", 1000, (Number 26))] "a2" 1001 (String "World")
[("a2",1001.0,String "hello"),("a1",1000.0,Number 26.0)]
runInsertUpdate [("a2", 1001, (String "hello")), ("a1", 1000, (Number 26))] "a2" 1001 (String "hello")
[("a2",1001.0,String "hello"),("a1",1000.0,Number 26.0)]
--}

-- Removing an object from the memory.
-- The procedure carried out is the same as the above one, get the state pass it to an auxiliary function along 
-- with other parameter it return a state of memory, put it.  														
objectRemove :: String -> Double -> Objects -> State Memory Objects
objectRemove varname memLoc varValue = do
							mem <- get
							put (objectRemove1 varname memLoc varValue mem 0)
							return varValue

-- If the initial memory is empty then nothing to remove.
-- Search through the memory and match the name and the location of the one to be removed if match found
-- then remove and return the memory else the object does not exist in the memory. 							
objectRemove1
  :: (Eq a, Eq a1, Sel2 a2 a1, Sel1 a2 a) =>
     a -> a1 -> t -> [a2] -> Int -> [a2]							
objectRemove1 varName memLoc varValue []  _ = error "Memory Empty"
objectRemove1 varName memLoc varValue xs n = if ( n <= (length xs - 1) && (sel1 (xs !! n)) == varName && (sel2 (xs !! n)) == memLoc)
													then (take n xs) ++ (drop (n + 1) xs)
													else if (n > length xs - 1)
														then error "Object not Found"
														else objectRemove1 varName memLoc varValue xs (n + 1)

-- A run function to pass the initial memory state to the function and the required parameter to get the result.														
runRemove :: Memory -> String -> Double -> Objects -> Memory
runRemove initMem varName memLoc varVal = snd $ runState (objectRemove varName memLoc varVal) initMem

-- OUTPUT
{--
runRemove [("a2", 1001, (String "hello")), ("a1", 1000, (Number 26))] "a2" 1001 (String "Hello")
[("a1",1000.0,Number 26.0
runRemove [("a2", 1001, (String "hello")), ("a1", 1000, (Number 26))] "a3" 1001 (String "Hello")
*** Exception: Object not Found
runRemove [] "a3" 1001 (String "Hello")
*** Exception: Memory Empty
--}

-- Retrieving an Object at particular location in the memory.
-- Same procedure as above the difference lies in the working of the auxiliary function.
objectRetrieve :: String -> Double -> Objects -> State Memory Objects							
objectRetrieve varName memLoc varValue = do
											mem <- get
											put (objectRetrieve1 varName memLoc varValue mem 0)
											return varValue

-- The base case remains the same if empty memory then nothing to retrieve.
-- Search the initial memory for matching location name and location if found then return a memory with a single 
-- cell, containing the retrieved object.										
objectRetrieve1
  :: (Eq a, Eq a1, Sel2 a2 a1, Sel1 a2 a) =>
     a -> a1 -> t -> [a2] -> Int -> [a2]											
objectRetrieve1 varName memLoc varValue [] _ = error "Empty Memory"											
objectRetrieve1 varName memLoc varValue xs n = if (( n <= (length xs - 1) && (sel1 (xs !! n)) == varName && (sel2 (xs !! n)) == memLoc))
													then [(xs !! n)]
													else if (n > length xs - 1)
														then error "Object not Found"
														else objectRetrieve1 varName memLoc varValue xs (n + 1)
														
-- A run function to pass the initial memory state to the function and the required parameter to get the result.
runRetrieve :: Memory -> String -> Double -> Objects -> Memory
runRetrieve initMem varName memLoc varVal = snd $ runState (objectRetrieve varName memLoc varVal) initMem

-- OUTPUT
{--
runRetrieve [("a2", 1001, (String "hello")), ("a1", 1000, (Number 26))] "a2" 1001 (String "Hello")
[("a2",1001.0,String "hello")]
runRetrieve [("a2", 1001, (String "hello")), ("a1", 1000, (Number 26))] "a3" 1001 (String "Hello")
*** Exception: Object not Found
runRetrieve [] "a3" 1001 (String "Hello")
*** Exception: Empty Memory
--}

------------------------------------------------------------------------------------------------------------
-- Simplified Example
------------------------------------------------------------------------------------------------------------
type VariableName = String

type Value = Int

data Variable = Variable (VariableName, Value) deriving (Show, Eq)

type IntegerDictionary = [Variable]

-- Creating Initial State of Integer Dictionary
init_dictionary :: IntegerDictionary
init_dictionary = [Variable ("x0", 0), Variable ("x1", 1), Variable ("x2", 2), 
	Variable ("x3", 3), Variable ("x4", 4), Variable ("x5", 5)] 

insertVariable :: Variable -> State IntegerDictionary Variable
insertVariable variable = do
	init_dictionary <- get
	put (insertVariableHelper init_dictionary variable)
	return variable
insertVariableHelper :: IntegerDictionary -> Variable -> IntegerDictionary
insertVariableHelper init_dictionary variable = if (elem variable
	 init_dictionary) then (updateVariable init_dictionary variable)
	else init_dictionary ++ [variable]

updateVariable :: IntegerDictionary -> Variable -> IntegerDictionary
updateVariable init_dictionary variable = (take (fromJust $ elemIndex variable 
	init_dictionary) init_dictionary ++ 
	[variable] ++ drop ((fromJust $ elemIndex variable init_dictionary) + 1) 
		init_dictionary)
runInsertVariable :: IntegerDictionary -> Variable -> IntegerDictionary
runInsertVariable init_dictionary variable = snd $ runState (insertVariable 
	 variable) init_dictionary
{--
runInsertVariable init_dictionary (Variable ("x",10))
[Variable ("x0",0),Variable ("x1",1),Variable ("x2",2),Variable ("x3",3),
Variable ("x4",4),Variable ("x5",5),Variable ("x",10)]
--}

removeVariable :: Variable -> State IntegerDictionary Variable
removeVariable variable = do
	init_dictionary <- get
	put (removeVariableHelper init_dictionary variable)
	return variable
removeVariableHelper :: IntegerDictionary -> Variable -> IntegerDictionary
removeVariableHelper [] variable = error "Empty Dictionary"
removeVariableHelper init_dictionary variable = if (elem variable 
	init_dictionary) then (delete variable init_dictionary)
	else error "Variable Not Found"

runRemoveVariable :: IntegerDictionary -> Variable -> IntegerDictionary
runRemoveVariable init_dictionary variable = snd $ runState (removeVariable 
	variable) init_dictionary
{--
runRemoveVariable init_dictionary (Variable ("x0",0))
[Variable ("x1",1),Variable ("x2",2),Variable ("x3",3),Variable ("x4",4),
Variable ("x5",5)]
--}


-- give example of x * y with insert and update

extractVariableValue :: Variable -> Value
extractVariableValue (Variable (_, value)) = value

exampleOperation :: Variable -> Variable -> State IntegerDictionary Variable
exampleOperation variableX variableY = do 
	init_dictionary_1 <- get
	put (insertVariableHelper init_dictionary_1 variableX)
	init_dictionary_2 <- get
	put (insertVariableHelper init_dictionary_2 variableY)
	let product = Variable ("product", (extractVariableValue variableX) * (
		extractVariableValue variableY))
	init_dictionary_3 <- get
	put (insertVariableHelper init_dictionary_3 product) 
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