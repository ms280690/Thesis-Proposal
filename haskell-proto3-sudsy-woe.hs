-- Stack based Prolog inference engine
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

module StackEngine( version, prove ) where

import Prolog
import st
import Interact

version = "stack based"

--- Calculation of solutions:

-- the stack based engine maintains a stack of triples (s,goal,alts)
-- corresponding to backtrack points, where s is the stitution at that
-- point, goal is the outstanding goal and alts is a list of possible ways
-- of extending the current proof to find a solution.  Each member of alts
-- is a pair (tp,u) where tp is a new goal that must be proved and u is
-- a unifying stitution that must be combined with the stitution s.
--
-- the list of relevant clauses at each step in the execution is produced
-- by attempting to unify the head of the current goal with a suitably
-- renamed clause from the database.

type Stack = [ (st, [Term], [Alt]) ]
type Alt   = ([Term], st)

alts       :: Database -> Int -> Term -> [Alt]
alts db n g = [ (tp,u) | (tm:-tp) <- renClauses db n g, u <- unify g tm ]

prove      :: Database -> [Term] -> [st]
prove db gl = solve 1 nullst gl []
 where
   solve :: Int -> st -> [Term] -> Stack -> [st]
   solve n s []     ow          = s : backtrack n ow
   solve n s (g:gs) ow
                    | g==theCut = solve n s gs (cut ow)
                    | otherwise = choose n s gs (alts db n (app s g)) ow

   choose :: Int -> st -> [Term] -> [Alt] -> Stack -> [st]
   choose n s gs []          ow = backtrack n ow
   choose n s gs ((tp,u):rs) ow = solve (n+1) (u@@s) (tp++gs) ((s,gs,rs):ow)
   
   backtrack                   :: Int -> Stack -> [st]
   backtrack n []               = []
   backtrack n ((s,gs,rs):ow)   = choose (n-1) s gs rs ow

theCut    :: Term
theCut     = Struct "!" []

cut                  :: Stack -> Stack
cut ss                = []

--- End of Engine.hs
