{-# LANGUAGE  DeriveDataTypeable,
              ViewPatterns,
              ScopedTypeVariables,
              DefaultSignatures,
              TypeOperators,
              TypeFamilies,
              DataKinds,
              DataKinds,
              PolyKinds,
              OverlappingInstances,
              TypeOperators,
              LiberalTypeSynonyms,
              TemplateHaskell,
              AllowAmbiguousTypes,
              ConstraintKinds, 
              Rank2Types, 
              MultiParamTypeClasses,
              FunctionalDependencies,
              FlexibleContexts,
              FlexibleInstances,
              UndecidableInstances 
              #-}

-- stitutions and Unification of Prolog Terms
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

module st where

import Prolog
import CustomSyntax
import Data.Map as Map
import Data.Maybe
import Data.Either

--Unification
import Control.Unification.IntVar
import Control.Unification.STVar as ST

import Control.Unification.Ranked.IntVar
import Control.Unification.Ranked.STVar

import Control.Unification.Types as UT

import Control.Monad.State.UnificationExtras
import Control.Unification as U

-- Monads
import Control.Monad.Error
import Control.Monad.Trans.Except

import Data.Functor.Fixedpoint as DFF

--State
import Control.Monad.State.Lazy
import Control.Monad.ST
import Control.Monad.Trans.State as Trans

infixr 3 @@
infix  4 ->-

type Subst = Id -> Term

newtype SubstP = SubstP { unSubstP :: Subst }

app                     :: Subst -> Term -> Term
app s (Var i)            = s i
app s (Struct a ts)      = Struct a (Prelude.map (app s) ts)

nullSubst               :: Subst
nullSubst i              = Var i

(->-)                   :: Id -> Term -> Subst
(i ->- t) j | j==i       = t
            | otherwise  = Var j

(@@)                    :: Subst -> Subst -> Subst
s1 @@ s2                 = app s1 . s2

unify :: Term -> Term -> [Subst]
unify (Var x)       (Var y)       = if x==y then [nullSubst] else [x->-Var y]
unify (Var x)       t2            = [ x ->- t2 | x `notElem` varsIn t2 ]
unify t1            (Var y)       = [ y ->- t1 | y `notElem` varsIn t1 ]
unify (Struct a ts) (Struct b ss) = [ u | a==b, u<-listUnify ts ss ]

listUnify :: [Term] -> [Term] -> [Subst]
listUnify []     []     = [nullSubst]
listUnify []     (r:rs) = []
listUnify (t:ts) []     = []
listUnify (t:ts) (r:rs) = [ u2 @@ u1 | u1<-unify t r,
                                       u2<-listUnify (map (app u1) ts)
                                                     (map (app u1) rs) ]


-- instance Show stP where
--  show (i) = show $ Var i
-- stitutions are represented by functions mapping identifiers to terms.
--
-- app s     extends the stitution s to a function mapping terms to terms
{--
Looks like an apply function that applies a stitution function tho the variables in a term.
--}


-- nullst is the empty stitution which maps every identifier to the same identifier (as a term).



-- i ->- t   is the stitution which maps the identifier i to the term t, but otherwise behaves like nullst.


-- s1@@ s2  is the composition of stitutions s1 and s2
--           N.B.  app is a monoid homomorphism from (st,nullst,(@@))
--           to (Term -> Term, id, (.)) in the sense that:
--                  app (s1 @@ s2) = app s1 . app s2
--                 s @@ nullst = s = nullst @@ s

{--
app (stFunction) (Struct "hello" [Var (0, "Var")])
hello(Var_2) :: Term

--}


{--
nullst (0, "Var")
Var :: Term
--}


{--
:t (->-) (1,"X") (Struct "hello" [])
(1,"X") ->- Struct "hello" [] :: (Int,[Char]) -> Term
--}

-- Function composition for applying two stitution functions.

--- Unification:

-- unify t1 t2 returns a list containing a single substitution s which is
--             the most general unifier of terms t1 t2.  If no unifier
--             exists, the list returned is empty.