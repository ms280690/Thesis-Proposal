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

type st = Id -> Term

newtype stP = stP { unstP :: st }

app                     :: st -> Term -> Term
app s (Var i)            = s i
app s (Struct a ts)      = Struct a (Prelude.map (app s) ts)

nullst               :: st
nullst i              = Var i

(->-)                   :: Id -> Term -> st
(i ->- t) j | j==i       = t
            | otherwise  = Var j

-- Function composition for applying two stitution functions.
(@@)                    :: st -> st -> st
s1 @@ s2                 = app s1 . s2


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
