
= Constraint Satisfaction Problem =

> module Def ( Var
>            , Val
>            , Domain
>            , Asgn
>            , Constraint(..)
>            , scope
>            , Def(..)
>            , Sol 
>            , satisfies
>            , testsDef ) where

Tests are everything.

> import Test
> import List( (\\), sort, union )
> import Maybe( fromJust )

A constraint network is defined by a set of variables, their domains,
and constraints. Variable domains are finite.

> type Var = String
> type Val = Int
> type Domain = [Val]
> type Asgn = (Var, Val)

A constraint is a boolean function on a vector of variables, its scope.

> data Constraint = Same Var Var              -- variables have save value
>                 | Diff Var Var              -- variables have different values
>                 | Less Var Var              -- first less than second
>                 | Nogood Var Var [(Val,Val)] -- the two assignments are incompatible
>                 | Or Constraint Constraint  -- union
>     deriving (Read, Show, Eq)

It is useful to know the scope of a constraint:

> type Scope = [Var]
> scope :: Constraint -> Scope
> scope = sort . scope'
>     where scope' (Same va vb) = [va, vb]
>           scope' (Diff va vb) = [va, vb]
>           scope' (Less va vb) = [va, vb]
>           scope' (Nogood vara varb _) = [vara, varb]
>           scope' (Or ca cb) =  scope ca `union` scope cb

Here is finally the problem definition, with derived Read and Show for
parsing and printing.

> data Def = Def [(Var,Domain)] [Constraint] deriving (Read, Show, Eq)

A solution is a list of bindings.

> type Sol = [Asgn]

Verifying a solution against the definition serves as an integrity check
and a soundness test for the search algorithm.

> satisfies :: Def -> Sol -> Bool
> satisfies (Def _ constraints) solution =
>     all sat constraints
>     where val name = fromJust $ lookup name solution
>           sat (Same va vb) = (val va)==(val vb)
>           sat (Diff va vb) = (val va)/=(val vb)
>           sat (Less va vb) = (val va)<(val vb)
>           sat (Nogood va vb nogoods) = (val va, val vb) `notElem` nogoods
>           sat (Or ca cb) = (sat ca) || (sat cb)


== Tests ==

Scopes are concatenated properly.

> testScopeOr = resultEQ
>               (["a", "b", "c", "d"] 
>                \\ scope (Or (Diff "a" "a")
>                             (Or (Same "a" "b") (Less "c" "d"))))
>               []
>               "Def.scope Or"

The scope for nogood constraints is computed correctly.

> testScopeNogood = resultEQ
>                   (["a", "b"]
>                    \\ scope (Nogood "a" "b" [(1,1), (2,1)]))
>                   []
>                   "Def.scope Nogood"

All tests together.

> testsDef = runSuite [ testScopeOr, testScopeNogood ]
