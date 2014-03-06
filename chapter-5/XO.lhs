= Variable Ordering =

> module XO ( orderVariablesRandomly
>           , orderVariablesMCF
>           , testsXO ) where

> import Def
> import Perm ( perm )
> import List(sortBy)

> import qualified Test

Sort variables randomly. Eliminates dependance on the predefined order.

> orderVariablesRandomly (Def vars constraints) =
>     Def (perm vars) constraints

Sort variables by decreasing order of constraints they appear in.

> orderVariablesMCF (Def vars constraints) =
>     Def (sortBy (\va vb -> compare (cc vb) (cc va)) vars) constraints
>     where cc (v,_) = (length . filter (v `elem`) . map scope) constraints

== Tests ==

> testMCF = 
>     let p@(Def vars constraints) = Def [("a", []), ("b", []), ("c", [])]
>             [Same "b" "a", Diff "b" "b", Less "b" "c", Same "c" "c"]
>     in Test.resultEQ
>            (orderVariablesMCF p)
>            (Def [("b", []), ("c", []), ("a", [])] constraints)
>            "XO.orderVariablesMCF"

> testsXO = Test.runSuite [ testMCF ]