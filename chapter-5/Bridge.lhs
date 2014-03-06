> {-# LANGUAGE CPP #-}

= Bridge between Problem Definition and Algorithms =

> module Bridge ( fd_of_def 
>               , bp_of_def 
>               , pac_of_def
>               , testsBridge ) where

> import Def
> import FD
> import BP
> import qualified PAC

> import List ( groupBy, head )
> import Control.Monad ( mapM, sequence, mplus )
> import Maybe ( fromJust )
> import Data.Bits (shift)
> import qualified Data.IntSet as IntSet

> import Dibbuk 

Tests is everything:

> import Test

== Problem Definition to Finite Domain Solver ==

Translate a problem definition into an FD monad. The monad wraps an
association list of variable, value pairs; thus the result is a list
of association lists of assignments.

> fd_of_def :: Def -> FD s Sol
> fd_of_def (Def vars constraints) = 
>     let (names, domains) = unzip vars
>     in do fdvars <- mapM newVar domains
>           fd_of_cs (zip  names fdvars) constraints
>           vals <- labelling fdvars
>           return (zip names vals)

Constraints refer to variables by names but applied to FDVars, vxlat is
the translation table from Var to FDVar.

> fd_of_cs :: [(Var, FDVar s)] -> [Constraint] -> FD s ()
> fd_of_cs vxlat constraints = mapM_ of_c constraints
>     where of_v v = fromJust (lookup v vxlat)
>           of_c (Same va vb) = (of_v va) `same` (of_v vb)
>           of_c (Diff va vb) = (of_v va) `diff` (of_v vb)
>           of_c (Less va vb) = (of_v va) `less` (of_v vb)
>           of_c (Nogood va vb nogoods) = nogood nogoods (of_v va) (of_v vb)
>           of_c (Or ca cb) = (of_c ca) `mplus` (of_c cb)

== Problem Definition to Bayesian Network for Solution Counting ==

> bp_of_def :: Def -> Network
> bp_of_def def@(Def vars constraints) =
>     fix_widows nodes
>     where nodes = 
>               map (\i -> (i, Node (edges_from_preds i) (number_of_succs i)))
>                   [0..(length vars)-1]

Vacuous constraints must be added from all widow nodes to their successors 
in variable order.

>           fix_widows (node:rest) = 
>               let fix_widows' node head [] = reverse (node:head)
>                   fix_widows' (i, n) head ((i',n'@(Node _ 0)):rest) = 
>                       fix_widows' (i',n' {nsuccs=1}) ((i, n {pedges=pedges n++[(i', vacuous)]}):head)
>                          rest
>                   fix_widows' node head (node':rest) = 
>                       fix_widows' node' (node:head) rest
>               in fix_widows' node [] rest
>               where vacuous _ _ = True -- vacuous constraint, any pair is allowed

The numbers and lists of successors and predecessors are used to propagate
the belief.

>           (edges_from_preds, _, _, number_of_succs) = netlib def

> pac_of_def def@(Def vars constraints) =
>     map (\i -> (i, PAC.Node (edges_from_preds i ++ edges_to_succs i)))
>             [0..(length vars)-1]
>     where (edges_from_preds, edges_to_succs, _, _) = netlib def

== Common functions for different variants of belief networks ==

> netlib def@(Def vars constraints) =
>     (edges_from_preds, edges_to_succs, number_of_preds, number_of_succs)
>     where 
>       edges_from_preds i = 
>           map (\edge -> (ipred edge, of_e edge))
>               $ filter (\edge -> (isucc edge == i)) edges
>       edges_to_succs i =
>           map (\edge -> (isucc edge, flip $ of_e edge))
>               $ filter (\edge -> (ipred edge == i))edges
>       number_of_preds i =
>           length $ filter (\edge -> (isucc edge == i)) edges
>       number_of_succs i =
>           length $ filter (\edge -> (ipred edge == i)) edges

Edges are labeled by groups of constraints between the nodes.

>       edges = groupBy (\x y -> (scope x) == (scope y)) constraints

>       ipred = maximum . ends
>       isucc  = minimum . ends
>       ends = map of_v . scope . head

Variables are mapped from names to indices and are referred by the indices.

>       of_v v = fromJust (lookup v vxlat)
>           where vxlat = zip (map fst vars) [0..]

Constraint groups on edges are translated into functions.

>       of_e (c:cs) = foldl and (of_c c) cs
>           where and f c = \x y -> f x y && (of_c c) x y
>                 of_c (Same va vb) = (==)
>                 of_c (Diff va vb) = (/=)

The arguments are passed in the accending order of variables. Adjust the asymetric
constraints accordingly.

>                 of_c (Less va vb) =
>                     order_args va vb $ (<)
>                 of_c (Nogood va vb nogoods) =
>                     let nogoodset = IntSet.fromList $ map encode nogoods
>                     in order_args va vb $ 
>                        \x y -> (encode (x,y)) `IntSet.notMember` nogoodset
>                     where encode (x, y) =  (x-min_value)*value_range + y-min_value

Disjunction of constraints with the same scope only is allowed.

>                 of_c c@(Or ca cb) =
>                     case (scope c) of
>                       [_, _] ->  \x y -> (of_c ca) x y || (of_c cb) x y
>                       _ -> error "Or: non-binary scope"

>                 order_args vara varb f = if of_v vara < of_v varb then f else flip f

To compress nogoods constraints into integers we need to know the value range.

>                 ext_value ext = ext $ map (\(_, domain) -> ext domain) vars
>                 max_value = ext_value maximum
>                 min_value = ext_value minimum
>                 value_range = max_value-min_value+1

== Tests ==

=== FD ===

A minimal test that converts definition to FD instance,
solves it and ensures that the solution is correct.

> testFd_Same = resultEQ
>                   ( runFD $ fd_of_def 
>                           $ Def [("a", [1,2]), ("b", [1,3])]
>                                 [Same "a" "b"] )
>                   [[("a", 1), ("b", 1)]]
>                   "Bridge.fd_of_def Same"

`Or' works on constraints with identical domains.

> testFd_Or = resultEQ
>                 (runFD $ fd_of_def
>                        $ Def [("a", [1,2]), ("b", [1,2])]
>                              [Or (Same "a" "b") (Less "a" "b"),
>                               Or (Same "a" "b") (Less "b" "a")] )
>                 [[("a", 1), ("b", 1)], [("a", 2), ("b", 2)]]
>                 "Bridge.fd_of_def Or"

=== BN ===

A bucket graph for testing bp_of_def.

> test_network = bp_of_def (Def [ ("a", [1, 2]), ("b", [1, 2]), ("c", [1, 2]) ]
>                               [ Less "b" "c"
>                               , Diff "b" "c"
>                               , Diff "a" "c" ])

Widows are grounded correctly in the sink.

> testBN_widow = resultEQ
>                 (let (1, Node pedges nsuccs) = test_network!!1
>                  in (length pedges, nsuccs))
>                 (1, 1)
>                 "Bridge.bp_of_def widow"

> testBN_sink = resultEQ
>                 (let (0, Node pedges nsuccs) = test_network!!0
>                  in (length pedges, nsuccs))
>                 (2, 0)
>                 "Bridge.bp_of_def sink"

Constraint compilation works.

> testBN_compilation = resultEQ
>                      (let (0, Node ((_,f):_) _) = test_network!!0
>                       in (f 1 2, f 2 2, f 1 2))
>                      (True, False, True)
>                      "Bridge.bp_of_def compilation"

=== All tests together ===

> testsBridge = runSuite 
>               [ testFd_Same
>               , testFd_Or
>               , testBN_widow
>               , testBN_sink
>               , testBN_compilation ]
