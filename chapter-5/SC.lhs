= Solution Counting Heuristic =

> {-# LANGUAGE CPP #-}

#ifndef RANDOMSC
#define RANDOMSC 0
#endif

> module SC ( randomsc
>           , orderValuesSC ) where

> import Dibbuk
> import BP
> import FD

> import List ( filter, sortBy )
> import Maybe ( fromJust )
> import Control.Monad.State
> import qualified Data.Map as Map
> import qualified Data.IntSet as IntSet
> import qualified Data.IntMap as IntMap
> import Data.IntMap ( IntMap )

> randomsc = RANDOMSC==1

> type Cost = Double

The solution counting heuristic orders values in the descending order
of solutions, and prunes values for which the solution count is 0.
The solution counting algorithm provides an upper bound of the number of solutions,
thus 0 solutions is always exact.

The heuristic is computed gradually, and the state is updated after counting
solutions for each value assignment.

> data SCState = SCState { n_alpha :: Double,
>                          counts :: [(Int, Double)],
>                          uncounted :: [Int],
>                          n_left :: Double,
>                          d_left :: Double }

> type SC a = State SCState a

Domain store must be passed to the solution counter. The store must ensure
fast retrieval and update.

> newtype DomainMap = DomainMap (IntMap Domain) deriving Show
> instance DomainStore DomainMap where
>     domput ivar domain (DomainMap dm) = DomainMap (IntMap.insert ivar domain dm)
>     domget ivar (DomainMap dm) = dm IntMap.! ivar

> orderValuesSC :: Network -> Cost -> FDState s -> FDVar s -> [Int] -> [Int]
> orderValuesSC bn cost state var vars = 

The solution counter expects the Bayesian network and variable
domains. The domains are retrieved from the FD state.

>     let domains = DomainMap $ IntMap.map (\info ->
>                                           IntSet.toList (values info))
>                                          (varMap state)

The variable is referred by its index unwrapped from FDVar.

>         ivar = unFDVar var

Values in the variable's domain are tried.

>         vardom = vars
>         domlen = fromIntegral $ length vardom

Before counting solutions for a particular assignment, the domains are
modified by setting the domain of the variable to a singleton
corresponding to the assignment.

>         assign val = domput ivar [val] domains

== Updating the State ==

When the new count is obtained, counts both for the currently
evaluated assignment and for yet unevaluated assignments must be updated.

>         update counts val n n_left d_left domain = 
>             let n_rest = n_left/d_left
>             in map (\vn@(val', n') ->
>                         if val==val' then (val, n)
>                         else if val `elem` domain then (val, n_rest)
>                         else vn)
>                     counts

== The Main Loop ==

#if RANDOMSC
For experimenting and supporting the hypothesis that the heuristic must
be called at the right places, random re-computation is implemented. In the experiments, 
the SC heuristic is applied after the domain values are randomized, thus
computing cost*domlen first counts is like 

>         next_assignment = 
>             let next_assignment i =
>                     do SCState n_alpha counts _ n_left d_left <- get
>                        if i/=0
>                           then do count_solutions
>                                   next_assignment (i-1)
>                           else return $ order_values counts
>             in next_assignment $ round (domlen*cost)

#else
Assignments are evaluated until no more assignments are left or
solutions are not worth counting.

>         next_assignment = 
>             do SCState n_alpha counts _ n_left d_left <- get

For the backtracking probability estimate  T_1/T_all, the VOI is
approximately (T_\alpha-T)*|D|; cost/domlen is the normalized
computation cost when VOI is computed as T_\alpha-T.

>                if worth (cost/domlen) n_alpha n_left d_left
>                   then do count_solutions
>                           next_assignment
>                   else return $ order_values counts
#endif

When solutions for the assignment are counted, the state is updated
with the new solution rates, and the evaluated assignment is removed
from the value list.

>         count_solutions =
>             modify $ \(SCState n_alpha counts (y:uncounted) n_left' d_left') -> 
>                 let n = watch "SC: score" $ score bn (assign y)
>                     n_left = n_left'-n
>                     d_left = d_left'-1
>                 in SCState (max n n_alpha)
>                            (update counts y n n_left d_left uncounted)
>                            uncounted
>                            n_left d_left

== Entry Point ==

The loop starts from the initial state, in which all assignments are unevaluated,
and the solution rate is the same for all assignments.

>     in case vardom of
>          [] -> []
>          _ -> evalState next_assignment
>               $ let s@(SCState n_alpha counts uncounted n_left d_left) =
>                         SCState 
>                         (n_left/d_left)                    -- n_alpha 
>                         (zip uncounted $ repeat n_alpha)   -- counts
>                         vardom                             -- uncounted
>                         (score bn domains)                 -- n_left
>                         domlen                             -- d_left
>                 in s

Values with non-zero count are returned in the descending count order.

>     where order_values = map fst
>                          . watch "SC: order"
>                          . sortBy (\(_, na) (_, nb) -> compare nb na)
>                          . filter (\(_, n) -> n /= 0.0)

Solutions are worth counting for an assignment if there are uncounted
assignments and VOI is positive. VOI is computed as follows:

  VOI/T = -γ + e^-ν Σ_{n=n_α+1}^∞ (1/n_α - 1/n) ν^n/n! > 0

> worth cost n_alpha n_left d_left = d_left > 0 && (cost==0 || voi >= 0)
>
>     where voi = let nu = n_left/d_left
>                     voi0 = -cost
>                     n0 = n_alpha+1

Factor c = exp^-ν ν^n/n!, computed safely; just computing the power in the
nominator and the factorial in the denominator would result in
Infinity/Infinity=NaN even for smaller values of ν and n.

>                     loop v n c =
>                         let v' = v + c*(1/n_alpha - 1/n)
>                         in if v==v' || isNaN v' then watch "SC: voi" v
>                            else loop v' (n+1) $! (c*nu/(n+1))

>                 in (loop voi0 n0 
>                     (let loop0 i c =
>                              if i>=n0 || c==0 then c
>                              else loop0 (i+1.0) $! c*(nu/(i+1))
>                      in loop0 0.0 $ exp (-nu)))
