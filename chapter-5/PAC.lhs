= Probabilistic Arc Consistency =

An implementation of pAC heuristic by Horsch and Havens.

> module PAC ( orderValuesPAC
>            , Network
>            , Node(..)
>            , testsPAC ) where

Debugging output makes our lives so much easier.

> import Dibbuk

> import FD

> import List (sortBy)
> import Maybe (fromJust)
> import Control.Monad.State
> import qualified Data.Map as Map
> import Data.Map ( Map )
> import qualified Data.IntMap as IntMap
> import Data.IntMap ( IntMap )
> import qualified Data.IntSet as IntSet

Tests are everything.

> import Test

Types for variables, values, domains and constraints are named
to aid readability.

> type IVar = Int
> type Val = Int
> type Edge = (IVar, IVar)
> type Domain = [Val]

The network is an array of nodes connected by constraint edges.

> type Network = [(IVar, Node)]

For each node, the constraints with predecessors and the number 
of successors must be known.

> data Node = Node { edges :: [(IVar, Con)] }

A constraint is a function that returns True iff satisfied.

> type Con = Val -> Val -> Bool

The pAC heuristic orders values in the decreasing order of relative
solution probabilities, computed simultaneously. The algorithm is a
variant of loopy propagation. The propagation loop terminates when
either the maximum number of iterations is reached, of the relative
difference in relative probabilities between two consecutive
iterations is smaller than the precision.

> orderValuesPAC :: Network -> Double -> Int -> FDState s -> FDVar s -> [Int] -> [Int]
> orderValuesPAC _ _ _ _ _ [] = []
> orderValuesPAC network prec niter state var vars = 
>     map fst $ watch "PAC: order"
>             $ sortBy (\(_, sa) (_, sb) -> compare sb sa)
>             $ filter (\(_, s) -> s>0)
>             $ zip (domains IntMap.! varidx) 
>             $ score network domains varidx prec niter
>     where varidx = unFDVar var
>           domains = IntMap.map (\info -> IntSet.toList (values info)) (varMap state)

> type Prob = Double
> type Probs = [Prob]

> mul x y = let z = x*y in if z>0 then z else min x y

> score network domains varidx prec niter =
>     evalState (loop 0 (normalize $ map (const 1.0) (domains IntMap.! varidx))) Map.empty
>     where update (ivar, Node edges) =
>               do ms <- get 
>                  let di = domains IntMap.! ivar
>                      ss = foldl (\ss (jvar, con) -> 
>                                      let dj = (domains IntMap.! jvar)
>                                          mji = if Map.member (jvar, ivar) ms 
>                                                then ms Map.! (jvar, ivar) 
>                                                else  map (const 1.0) dj
>                                          sji = map (\ival ->
>                                                         sum (zipWith (\jval mjval ->
>                                                                           (if con ival jval
>                                                                            then mjval
>                                                                            else 0.0))
>                                                                      dj mji))
>                                                    di
>                                      in Map.insert (jvar, ivar) sji ss)
>                                 Map.empty edges
>                      fi = normalize $ foldl (\fi (jvar, _) ->
>                                                  zipWith mul fi (ss Map.! (jvar, ivar)))
>                                             (map (const 1.0) di) edges
>                  let ms' = foldl (\ms (jvar, _) ->
>                                       let sji = ss Map.! (jvar, ivar)
>                                           m = zipWith (\f s -> if s>0 then f/s else 0) fi sji
>                                       in Map.insert (ivar, jvar) m ms)
>                                  ms edges
>                  put ms'
>                  return (ivar, fi)

>           once =
>               do mapM_ update (reverse network)
>                  fis <- mapM update network
>                  return (fromJust $ lookup varidx fis)

>           loop i fprev = 
>               do f <- once
>                  if difference (watch "PAC: f" f) fprev < prec || (watch "PAC: i" i)==niter
>                     then return f
>                     else loop (i+1) f

Probabilities must sum up to one.

> normalize probs = 
>     let s = sum probs
>     in if s>0 then map (/s) probs else probs

The loopy propagation stops when the probability vectors are
sufficiently similar.

> difference proba probb = maximum  $ zipWith (\a b -> abs (a-b)) proba probb

== Unit Testing ==

> roundprobs = map ((/1000.0) . fromIntegral . round . (*1000.0)) 

> testUnconnected =
>     let unconnected = [ (0, Node [])
>                       , (1, Node []) ]
>     in resultEQ (score unconnected (IntMap.fromList [(0, [0,1]), (1, [0,1])]) 0 0.0001 10)
>        [0.5, 0.5]
>        "PAC.score unconnected"
>                      

> testChain =
>     let chain = [ (0, Node [(1, \x y -> x==y)])
>                 , (1, Node [(0, \x y -> x==y), (2, \x y -> x/=y)])
>                 , (2, Node [(1, \x y -> x/=y)]) ]
>     in resultEQ (roundprobs $ score chain (IntMap.fromList [(0, [0, 1]), (1, [0, 1, 2]), (2, [0, 1])]) 1 0.0001 10)
>        (roundprobs [1/2, 1/2, 0])
>        "PAC.score chain"

> testTree =
>     let tree = [ (0, Node [(1, \x y -> x > y), (2, \x y -> x > y)])
>                , (1, Node [(0, \x y -> x < y)])
>                , (2, Node [(0, \x y -> x < y)]) ]
>     in resultEQ (roundprobs $ score tree (IntMap.fromList [(0,[0,1,2]), (1, [0,1,2]), (2, [0,1,2])]) 0 0.00001 10)
>        (normalize $ [0, 1, 4])
>        "PAC.score tree"

Solution count for structures with cycles is approximate.

> testCycle =
>     let cycle = [ (0, Node [(1, \x y -> x==y), (2, \x y -> x==y)])
>                 , (1, Node [(0, \x y -> x==y), (2, \x y -> x==y)])
>                 , (2, Node [(0, \x y -> x==y), (1, \x y -> x==y)]) ]
>     in resultEQ (roundprobs $ score cycle (IntMap.fromList [(0, [0, 1]), (1, [0, 1]), (2, [0, 1])]) 0 0.00001 10)
>        (roundprobs $ normalize [1, 1])
>        "PAC.score cycle"

> testKite =
>     let kite = [ (0, Node [(1, \x y -> x==y), (2, \x y -> x==y)])
>                , (1, Node [(0, \x y -> x==y), (2, \x y -> x==y)])
>                , (2, Node [(0, \x y -> x==y), (1, \x y -> x==y), (3, \x y -> x==y)])
>                , (3, Node [(2, \x y -> x==y)]) ]
>     in resultEQ (roundprobs $ score kite (IntMap.fromList [(0, [0]), (1, [0,1]), (2, [0, 1, 2]), (3, [0, 1, 2, 3])]) 0 0.00001 10, roundprobs $ score kite (IntMap.fromList [(0, [0]), (1, [0, 1]), (2, [0, 1, 2]), (3, [0, 1, 2, 3])]) 3 0.00001 10)
>        (roundprobs $ normalize [1], roundprobs $ normalize [1, 0, 0, 0])
>        "PAC.score kite"

> testsPAC = runSuite [ testUnconnected 
>                     , testChain
>                     , testTree
>                     , testCycle
>                     , testKite ]
