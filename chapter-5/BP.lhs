> {-# LANGUAGE CPP #-}

#ifndef SPANTREE
#define SPANTREE 0
#endif

= Belief Propagation for Solution Counting =

> module BP ( spantree
>           , Network
>           , Node(..)
>           , DomainStore(..)
>           , Domain
>           , score
>           , testsBP ) where

> import Dibbuk

> import qualified Array
> import Array ( Array, listArray, (!), (//), elems )
> import Maybe ( fromJust )
> import Control.Monad.State
> import qualified Data.IntSet as IntSet
> import Data.IntSet ( IntSet )
> import qualified Data.Set as Set
> import Data.Set ( Set )
> import qualified Data.IntMap as IntMap
> import Data.IntMap ( IntMap )

Tests is everything.

> import Test

> spantree = SPANTREE==1

The module implements the Belief Propagation algorithm
for solution counting. The algorithm returns an estimate 
of the number of solutions to the CSP.

> type Score = Double

The variables are numbered sequentially, the domains are sets 
of integers.

> type IVar = Int
> type Val = Int
> type Domain = [Val]

The network is an array of nodes connected by constraint edges.

> type Network = [(IVar, Node)]

For each node, the constraints with predecessors and the number 
of successors must be known.

> data Node = Node {pedges :: [(IVar, Con)], nsuccs :: Int}

A constraint is a function that returns True iff satisfied.

> type Con = Val -> Val -> Bool

The domain store is provided as an argument of the algorithm.

> class DomainStore a where
>     domput :: IVar -> Domain -> a -> a
>     domget :: IVar -> a -> Domain

== Single-pass Propagation (Meisels-Shimony) ==

The single-pass propagation traverses the network from the root 
to the sink computing probabilities. At the end, the solution
count is computed based on the probabilities and the product
domain size.

At every node, the algorithm computes the list of pairs
[(domain-value, probability)] for the current variable, and then 
adds the pair (ivar, ([(d,p)], eta=(sum p)^(1/nsucc-1))) to the
list (PTStore). Propagation of bottom through ultiple paths
must be compensated.

The spanning tree is one way to compensate for multiple paths.

> type Edge = (IVar, IVar)
> type SpanningTree = Set Edge
> data STState = STState { tree :: SpanningTree, nodes :: IntSet }

> spanning_tree :: Network -> SpanningTree
> spanning_tree network@(sink:_) = 
>     let add_parents :: IVar -> [IVar] -> State STState tree -> State STState tree
>         add_parents _ [] after = after
>         add_parents i (j:more_parents) after =
>             do STState tree nodes <- get
>                if j `IntSet.member` nodes
>                   then do let tree' = (i, j) `Set.insert` tree
>                               nodes' = j `IntSet.delete` nodes
>                           if IntSet.null nodes
>                               then after
>                               else do put $ STState tree' nodes'
>                                       add_parents j (map fst $ pedges $ fromJust
>                                                              $ lookup j network)
>                                           $ add_parents i more_parents after
>                   else add_parents i more_parents after
>     in evalState
>             (add_parents 0 [0] $
>                 do STState tree _ <- get
>                    return $ (0, 0) `Set.delete` tree)
>            $ STState Set.empty (IntSet.fromList [0..length network-1])

PTStore holds probability vectors for each traversed variable. The probability vector for the sink 
is finally used to compute the solution count.

> type Prob = Double
> data PTab = PTab { ptab :: [(Val, Prob)], eta :: Double }
> class PTStore a where
>     ptput :: IVar -> PTab -> a -> a
>     ptget :: IVar -> a -> PTab

> newtype PTMap  = PTMap (IntMap PTab)
> instance PTStore PTMap where
>     ptput v pt (PTMap pts) = PTMap (IntMap.insert v pt pts)
>     ptget v (PTMap pts) = pts IntMap.! v

> score :: DomainStore ds => Network -> ds -> Score
> score network domains =

The network is traversed from the root to the sink.

>     let tree = spanning_tree network
>         traverse  =
             
For each variable, a vector of conditional probabilities of successfully instantiating
each of the values is computed and added to the store.
     
>            let probs (ivar, node) = 
>                    do pts <- get
>                       let ptab = map (\val -> (val, prob pts val)) domain

`eta' is a factor that compensates for multiple paths from a parent to a child. the factor
is pre-computed for each node and used to compute probabilities of value assignments for
childrens of the node. 

>                           eta = let psum = sum $ map snd ptab
>                                 in if psum==0 then 0
>                                    else 
#if SPANTREE
>                                         1/psum
#else
>                                         psum**(1/(fromIntegral $ nsuccs $ node)-1)
#endif
>                       put $ ptput ivar (PTab ptab eta) pts

The probability for a particular value is the product of probabilities that the value
is consistent with each parent.

>                    where domain = domget ivar domains
>                          domain_size = fromIntegral $ length $ domain
>                          prob pts val = 
>                              foldl (\acc (ipar, con) -> acc*prob_par ipar con)
>                                    (1/domain_size) (pedges node)
>                              where prob_par ipar con =
>                                        let PTab ptab eta = ptget ipar pts
>                                        in 
#if SPANTREE 
>                                           (if (ivar, ipar) `Set.member` tree then 1 else eta)
#else
>                                           eta    
#endif
>                                           *foldl (\acc (val_par, p) -> 
>                                                           if con val val_par
>                                                           then acc+p
>                                                           else acc)
>                                                      0.0 ptab
>             in mapM_ probs (reverse network)

At the end of the traversal, the solution count is computed as the product_domain_size times
the probability that a solution is successfully instantiated.

>         solution_count :: PTStore a => State a Score
>         solution_count =
>             do pts <- get
>                return $ watch "BP: sc"
>                       $ (sum $ map snd $ ptab $ ptget 0 pts)*product_domain_size
>             where product_domain_size =
>                       foldl (\pds (ivar, _) ->
>                                  pds*fromIntegral (length (domget ivar domains)))
>                             1.0 network


>     in evalState (traverse >> solution_count) $ PTMap IntMap.empty


== Unit Testing ==

Define a simple domain store for testing.

> newtype DomArr = DomArr (Array Int Domain) deriving Show
> instance DomainStore DomArr where
>     domput ivar domain (DomArr da) = DomArr (da // [(ivar,domain)])
>     domget ivar (DomArr da) = da ! ivar

Check that the solution count is exact for trees and lists.

> testUnconnected =
>     let unconnected = [ (0, Node [] 0)
>                       , (1, Node [] 1)
>                       , (2, Node [] 1) ]
>     in resultEQ (score unconnected (DomArr (listArray (0,2) [[0,1], [0,1], [0,1]])))
>        8
>        "BP.score unconnected"
>                      

> testTree =
>     let tree = [ (0, Node [(1, \x y -> x > y), (2, \x y -> x > y)] 0)
>                , (1, Node [] 1)
>                , (2, Node [] 1) ]
>     in resultEQ (score tree (DomArr (listArray (0,2) [[1,2], [0,1,2], [0,1,2]])))
>        5
>        "BP.score tree"

> testChain =
>     let chain = [ (0, Node [(1, \x y -> x==y)] 0)
>                 , (1, Node [(2, \x y -> x/=y)] 1)
>                 , (2, Node [] 1) ]
>     in resultEQ (score chain (DomArr (listArray (0,2) [[0,1], [0,1], [0,1]])))
>        2
>        "BP.score chain"

Solution count for structures with cycles is approximate.

> testCycle =
>     let cycle = [ (0, Node [(1, \x y -> x==y), (2, \x y -> x==y)] 0)
>                 , (1, Node [(2, \x y -> x==y)] 1)
>                 , (2, Node [] 2) ]
>     in resultEQ (score cycle (DomArr (listArray (0,2) [[0,1], [0,1], [0, 1]])))
>        1 -- should be 2
>        "BP.score cycle"

> testKite =
>     let kite = [ (0, Node [(1, \x y -> x==y), (2, \x y -> x==y)] 0)
>                , (1, Node [(2, \x y -> x==y)] 1)
>                , (2, Node [(3, \x y -> x==y)] 2)
>                , (3, Node [] 1) ]
>     in resultEQ (score kite (DomArr (listArray (0,3) [[0,1], [0,1], [0, 1], [0, 1, 2, 3]])))
>        1 -- should be 2
>        "BP.score kite"


> testsBP = runSuite [ testUnconnected 
>                    , testChain
>                    , testTree 
>                    , testCycle 
>                    , testKite ]


