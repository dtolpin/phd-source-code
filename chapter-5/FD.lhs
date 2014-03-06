> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE CPP #-}

#ifndef DEEP
#define DEEP 0
#endif

= Maintaining Arc Consistency =

Borrowed from http://overtond.blogspot.com/2008/07/pre.html, with
modifications. Many of the comments are from the original post.

= Constraint Programming in Haskell =

The Haskell list monad already allows a style of programming that
feels somewhat like non-deterministic logic programming, with
backtracking to do a depth-first left-to-right search. We can build 
a  monad on top of the list monad to do finite domain constraint
programming. In a finite domain solver the variables represent a
finite set of (usually integer) values to which the programmer can add
various constraints.

== Constraint Program Example ==

An example of the kind of constraint program we're aiming to be able
to write is

runTest = runFD test

test = do
    x <- newVar [0..3]
    y <- newVar [0..3]
    (( x .<. y) `mplus` (x `same` y))
    x .:=. 2
    labelling [x, y]

Here we create two finite domain variables x and y, each with an
initial domain of {0, 1, 2, 3}. We then add a constraint that says
either x is less than y or x is the same as y. We then further
constrain x to have the value 2. Finally, the call to labelling will
search for all possible solutions that satisfy the given
constraints. When we evaluate runTest we get the result [[2,3],[2,2]]
which represents the two possible solutions {x = 2, y = 3} and {x = 2,
y = 2}.

Finite domain solvers also allow constraints to contain arithmetic
expressions involving constraint variables and integers. To keep
things simple, we'll leave them out for now. The interface we are
going to implement is shown below.

== Module Definition ==

> module FD (

=== Configuration constants ===

>             deep

=== Types ===

>           , FD           -- Monad for finite domain constraint solver
>           , FDVar(..)    -- Finite domain solver variable
>           , FDState(..)  -- Finite domain solver state
>           , VarInfo(..)  -- Variable info
>           , OrderValues  -- Value ordering heuristics

=== Functions ===

>           , runFD        -- Run the monad and return a list of solutions.
>           , newVar       -- Create a new FDVar
>           , newVars      -- Create multiple FDVars
>           , labelling    -- Backtracking search for all solutions

==== Constraints ====

>           , same         -- Constrain two FDVars to be the same
>           , diff         -- Constrain two FDVars to be different
>           , less         -- Constrain one FDVar to be less than another
>           , nogood       -- Forbid a particular assignment to two FDVars

=== Value Ordering Heuristics ===

>           , withValueOrdering
>           , orderValuesRandomly
>           , orderValuesLCF

== Test suite ==

>           , testsFD
>           ) where


== Imports ==

Modules we need to import:

> import Prelude hiding ( lookup )
> import List ( sortBy, groupBy )
> import Control.Monad.State.Lazy
> import Control.Monad.Trans
> import qualified Data.IntSet as IntSet
> import Data.IntSet ( IntSet )
> import qualified Data.IntMap as IntMap
> import Data.IntMap ( IntMap )

Randomization:

> import Perm ( perm )

Debugging and statistics:

> import Dibbuk 

Tests are everything, test suites are exported and run from main:

> import Test

For dirty debugging:

> import Debug.Trace (trace)

> deep = DEEP==1

= Type Definitions for the Solver ==

Below we define the types for the solver.

The FD monad:

> newtype FD s a = FD { unFD :: StateT (FDState s) [] a }
>     deriving (Monad, MonadPlus, MonadState (FDState s))

FD variables:

> newtype FDVar s = FDVar { unFDVar :: Int } deriving (Ord, Eq, Show)

> type VarSupply s = FDVar s
> data VarInfo s = VarInfo { delayedConstraints :: FD s (),
>                            values :: IntSet }
> type VarMap s = IntMap (VarInfo s)

IntMap is used to map from FDVar to VarInfo.

> insert v vi vm = IntMap.insert (unFDVar v) vi vm
> (!) vm v = vm IntMap.! (unFDVar v)

A value ordering heuristics function takes a state and a variable and
returns the list of values.

> type OrderValues s = FDState s -> FDVar s -> [Int] -> [Int]

> data FDState s = FDState { varSupply :: VarSupply s,
>                            varMap :: VarMap s,
>                            valOrdHeur :: OrderValues s,
>                            propagate :: Bool }

The type FD s a is our constraint solver monad. It contains a list
monad to provide the search capability. This is wrapped in a StateT
monad transformer which threads through the constraint store FDState
s. The type variable s is a phantom type. We will see later how this
can be used to prevent any of the implementation detail "leaking out"
of the monad.

Our constraint store FDState s contains a supply of fresh constraint
variables and also keeps track of the information we need to know
about existing variables. For each existing variable we record its set
of possible values (its domain) and a set of constraints on
it. Whenever the domain of a variable changes, we need to execute its
constraints to check that they are still satisfied. This, in turn, may
further constrain the domain of other variables. This is known as
constraint propagation.

== Running the Monad ==

Run the FD monad and produce a lazy list of possible solutions:

> runFD :: (forall s . FD s a) -> [a]
> runFD fd = evalStateT (unFD fd) initState
> 
> initState :: FDState s
> initState = FDState { varSupply = FDVar 0, 
>                       varMap = IntMap.empty,
>                       valOrdHeur = orderValues,
>                       propagate = True }

The default value ordering heuristics does nothing:

>     where orderValues _ _ [] = []
>           orderValues _ _ vals = watch "FD: order" vals

The function runFD runs a constraint solver, starting with an
initially empty constraint store, and return a list of all possible
solutions. The type (forall s . FD s a) -> [a] ensures that any values
of a type containing the phantom type variable s can't "escape" from
the monad. This means that we can't take a constraint variable from
one monad and use it inside another one, thus ensuring, through the
type system, that the monad is used safely.

== Variables and Constraints ==

Get a new FDVar:

> newVar :: [Int] -> FD s (FDVar s)
> newVar domain = do
>     v <- nextVar
>     v `isOneOf` domain
>     return v
>     where
>         nextVar :: FD s (FDVar s)
>         nextVar = do
>             s <- get
>             let v = varSupply s
>             put $ s { varSupply = FDVar (unFDVar v + 1) }
>             return v
>         isOneOf :: FDVar s -> [Int] -> FD s ()
>         x `isOneOf` domain =
>           do s <- get
>              let vm = varMap s
>                  vi = VarInfo {
>                         delayedConstraints = return (),
>                         values = IntSet.fromList domain }
>              put s { varMap = insert x vi vm }
> 
> newVars :: Int -> [Int] -> FD s [FDVar s]
> newVars n domain = replicateM n (newVar domain)

The function newVar domain creates a new constraint variable
constrained to values in domain. The function newVars n domain is a
convenient way of creating multiple variables with the same domain.

Some helper functions which are not exported, but are used when we
define the constraint functions:

Lookup the current domain of a variable: 

> lookup :: FDVar s -> FD s IntSet
> lookup x = do
>     s <- get
>     return . values $ varMap s ! x

Update the domain of a variable and fire all delayed constraints
associated with that variable:

> update :: FDVar s -> IntSet -> FD s ()
> update x i = do
>     s <- get
>     let vm = varMap s
>     let vi = vm ! x
>     put $ s { varMap = insert x (vi { values = i}) vm }
>     when (propagate s) $ delayedConstraints vi

Add a new constraint for a variable to the constraint store:

> addConstraint :: FDVar s -> FD s () -> FD s ()
> addConstraint x constraint = do
>     s <- get
>     let vm = varMap s
>     let vi = vm ! x
>     let cs = delayedConstraints vi
>     put $ s { varMap =
>         insert x (vi { delayedConstraints = cs >> constraint }) vm }
 
Useful helper function for adding binary constraints between FDVars:

> type BinaryConstraint s = FDVar s -> FDVar s -> FD s ()
> addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
> addBinaryConstraint f x y = do
>     let constraint  = f x y
>     constraint
>     addConstraint x constraint
>     addConstraint y constraint

The function lookup returns the current domain for a variable; update
updates the domain for a variable and propagates the change into all
constraints on that variable; addConstraint inserts a constraint into
the constraint store; addBinaryConstraint tests a constraint on two
variable and then adds it to the constraint store for each variable.

== Constraint Functions ==

Now we can define the actual constraint functions:

Constrain a variable to a particular value. The constraint is processed
immediately and never added to the delayed constraints:

> hasv :: FDVar s -> Int -> FD s ()
> var `hasv` val = do
>     vals <- lookup var
>     guard $ val `IntSet.member` vals
>     let i = IntSet.singleton val
>     when (i /= vals) $ update var i

In hasv we lookup the current domain of the variable and test that
the value to be set is within the domain. If the domain has changed,
we update the constraint store and propagate the change. The other
constraints are defined similarly:

Constrain two variables to have the same value:

> same :: FDVar s -> FDVar s -> FD s ()
> same = addBinaryConstraint $ \x y -> do
>     xv <- lookup x
>     yv <- lookup y
>     let i = IntSet.intersection xv yv
>     guard $ not $ IntSet.null i
>     when (i /= xv) $ update x i
>     when (i /= yv) $ update y i

Constrain two variables to have different values:

> diff :: FDVar s -> FDVar s -> FD s ()
> diff = addBinaryConstraint $ \x y -> do
>     xv <- lookup x
>     yv <- lookup y
>     guard $ IntSet.size xv > 1 || IntSet.size yv > 1 || xv /= yv
>     when (IntSet.size xv == 1 && xv `IntSet.isProperSubsetOf` yv) $
>         update y (yv `IntSet.difference` xv)
>     when (IntSet.size yv == 1 && yv `IntSet.isProperSubsetOf` xv) $
>         update x (xv `IntSet.difference` yv)

Constrain one variable to have a value less than the value of another
variable:

> less :: FDVar s -> FDVar s -> FD s ()
> less = addBinaryConstraint $ \x y -> do
>     xv <- lookup x
>     yv <- lookup y
>     let xv' = IntSet.filter (< IntSet.findMax yv) xv
>     let yv' = IntSet.filter (> IntSet.findMin xv) yv
>     guard $ not $ IntSet.null xv'
>     guard $ not $ IntSet.null yv'
>     when (xv /= xv') $ update x xv'
>     when (yv /= yv') $ update y yv'

Forbid a particular assignment to two FDVars, useful with random CSPs.

> nogood :: [(Int, Int)] -> FDVar s -> FDVar s -> FD s ()
> nogood nogoods = 

Precompute antisupports:

>     let xas = antisupports nogoods
>         yas = antisupports $ map (\(vx, vy) -> (vy, vx)) nogoods

To compute removal set for a variable, we need the variable domain
and the list of antisupports. We use the codomain as the initial
value for the removal set.

>         remset dom codom as = 
>             IntSet.fold (\val rs -> rs `IntSet.intersection`
>                          (IntMap.findWithDefault IntSet.empty val as))
>             codom dom

Non-empty removal set triggers constraint propagation.

>         remove x xv yv yas =
>                  do let xrs = remset yv xv yas
>                     when (not $ IntSet.null xrs) $
>                          update x $ xv `IntSet.difference` xrs
>                                 

>     in addBinaryConstraint $ \x y -> do
>          xv <- lookup x
>          yv <- lookup y
>          remove x xv yv yas
>          remove y yv xv xas

where 

> antisupports = IntMap.fromList .
>                map (\g -> (fst $ head g, IntSet.fromList $ map snd g)) .
>                groupBy (\(a, _) (b, _) -> a==b) .
>                sortBy (\(a, _) (b, _) -> compare a b)

== Finding Solutions ==

Finally, in the labelling function we make use of the underlying list
monad to search for all solutions for the given set of variables.

Label variables using a depth-first left-to-right search:

> labelling :: [FDVar s] -> FD s [Int]
> labelling = mapM label where
>     label var = debug ("FD: var="++(show var)) $
>             do s <- get
>                val <- FD . lift $ orderValues s var
>                                 $ fetchValues s var
>                var `hasv` (debug ("FD: val="++(show val)) val)
>                return val

Values are fetched as a list in a natural order, and then ordered
by a value ordering heuristics.

>     fetchValues s = IntSet.toList . values . (varMap s !)
>     orderValues s = valOrdHeur s s

In a later post I plan to show how to use this finite domain solver
monad to write a solver for Sudoku puzzles, and extend the monad to
support arithmetic expressions.

== Value Ordering Heuristics ==

When a single solution must be found, a value ordering heuristics can
be used instead of left-to-right search to improve the running time.
An heuristic is a function from the state and the variable to the
order of values. Value ordering heuristics can be combined, such
that the later heuristics improves the ordering offered by the earlier
one.

> withValueOrdering :: OrderValues s -> FD s ()
> withValueOrdering orderValues = 
>     do s <- get
>        let orderValues' = valOrdHeur s
>        put $ s { valOrdHeur =
>                  \s var -> orderValues s var . orderValues' s var }

I am going to eventually experiment with the following heuristics:

 - least constrained first;
 - solution count;
 - selective solution count.

=== Random Ordering ===

This heuristics randomly shuffles the values in the domain. The heuristic
ensures independence of the experiment outcomes of the predefined value order.

> orderValuesRandomly :: OrderValues s
> orderValuesRandomly s var vals = perm vals

=== Least Constrained Value ===

The heuristics computes the domain size after (shallow or deep, see DEEP)
application of hasv constraint for each of the values ...

> orderValuesLCF :: OrderValues s
> orderValuesLCF s var vals = 
>     let domsizes = zip vals $
>                do val <- vals
>                   evalStateT (unFD (tryv var val)) s

and arranges the values in the descending order by the domain size:

>     in case vals of
>          [] -> []
>          _ -> map fst 
>               $ watch "FD: LCF: order"
>               $ sortBy (\x y -> compare (snd y) (snd x)) 
>               $ filter (\(_, n) -> n /= 0.0)
>               $ domsizes

Where tryv is (informally) an implementation of hasv>>=shallowUpdate:

> tryv :: FDVar s -> Int -> FD s Double
> tryv var val =
>     do s <- get
>        let vm = varMap s
>        let vi = vm ! var
>        put $ s { varMap = insert var (vi { values = IntSet.singleton val}) vm,
>                  propagate = deep }
>        delayedConstraints vi

After variable domains are updated by shallow application of
constraints, the product of domain sizes is computed:

>        s <- get
>        return $ domSize $ varMap s
>     where domSize vm = IntMap.fold (\x sz -> fromIntegral (IntSet.size (values x)) * sz) 1.0 vm

=== Solution Count ===

Heuristics based on solution count are implemented in module SC.

==== Exhaustive Solution Count ====

For each value assignment, the number of
solutions is estimated, and the assignments are ordered by the
decreasing number of solutions.

==== Selective Solution Count ====

The number of solutions is estimated for an assignment
only if the value of information of the estimation is positive.

== Tests ==

An empty problem.

> testEmpty = resultEQ
>             (runFD $ labelling [])
>             [[]]
>             "FD.Empty"

An incosistent problem.

> testInconsistent = resultEQ
>                    (runFD $ do var <- newVar [1]
>                                hasv var 2
>                                labelling [var])
>                    []
>                    "FD.Inconsistent"

A single variable.

> testSingletone = resultEQ
>              (runFD $ do vars <- newVars 1 [1]
>                          labelling vars)
>              [[1]]
>              "FD.Single"

Two variables, one constraint.

> testLess = resultEQ
>            (runFD $ do [a,b] <- newVars 2 [1,2]
>                        less a b
>                        labelling [a,b])
>            ([[1,2]])
>            "FD.Less"

Ambiguious, many solutions.

> testMany = resultEQ 
>            (runFD $ do var <- newVar [1,2]
>                        labelling [var])
>            ([[1],[2]])
>            "FD.Many"

=== No goods ===

A separate test for antisupports, I failed in the implementation initially:

> testAntisupports = resultEQ 
>                    (antisupports [(1,2),(2,2),(1,1), (3, 1), (1, 3)])
>                    (IntMap.fromList [(1, IntSet.fromList [1,2,3]),
>                                      (2, IntSet.fromList [2]),
>                                      (3, IntSet.fromList [1])])
>                    "FD.antisupports"

A nogood constraint that does not affect the domains.

> testNogoodVacuous = resultEQ
>                     (runFD $ do a <- newVar [1]
>                                 b <- newVar [2]
>                                 nogood [(2,1)] a b
>                                 labelling [a, b])
>                     ([[1,2]])
>                     "FD.Nogood Vacuous"

An inonsistent problem instant.

> testNogoodUnsat = resultEQ
>                   (runFD $ do a <- newVar [1]
>                               b <- newVar [2]
>                               nogood [(1,2)] a b
>                               labelling [a, b])
>                   ([])
>                   "FD.Nogood Unsat"
>                   

A relatively general case.

> testNogood = resultEQ
>               (runFD $ do vars@[a, b] <- newVars 2 [1, 2]
>                           nogood [(1,2), (2,2)] a b
>                           labelling vars)
>               ([[1, 1], [2, 1]])
>               "FD.Nogood"

All tests in a test suite.

> testsFD = runSuite [ testEmpty
>                    , testInconsistent
>                    , testSingletone
>                    , testLess 
>                    , testMany
>                    , testAntisupports
>                    , testNogoodVacuous
>                    , testNogoodUnsat
>                    , testNogood ]
