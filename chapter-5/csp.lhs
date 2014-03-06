= Command-line Driver for CSP Solver =

> module Main where

> import Dibbuk

Problem definition, the algorithm, and the bridge are used to solve a problem:
 - problem definition to parse the standard input:

> import Def

 - the finite domain CSP algorithm to solve the problem:

> import FD

 - the heuristics:

> import XO
> import BP ( spantree, testsBP )
> import SC ( randomsc, orderValuesSC )
> import PAC ( orderValuesPAC, testsPAC )

 - and the bridge to translate from the problem definition (Def) 
   to the FD monad:

> import Bridge

Testing is everything; tests are based on module Test and run at 
the beginning of main.

> import Test

Command-line processing is based on System.Console.GetOpt, and is copied
from the Haskell documentation.

> import System ( getArgs )
> import System.Console.GetOpt
> import Data.Maybe ( fromMaybe )
 
Command-line options allow to specify variable-ordering and value-ordering
heuristics.

> data Flag = VariableOrdering VariableOrdering
>           | ValueOrdering ValueOrdering
>           | All
>           | Help
>     deriving Eq

> data VariableOrdering = VarRandom | VarMCF deriving Eq
> data ValueOrdering = ValRandom | ValLCF | ValSC Double | ValPAC Double Int deriving Eq  

> options :: [OptDescr Flag]
> options =
>     [ Option ['V'] ["var", "variable-ordering"]
>       (ReqArg parseV "Random|MCF")
>       "variable ordering heuristics"
>     , Option ['v'] ["val", "value-ordering"]
>       (ReqArg parsev "Random|LCF|SC|PAC")
>       "value ordering heuristics"
>     , Option ['a'] ["all"]
>       (NoArg All)
>       "find all solutions"
>     , Option ['h'] ["help"]
>       (NoArg Help) 
>       "print help message and exit" ]
>     where parseV "Random" = VariableOrdering VarRandom
>           parseV "MCF" = VariableOrdering VarMCF
>           parseV vo = error $ "unrecognized variable ordering: "++vo
>           parsev "Random" = ValueOrdering ValRandom
>           parsev "LCF" = ValueOrdering ValLCF
>           parsev ('S':'C':':':cost) = ValueOrdering (ValSC (read cost))
>           parsev "SC" = ValueOrdering (ValSC 0.0)
>           parsev "PAC" = ValueOrdering (ValPAC 0.001 20)
>           parsev ('P':'A':'C':':':prec_niter)
>                      = ValueOrdering (let (prec,niter) = read $ "("++prec_niter++")"
>                                       in  ValPAC prec niter)
>           parsev vo = error $ "unrecognized value ordering: "++vo

Function opts wraps getOpt: it receives a list of command line arguments 
and returns the list of flags.

> opts :: [String] -> IO [Flag]
> opts argv = 
>     case getOpt Permute options argv of
>              (o, [], []  ) | Help `elem` o ->
>                                error $ usageInfo header options
>              (o, [], []  ) ->  return o
>              (_, _, errs) ->   error $ concat errs ++ usageInfo header options
>         where header = "Usage: csp [OPTION...]"

The main routine reads Def on input using read from class Read, translates
it to the FD monad, and runs the monad. 

> main = do putStr ("\n= CONFIGURATION =\n\n"++
>                   "DEBUG="++(show Dibbuk.debugging)++"\n"++
>                   "DEEP="++(show FD.deep)++"\n"++
>                   "SPANTREE="++(show BP.spantree)++"\n"++
>                   "RANDOMSC="++(show SC.randomsc)++"\n")
>           putStr "\n= TESTS =\n\n"
>           print tests
>           o <- getArgs >>= opts
>           s <- getContents

Options are processed inside the monad such that def is available.

>           let 

Some options modify the problem definition. They are converted into functions
Def->Def and combined.

>               defOpt (VariableOrdering VarRandom) = orderVariablesRandomly
>               defOpt (VariableOrdering VarMCF) = orderVariablesMCF
>               defOpt _ = id

Some other options must be applied to the monad state, therefore they are
just converted into monadic operations and bound together.

>               fdOpt (ValueOrdering ValRandom) = withValueOrdering orderValuesRandomly
>               fdOpt (ValueOrdering ValLCF) = withValueOrdering orderValuesLCF
>               fdOpt (ValueOrdering (ValSC cost)) =
>                   withValueOrdering (orderValuesSC (bp_of_def def) cost)
>               fdOpt (ValueOrdering (ValPAC prec niter)) =
>                   withValueOrdering (orderValuesPAC (pac_of_def def) prec niter)
>               fdOpt _ = return ()

Now, parse the definition, translate to an FD instance, ...
     
>               def = foldl (flip ($)) (read s) (map defOpt o)
>               fd = foldr (>>) (fd_of_def $ def) (map fdOpt o)

... and compute  and verify the solutions.

>               verisols = let sols = runFD fd
>                          in map (\sol -> (sol, satisfies def sol)) sols

We are experimenting with heuristics which optimize the time to find
the first solution; therefore, unless explicitly specified otherwise,
only one solution is printed.

>           putStr "\n= RESULTS =\n\n"
>           print $ ( if (All `elem` o)
>                     then id
>                     else take 1 ) verisols

== Tests ==

> tests = runSuites
>         [ testsTest
>         , testsDef 
>         , testsFD 
>         , testsBP
>         , testsPAC
>         , testsBridge 
>         , testsXO ]

