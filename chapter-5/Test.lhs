= Simple support for testing =

> module Test( Result(..)
>            , result
>            , resultEQ
>            , Results(..)
>            , run
>            , runSuite
>            , runSuites
>            , testsTest ) where

> import Data.Monoid

A test either succeeds or fails with an error.

> type ErrorMessage = String
> data Result = Success | Failure ErrorMessage deriving Show

Every test has a name.

> type Name = String

The result of a single test is a (name,result) tuple.

> result ok name err = (name,if ok then Success else Failure err)
> resultEQ :: (Show a, Eq a) => a -> a -> String -> (String,Result)
> resultEQ got exp name = result (got==exp) name ("got "++(show got)
>                                                 ++", expected "++(show exp))

The result of running a test suite is two lists:
  
  - names of tests which succeeded;
  - names and error messages of tests which failed.

> data Results = Results {successes :: [Name], failures :: [(Name,ErrorMessage)]}
>                deriving (Show,Eq)

Every single test must return test name and result a sa tuple:

> type Outcome = (Name,Result)


When test is run in a test suite, its result is appended to the corresponding list.

> run suite test = let (name,result) = test
>                  in case result of
>                       Success -> suite {successes = name:successes suite}
>                       Failure err -> suite {failures = (name,err):failures suite}

A suite is a list of tests:

> runSuite tests = foldl run mempty (reverse tests)
> runSuites suits = mconcat suits

A suite is a monoid:

> instance Monoid Results where
>     mempty = Results [] []
>     mappend a b = Results ((successes a)++(successes b)) ((failures a)++(failures b))
                                
A self-test-suite for the Test module.

A single result should be appended to the proper result branch:

> testRunSuccess = resultEQ (mempty `run` ("x",Success)) (Results ["x"] [])
>                  "Test.run success"
> testRunFailure = resultEQ (mempty `run` ("x",Failure "failure"))
>                  (Results [] [("x", "failure")])
>                  "Test.run failure"

Results of two test suites must join properly:

> testRunSuites = resultEQ (runSuites [Results ["s1"] [("f1","e1"),("f2","e2")],
>                                      Results ["s2","s3"] [("f3","e3")]])
>                          (Results ["s1","s2","s3"]
>                           [("f1","e1"), ("f2","e2"),("f3","e3")])
>                        "Test.Results.mappend"

All tests together form a test suite:

> testsTest = runSuite [testRunSuccess,testRunFailure,testRunSuites]
