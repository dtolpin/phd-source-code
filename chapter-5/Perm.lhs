= List permutation =

> module Perm ( perm , permIO ) where

> import Random
> import System.IO.Unsafe (unsafePerformIO)

Permutation requires escaping IO monad.

> perm :: [a] -> [a]
> perm = unsafePerformIO . permIO

Permutating a list in IO monad.

> permIO :: [a] -> IO [a]
> permIO l = 
>     let permIO' [] p = return p
>         permIO' l p = 
>             do i <- randomRIO (0, (length l) - 1)
>                let (head, x:tail) = splitAt i l
>                permIO' (head++tail) (x:p)
>     in permIO' l []






