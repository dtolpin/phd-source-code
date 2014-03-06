> {-# LANGUAGE CPP #-}

= Debugging Aid =

> module Dibbuk where

> import Debug.Trace

== Debugging Aids ==

#ifndef DEBUG
#define DEBUG 1
#endif

> debugging = DEBUG==1

Print or skip depending on the value of `debugging'.

> debug = if debugging then trace else \_ v -> v

Watch a named variable.

> watch name num = debug (name++"="++(show num)) num
