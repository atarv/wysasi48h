# Write Yourself a Scheme in 48 Hours

I worked through the wikibook [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), which guides you through writing an interpreter for a subset of R5RS Scheme (a lisp dialect). I think I did most of the provided exercises. Some very very basic programs written in the language can be found in the examples folder. Later I found out, that there's an [updated version of the book](https://wespiser.com/writings/wyas/home.html), which probably better follows current Haskell best practices.

## Building

Get [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then in the repository root run:

```
stack build
```

## Running

`stack run` opens up a REPL. If you want to run a script, pass the script path as an argument, i.e. `stack run examples/hello.scm`.
