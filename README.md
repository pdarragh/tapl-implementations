# TAPL Implementations

This repository supplies Haskell implementations of the various languages described in
["Types and Programming Languages" [Pierce, 2002]](https://www.cis.upenn.edu/~bcpierce/tapl/).

There is lots of duplicated code, but that's by design. Each module is meant to be independently runnable and easy to
read. So far, I've left out any fancy parsing, so you'd just have to write out the constructors by hand to execute.

The file names correspond to the number of the latest figure which describes the language at hand, so `3-1-Bools.hs` is
the language of booleans described in figure 3-1. The page number for each figure will be listed in a comment at the top
of each file.

Here's an example interaction:

```
tapl-implementations/ $ ghci
Prelude> :l src/3-2-Arith.hs
*Arith> eval (IfTerm (IfTerm (IsZeroTerm ZeroTerm) FalseTerm TrueTerm) ZeroTerm TrueTerm)
TrueTerm
```

The `eval` function is used to do a complete evaluation of a term. The single-step function ``eval` `` can be used to
evaluate a term to its immediate successor term. However, there is a caveat: sub-evaluations currently do complete
evaluation. For example, consider the following portion of the ``eval` `` function from `3-1-Bools.hs`:

```haskell
eval' (IfTerm t1 t2 t3) = Just (IfTerm (eval t1) t2 t3)
```

The sub-evaluation over term `t1` uses `eval` where it should use ``eval` ``. This is because I strayed from the book's
implementation in choosing to make the type of ``eval` `` to be `Term -> Maybe Term` instead of `Term -> Term`. The book
chooses to raise an error in the no-match case (``eval` _ = ...``), but I'm trying to avoid error handling altogether
and simply haven't gotten around to a proper implementation just yet.
