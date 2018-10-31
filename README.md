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

The `eval` function is used to do a complete evaluation of a term. The single-step function `eval'` can be used to
evaluate a term to its immediate successor term (`t -> t'`). In the case where `eval'` must make recursive calls to
itself, some fun operators are used, namely `<$>` and the new `<#>`. These are discussed more
[below](#fun-with-applicative-functors).

## Fun with applicative functors

The book guides a small-step interpretation style, where the function `eval'` takes a term `t` and computes the
immediate successor term `t'` based on the evaluation rules. Sometimes, no move forward can be made. This occurs most
often when attempting to evaluate a *value*, since values do not evaluate.

The book chooses a simple implementation. In the case where no steps can be taken, a `NoRuleApplies` error is raised.
This error is handled in the `eval` function by merely returning whatever term was passed in:

```ocaml
let rec eval ctx t =                                                                                                                
    try let t' = eval1 ctx t                                                                                                          
        in eval ctx t'                                                                                                                
    with NoRuleApplies -> t     
```

For my implementations, I wanted to avoid using the error-handling method. The book even suggests doing this:

> Another possibility would be to make the single-step evaluator return a `term option` indicating whether it was
> successful and, if so, giving the resulting term; this would also work fine, but would require a little more
> bookkeeping.
>
> *Types and Programming Languages*, p. 47 (First paragraph of §4.2.)

I have opted for this approach, giving my small-step evaluation function the type `eval' :: Term -> Maybe Term`.
However, this causes a problem. Consider the following excerpt from my original implementation of the Boolean language
described in Figure 3-1:

```haskell
data Term
    = ...
    | IfTerm Term Term Term
    ...

...

eval' :: Term -> Maybe Term
...
eval' (IfTerm t1 t2 t3) = Just (IfTerm (eval t1) t2 t3)
...
```

In the implementation of `eval'`, there is a call to `eval t1`. A correct implementation would use `eval'` instead, but
this makes things a little tricky. `IfTerm` has type `Term -> Term -> Term -> Term`, but `eval'` would produce a
`Maybe Term` instead.

To solve this problem, we turn to the solutions offered by applicative functors (defined using the Applicative typeclass
in Haskell). Specifically, there are two operators that will be useful in this case: `<$>` and `<*>`.

`<$>` has type `Applicative f => (a -> b) -> f a -> f b`. This means that we can take an argument which is in an
applicative, apply a function to its interior, and produce a new instance of the applicative. This will be useful
because now we can take our `IfTerm :: Term -> Term -> Term -> Term` and our `eval' t1 :: Maybe Term` (where
`t1 :: Term`) and we get a new `Maybe (Term -> Term -> Term)`.

All that's left is to apply the remaining two arguments, `t2 :: Term` and `t3 :: Term`. Now we use `<*>`, which has type
`Applicative f => f (a -> b) -> f a -> f b`. Unfortunately, we have to lift the terms `t2` and `t3` into the applicative
at hand manually (so we can have `f a`), but that isn't so bad.

We arrive at the following implementation:

```haskell
eval' (IfTerm t1 t2 t3) = IfTerm <$> eval' t1 <*> pure t2 <*> pure t3
```

I felt that this was a little bit verbose for my liking, so I set out to find a function with type
`Applicative f => f (a -> b) -> a -> f b`. Sadly, I couldn't find anything in the standard library, so instead I wrote
my own!

```haskell
(<#>) :: Applicative f => f (a -> b) -> a -> f b
(<#>) func x = func <*> pure x
infixl 4 <#>
```

The `infixl 4 <#>` is used to assign a precedence to the operator to ensure computations are handled in the correct
order.

Now the previous line can be implemented instead as:

```haskell
eval' (IfTerm t1 t2 t3) = IfTerm <$> eval' t1 <#> t2 <#> t3
```

It isn't very different really, but it avoids having to use `pure` manually and it also gave me the opportunity to delve
a little more into applicative functors!
