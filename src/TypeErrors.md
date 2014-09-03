
## Tips and tricks for dealing with errors

The types used in the suite, and particularly in `music-score` are very generic. The downside is that as a user you may eventually run into some scary type errors. XXX dealing 


### Don't panic

It is not necessary to understand *everything* going on in the types, but a general idea of what they mean is very helpful.

### Fix the types

Ambigous type etc

Utility functions `asScore` etc

### Use :info, not :type

### Understand Semigroup, Monoid, Applicative and Monad

Standard classes, TypeClassopedia
etc.

### Understand lenses, prisms, isos and traversals (intuitively)

The `lens` library uses state-of-the art Haskell to provide a very generic way of accessing nested structure. 

The concepts you need to use Music Suite are
`Lens`, `Prism`, `Iso` and `Traversal`.

The most common error in code that uses lenses is mixing up a lenses with functions, i.e. writing `duration x` when you mean `x^.duration` (or `view duration x`). This will give you an error that looks something like this:

    Couldn't match expected type `Duration' with actual type `a0 -> f0 a0'
