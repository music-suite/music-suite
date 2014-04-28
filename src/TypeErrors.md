
## Tips and tricks for dealing with errors

The types used in the suite, and particularly in `music-score` are very generic. The downside is that as a user you may eventually run into some scary type errors. XXX dealing 


### Don't panic

It is *not* necessary to understand everything going on in the types.

### Fix the types

Ambigous type etc

Utility functions `asScore` etc

### Use :info, not :type

### Understand basic lenses

The `lens` library is famously huge. The concepts you need to use Music Suite are
`Lens`, `Prism`, `Iso` and `Traversal`.

If you get an error on the form

    Couldn't match expected type `Time' with actual type `a0 -> f0 a0'

you probably mixed up a lens or getter with an acessor (i.e. `duration` vs `view duration`).

### Get an intuition for what the classes and type functions do

Standard classes, TypeClassopedia
etc.
