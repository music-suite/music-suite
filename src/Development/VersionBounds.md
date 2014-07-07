
## Version bounds

Annotate all Cabal files with version bounds when possible!

Which bound to use? Some principles:

  * Lower bound should be the version released in the targeted HP
  * Upper bound should generally be the next major version (i.e. if lower bound is 2.2.1, use 3.0)
    * For some libraries it is better to use the *minor* version, i.e. for 2.2.1, use 2.3
    * It is OK to increase the upper bound to the latest Hackage version if you have this version on your own system and want to test it. However take care to see that the lower-bound version works too (i.e. by building on a vanilla HP system).