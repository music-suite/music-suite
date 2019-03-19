
# hslinks

`hslinks` is a simple utility program that resolves links to Haskell identifiers in Markdown-style text. This is mainly useful if you want to maintain documentation
of a Haskell program or library outside source code but still have links the
Haddock-generated API documentation.

Invoke with a list of Cabal files as follows

    hslinks foo/Foo.cabal bar/Bar.cabal ... <input-file >output-file

The program acts as a text transformer that replaces

* `@[foo]` with `[foo][foo]`
* `@@@hslinks@@@` with *module index* consisting of URLs to Haddock files

For an example, see `Test.md`.

`hslinks` uses the modules currently installed on the system. If you have a sandbox, you can set `GHC_PACKAGE_PATH` before invoking it.


## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
