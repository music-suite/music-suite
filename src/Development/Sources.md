# Managing sources and builds

Cross-package development used to be a cubersome task in Haskell, but fortunately [sandboxes](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html) makes it much easier.

There is a also utility program [`music-util`](https://github.com/music-suite/music-util), created to assist developers with common tasks relating to the Music Suite source directories such as cloning, pushing, pulling and status checking. This program has been granted some knowledge of the names and internal dependencies of the Music Suite packages and it designed to be used with `cabal` and `git`.

This document describes how to install and use `music-util` to manage the Music Suite sources. Note that `music-util` always sets up a development verison of the suite in a dedicated sandbox, regardless of any global installations of the Suite that you may already have.

## Setup

<!--
Upgrade `cabal-install` to a 1.18 or later to get support for sandboxes. This will hopefully be in the Haskell Platform soon, but for now you have to upgrade manually like this:

~~~{.bash}
$ cabal update
$ cabal install cabal-install
~~~
-->

First, assure that you have the latest version of the Haskell Platform. Then install `music-util` (which does *not* depend on the other packages):

~~~{.bash}
$ cabal install music-util
~~~

Assure that the environment variable `MUSIC_SUITE_DIR` is set to the directory where you want to keep the music-suite sources. Note that `music-util` will never modify anything outside this directory.

~~~{.bash}
$ echo "export MUSIC_SUITE_DIR=/path/to/suite" >> ~/.profile
~~~

Run the setup script. This will clone all source repositories and setup a sandbox in `$MUSIC_SUITE_DIR/music-sandbox`. All source directories are configured to use the sandbox by default. It then proceeds to install the libraries and all their dependencies, which may take several minutes.

~~~{.bash}
$ music-util setup
~~~

## Testing

To start an interpreter session, simply use:

~~~{.bash}
$ music-util repl
~~~
    
This will recompile the suite as needed and load it into GHCi. Chnages to the source code becomes visible whenever you restart the REPL. You can still load modules directly using `:load`.

~~~{.bash}
$ music-util repl
ghci> :load examples/annotatations.hs
ghci> main
~~~

TODO run unit tests etc

## Git interaction

To check git status etc

    music-util foreach git status 

### Pull

You should be able to pull from all packages using

    music-util foreach git pull 

### Push

To push to a repo you need to be a member of the [Github organization](https://github.com/music-suite). You can then make a fork of a library as follows (in this example I will use `music-pitch`):

- Go to [https://github.com/music-suite/music-pitch](https://github.com/music-suite/music-pitch)

- Press the Fork button

- In the newly forked repo, find the Clone URL frame, press SSH and then copy the URL 
(which should look like git@github.com:username)

~~~{.bash}
$ cd $MUSIC_SUITE_DIR/music-pitch && git remote add fork <paste-url-here>
~~~

You can then push using `git push fork master`. You can also [set this up as the standard push location](http://sleepycoders.blogspot.se/2012/05/different-git-push-pullfetch-urls.html).


