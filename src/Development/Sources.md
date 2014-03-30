# Managing sources and builds

Developing a large cross-package system in Haskell used to be a cubersome task, but fortunately [sandboxes](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html) makes it much easier.

There is a also utility program [`music-util`](https://github.com/music-suite/music-util), created to assist developers with common tasks relating to the Music Suite source directories such as cloning, pushing, pulling and status checking. This program has been granted some knowledge of the names and internal dependencies of the Music Suite packages and it designed to be used with `cabal` and `git`.

This document describes how to install and use `music-util` to manage the Music Suite sources. It assumes that you are a member of the [Github organization](https://github.com/music-suite) with at least Read access to the libraries. Note that `music-util` always sets up a development verison of the suite in a dedicated sandbox, regardless of any global installations of the Suite that you may already have.


## First steps

Upgrade `cabal-install` to a 1.18 or later to get support for sandboxes. This will hopefully be in the Haskell Platform soon, but for now you have to upgrade manually like this:

~~~{.bash}
$ cabal update
$ cabal install cabal-install
~~~

Install the utility program (note that this does *not* install the rest of the suite):

~~~{.bash}
$ cabal update
$ cabal install music-util
~~~

Assure that the environment variable `MUSIC_SUITE_DIR` is set to the directory where you want to keep the music-suite sources. Note that `music-util` will never modify anything outside this directory.

~~~{.bash}
$ echo "export MUSIC_SUITE_DIR=/path/to/suite" >> ~/.profile
~~~

Run the setup script. This will clone all source repos and setup a sandbox in `$MUSIC_SUITE_DIR/music-sandbox`. All source directories are configured to use the sandbox by default.

~~~{.bash}
music-util setup
~~~


## Status

To check git status etc

    music-util foreach git status 

## Pull

You should be able to pull from all packages using

    music-util foreach git pull 

## Push

To push to a repo you need to make a fork. For example with the `music-pitch` package you would do it as follows:

- Go to [https://github.com/music-suite/music-pitch](https://github.com/music-suite/music-pitch)

- Press the Fork button

- In the newly forked repo, find the Clone URL frame, press SSH and then copy the URL 
(which should start with git@github.com:username)

- `cd $MUSIC_SUITE_DIR/music-pitch && git remote add fork <paste-url-here>`

You can then push using `git push fork master`. Note that you can [set up different repositories for push and pull](http://sleepycoders.blogspot.se/2012/05/different-git-push-pullfetch-urls.html).


## Building and testing

To test a particular package on the REPL, move into its source directory and run `cabal repl`. For testing the "standard" part of the suite you usually want to test from the `preludes` directory. 

    cd $MUSIC_SUITE_DIR/music-preludes
    cabal repl
    
Inside the REPL, everything should work as expected. You can import any module from the suite, and changes to these modules becomes visible whenever you restart the REPL. You can still load modules directly using `:load`.

    $ cabal repl
    ghci> :load examples/annotatations.hs
    ghci> main

