# Managing sources and builds

Unfortunately, developing a large cross-package system in Cabal is a sligthly cubersome task. There is a utility program [`music-util`](https://github.com/music-suite/music-util), created to assist developers with common tasks. The names and internal dependencies of the music-suite packages are hardcoded into this program.

## Setup source directories

Upgrade `cabal-install` to a 1.18 or later to get support for [sandboxes](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html). This will hopefully be in the Haskell Platform soon, but for now you have to upgrade manually like this:

~~~{.bash}
$ cabal install cabal-install
~~~

Install the utility program (note that this does *not* install the rest of the suite):

~~~{.bash}
$ cabal update
$ cabal install music-util
~~~

Assure that the environment variable `MUSIC_SUITE_DIR` is set to the directory where you want to keep the music-suite sources. Note that `music-util` will never modify anything outside this directory.

~~~{.bash}
$ cat >> ~/.profile 
export MUSIC_SUITE_DIR=/path/to/suite
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
    
Inside the REPL, everything should work as expected. You can load any module from the suite, and changes to the sources become visible whenever you restart the REPL. You can still load modules directly using `:load`.

