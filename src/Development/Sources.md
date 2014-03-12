
# Managing sources and builds

Unfortunately, developing a large cross-package system in Cabal is a sligthly cubersome task. There is a utility program (`music-util`)[https://github.com/music-suite/music-util] 

## Setup

- Install the utility program: `cabal update && cabal install music-util`

- Assure that the environment variable `MUSIC_SUITE_DIR` is set to the directory where you want to keep the music-suite sources.

- `cd $MUSIC_SUITE_DIR && music-util setup`

## Pull

You should be able to pull from all packages using `music-util foreach git pull`. 

## Push

To push to a repo you need to make a fork. For example with the `music-pitch` package you would do it as follows:

- Go to [https://github.com/music-suite/music-pitch](https://github.com/music-suite/music-pitch)

- Press the Fork button

- In the newly forked repo, find the Clone URL frame, press SSH and then copy the URL 
(which should start with git@github.com:username)

- `cd $MUSIC_SUITE_DIR/music-pitch && git remote add fork <paste-url-here>`

You can then push using `git push fork master`. Note that you can [set up different repositories for push and pull](http://sleepycoders.blogspot.se/2012/05/different-git-push-pullfetch-urls.html).

## Installing and testing

It is *highly* recommended to upgrade `cabal-install` to a version supporting sandboxes (1.18 or later).

    cabal install cabal-install

### Without sandboxes

Without sandboxes you can use the `music-util install` command to reinstall a particular package and its dependencies. For testing you generally need to reinstall the preludes:

    cd $MUSIC_SUITE_DIR/music-preludes
    music-util install music-preludes
    ghci
    ghci> :m + Music.Preludes.Basic

### With sandboxes

With sandboxes this is done automatically for you. The `setup` command (see above) creates a sandbox in `$MUSIC_SUITE_DIR/music-sandbox` and configures all packages in `$MUSIC_SUITE_DIR` to use that sandbox.

For testing simply run the new `cabal repl` command in the prelude package

    cd $MUSIC_SUITE_DIR/music-preludes
    cabal repl
