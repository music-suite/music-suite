
## Getting the sources

- Install the utility program: `cabal update && cabal install music-util`

- Assure that the environment variable `MUSIC_SUITE_DIR` is set to the directory where you want to keep the music-suite sources.

- `cd $MUSIC_SUITE_DIR && music-util setup`

- To build and install the whole suite, use `music-util install music-preludes`

You should be able to pull from all packages using `music-util foreach git pull`. To push to a repo you need to make a fork. For example with the `music-pitch` package you would do it as follows:

- Go to [https://github.com/music-suite/music-pitch]()

- Press the Fork button

- In the newly forked repo, find the Clone URL frame, press SSH and then copy the URL 
(which should start with git@github.com:username)

- `cd $MUSIC_SUITE_DIR/music-pitch && git remote add fork <paste-url-here>`

You can then push using `git push fork master`. Note that you can [set up different repositories for push and pull](http://sleepycoders.blogspot.se/2012/05/different-git-push-pullfetch-urls.html).