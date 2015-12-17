# A Gulp Based Potato

[Gulp](http://gulpjs.com) is a streaming build system.

# Instructions

0. `brew install npm`
1. `sudo npm install -g gulp`
2. `npm install`
3. `gulp`

Gulp will keep a watch on the source files specified in the
`sources.coffee` file, and publish to the `<potato>/public/assets`
directory automatically when changes are made to those files.

To build for production, use `gulp --type prod`

*IMPORTANT* the `<potato>/public` directory is a temporary
 location. It is recommended to completely purge the directory from
 time to time and rebuild from scratch.

## ClojureScript

To compile, you will need [leiningen](http://leiningen.org); on Mac OS X:

    brew install leiningen

From there, *leiningen* will manage all the dependencies. To compile
and get a debug REPL with auto-recompilation and auto-reload, run:

    lein repl

Usually, I use the REPL directly in Emacs with CIDER. To start a new
REPL, from the source file `C-c M-j` should run a `cider-jack-in`.

To start the debug server, execute in the REPL: `(run)`

# Tips & tricks

View js source in Firefox:

    $ cd public/assets/js
    $ ln -s ../../../web-app/src
