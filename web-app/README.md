# Frontend tool chain

[npm](https://www.npmjs.com) is the package manager for JavaScript.
[Bower](http://bower.io) is package manager for the web.
[Gulp](http://gulpjs.com) is a streaming build system.
[Leiningen](http://leiningen.org) is for automating Clojure projects
without setting your hair on fire.

## Installation on OS X

0. `brew install npm`
1. `sudo npm install -g gulp`
2. `npm install`
3. `sudo npm install -g bower`
4. `bower`
5. `gulp`


Bower will download font assets as describe in `bowet.json`. Gulp will
keep a watch on the source files specified in the `sources.coffee`
file, and publish to the `<potato>/public/assets` directory automatically when changes are made to those files.

To build for production, use `gulp --type prod`

*IMPORTANT* the `<potato>/public` directory is a temporary
 location. It is recommended to completely purge the directory from
 time to time and rebuild from scratch.

## ClojureScript

To compile, you will need [leiningen](http://leiningen.org); on Mac OS X:

    brew install leiningen

### Starting REPL from command line
From there, *leiningen* will manage all the dependencies. To compile
and get a debug REPL with auto-recompilation and auto-reload, run:

    lein figwheel

### Starting REPL in `emacs` via `cider`
Start a new REPL, from the source file `web-app/project.clj`:

    M-x cider-jack-in # or `C-c M-j`
     (use 'figwheel-sidecar.repl-api)
     (start-figwheel!)
     (cljs-repl)

# View the source in Firefox

At the time of writing only Firefox allow us to view the source in  `closurescript`, to do
so:

    $ cd public/assets/js
    $ ln -s ../../../web-app/src
