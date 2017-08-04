# Frontend tool chain

[Leiningen](http://leiningen.org) is for automating Clojure projects
without setting your hair on fire.

## Installation

0. Make sure `curl`, `unzip` and `sassc` are installed.
1. Run `./make.sh`

*IMPORTANT* the `<potato>/public` directory is a temporary
 location. It is recommended to completely purge the directory from
 time to time and rebuild from scratch.

## ClojureScript

To compile, you will need [leiningen](http://leiningen.org); on Mac OS X:

    brew install leiningen

### Starting REPL from command line
From there, *leiningen* will manage all the dependencies. To compile
and get a debug REPL with auto-recompilation and auto-reload, run:

    lein do clean, figwheel

### Starting REPL in `emacs` via `cider`
Start a new REPL, from the source file `web-app/project.clj`:

    M-x cider-jack-in # or `C-c M-j`
     (use 'figwheel-sidecar.repl-api)
     (start-figwheel!)
     (cljs-repl)

