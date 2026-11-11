# hf-power-analysis

Amateur radio log and HF signal power analysis in Clojure.

## Features

- ADIF log parsing and HF power/signal analysis.
- Interactive live literate programming workbook powered by [Clay](https://github.com/scicloj/clay).
- Built-in support for CIDER (`cider-nrepl` and `refactor-nrepl`) when running Clay or a development nREPL server.

## Clay Live Workbook & CIDER

### Running Live Clay Session with CIDER Support

When running Clay live-reload via CLI:

    $ clj -M:clay

Clay automatically starts both its HTTP viewer server and an nREPL server with `.nrepl.edn` middleware configured:
- `cider/cider-nrepl` (0.64.1)
- `refactor-nrepl/refactor-nrepl` (3.15.0)
- Writes `.nrepl-port` to the project root.

To connect from Emacs/CIDER:
1. Run `M-x cider-connect-clj` (or `cider-connect-sibling-clj`).
2. Point host to `localhost` and port to the value in `.nrepl-port`.

To render without launching the live nREPL/watcher:

    $ clj -M:clay -m '{:source-path "notebooks/index.clj"}' -r

### Running via CIDER Jack-In or Custom REPL

If you prefer starting your REPL inside Emacs via `cider-jack-in-clj` (`C-c C-x j j`):
1. Include the `:clay` or `:dev` alias when prompted.
2. In the REPL:
   ```clojure
   (require '[dev :refer [start-clay! make-clay!]])

   ;; Start Clay watcher & browser:
   (start-clay!)

   ;; Or re-render on demand:
   (make-clay!)
   ```

## Running the Application

Run the project directly via `:exec-fn`:

    $ clojure -X:run-x
    Hello, Clojure!

Run with custom args:

    $ clojure -X:run-x :name '"Someone"'
    Hello, Someone!

Run via `:main-opts`:

    $ clojure -M:run-m
    Hello, World!

## Tests and CI

Run tests:

    $ clojure -T:build test

Run the CI pipeline and generate an uberjar:

    $ clojure -T:build ci

Check for outdated dependencies:

    $ clj -X:search/outdated

## License

Copyright © 2026 John

Distributed under the [Eclipse Public License 2.0](https://www.eclipse.org/legal/epl-2.0).
