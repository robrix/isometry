# `isometry`

Messing around with programmatic tiling in an isometric setting.


## Development

Development currently assumes a Mac with `ghc` 8.8 & `cabal` 3.0. You can install them directly, or use [`ghcup`](https://www.haskell.org/ghcup/).

Initial setup:

```bash
brew bundle # for sdl2 & sqlite3
cat data/ephemerides.sql | sqlite3 data/data.db # to populate the solar system db with planets
cat data/factions.sql | sqlite3 data/data.db # to populate the solar system db with factions
cabal build # to set up dist-newstyle with the ghc package db
```

Run `script/repl` to load the project (both library & executable) into the REPL. In the REPL, `:main` will launch the game. Use `:main --profile` to enable profiling (timings for various parts of the game, shown on exit).

Alternatively, `cabal run isometry` will launch the game. Use `cabal run isometry -- --profile` to enable profiling.
