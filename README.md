Missile
====

Missile is a GUI for the game of points.

It's written in haskell language and implements "points console AI protocol v6" for playing with bots.

The one bot that implements this protocol is "[opai-rs](https://github.com/kurnevsky/opai-rs)".

Running
====

First of all you will need cabal with ghc installed on your system.

Then update list of packages:

```sh
    cabal update
```

If your distro doesn't have the latest cabal you can update it with:

```sh
    cabal install cabal-install
    export PATH=~/.cabal/bin:$PATH
```

Then init cabal sandbox in the directory with cloned repo:

```sh
    cabal sandbox init
```

Install dependencies:

```sh
    cabal install --dependencies-only --enable-optimisation=2
```

Compile with:

```sh
    cabal build
```

Run with

```sh
    ./dist/build/missile/missile
```

License
====

This project is licensed under AGPL version 3 or (at your option) any later version. See LICENSE.txt for details.

Copyright (C) 2015 Kurnevsky Evgeny
