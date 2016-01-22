# multipac

multipac is a multiplayer version of pacman. Since it's featured on the
<http://purescript.org> website, I recently decided it would be nice to tidy it
up and document it a bit, so that it can be used as a learning resource too.

## Play

An instance of the game is live at <https://mpac.herokuapp.com/>.

## Overview

`multipac` has examples of many useful PureScript patterns:

* The server and the client are both written in PureScript. The server is built
  on [`purescript-node-http`][], and the server and clients exchange data types
  using the [`Generic` instance deriving][] mechanism together with
  [`purescript-foreign-generic`][] in order to exchange data as JSON with
  minimal boilerplate.
* The project is built using [Pulp][] together with [npm scripts][]. The
  `package.json` file contains scripts to build the server and the client parts
  of the game.
* Some parts of the code, particularly the `Game` module, make use of lenses
  (from [`purescript-profunctor-lenses`][]) in order to make deeply nested
  updates easy.
* The clients communicate with the server using [HTML5 WebSockets][]. The
  clients use the [`purescript-websocket-simple`][] library, while the server
  uses a JavaScript WebSocket server implementation from npm, namely,
  `websocket`.
* The game-stepping function uses monad transformers from
  [`purescript-transformers`][]: the `StateT` transfomer, which holds
  information about all the objects in the current game, as well as a `WriterT`
  transformer, which accumulates information about all the changes which were
  made to the game. The `WriterT` is used so that all of the changes which
  occur in each game step can be sent to each of the clients, so that they can
  update their versions of the game.

If you want to browse the source code, here are some pointers:

* The `Types` module is perhaps the most important module. It contains type
  declarations and a few basic functions for all of the types involved, in
  particular, types for games, players, game items, messages sent between the
  server and clients, and so on. Most of the other modules import `Types`.
* The `Game` module contains the bulk of the game logic. exporting functions
  such as `stepGame`, which advances the game by one step, returning a new game
  and a list of things that changed.
* The `Rendering` module is for functions which draw a game onto an HTML canvas
  element.
* `BaseServer` contains a basic HTTP server which deliberately has no knowledge
  of multipac specifically, and could potentially be useful for implementing
  any similar multiplayer HTML5 game. `Server`, which is the entry point module
  for the server part, builds on `BaseServer`. Likewise, `BaseClient`
  deliberately has no knowledge of multipac specifically, and `Client`, the
  client entry point module, builds on `BaseClient`.

Note that not everything in here is perfect! I'm sure there are lots of places
that could benefit from a little bit of refactoring. Please feel free to ask if
anything seems weird &mdash; I would like this project to function as a good
learning resource as well as a fun experiment.

## Notes

If you want to compile multipac, it currently requires the `master` version of
the PureScript compiler, because of a bug in `Generic` instance deriving. At
the time of writing, the most recent version, 0.8.0-rc1, is affected by this
bug. Any later releases will most probably contain the fix.

[HTML5 WebSockets]: http://www.html5rocks.com/en/tutorials/websockets/basics/
[`purescript-profunctor-lenses`]: http://pursuit.purescript.org/packages/purescript-profunctor-lenses
[`purescript-transformers`]: http://pursuit.purescript.org/packages/purescript-transformers
[`purescript-node-http`]: http://pursuit.purescript.org/packages/purescript-node-http
[`purescript-foreign-generic`]: http://pursuit.purescript.org/packages/purescript-foreign-generic
[`Generic` instance deriving]: http://www.purescript.org/learn/generic/
[Pulp]: https://github.com/bodil/pulp
[npm scripts]: https://docs.npmjs.com/misc/scripts
