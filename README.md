# multipac

multipac is a multiplayer version of pacman. It is implemented as a
client-server application; both the client and the server are written in
PureScript. You might find it useful as a learning resource.

## Play

An instance of the game is live at <https://mpac.herokuapp.com/>.

## Overview

`multipac` has examples of many useful PureScript patterns:

* The server and the client are both written in PureScript. The server is built
  on [`purescript-node-http`][], and the server and clients exchange data types
  using the [`Generic` instance deriving][] mechanism together with
  [`purescript-argonaut-generic`][] in order to exchange data as JSON with
  minimal boilerplate.
* The project is built using [Pulp][] together with [npm scripts][]. The
  `package.json` file contains scripts to build the server and the client parts
  of the game.
* Some parts of the code, particularly the `Game` module, make use of lenses
  (from [`purescript-profunctor-lenses`][]) in order to make deeply nested
  updates easy.
* The clients communicate with the server using [HTML5 WebSockets][]. The
  clients use the [`purescript-web-socket`][] library, while the server
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
* The `Game` module contains the bulk of the game logic, exporting functions
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

[HTML5 WebSockets]: http://www.html5rocks.com/en/tutorials/websockets/basics/
[`purescript-profunctor-lenses`]: http://pursuit.purescript.org/packages/purescript-profunctor-lenses
[`purescript-transformers`]: http://pursuit.purescript.org/packages/purescript-transformers
[`purescript-web-socket`]: http://pursuit.purescript.org/packages/purescript-web-socket
[`purescript-node-http`]: http://pursuit.purescript.org/packages/purescript-node-http
[`purescript-argonaut-generic`]: http://pursuit.purescript.org/packages/purescript-argonaut-generic
[`Generic` instance deriving]: https://pursuit.purescript.org/packages/purescript-generics-rep
[Pulp]: https://github.com/purescript-contrib/pulp
[npm scripts]: https://docs.npmjs.com/misc/scripts
