<h1 align="center">
  <a href="https://github.com/oldmanmike/opensandbox">
    OpenSandbox
  </a>
</h1>

<p align="center">
  <a href="https://travis-ci.org/oldmanmike/opensandbox">
    <img alt="Tests"
      src="https://img.shields.io/travis/oldmanmike/opensandbox.svg?style=flat-square">
  </a>
</p>

<p align="center">
  A modular, user-centric, concurrent Minecraft Server.
</p>

OpenSandbox is a WIP, free and open source, custom Minecraft server implemented in Haskell.
The goal of this server is not to perfectly clone the features of the Notchian server, but rather to improve on the original game through purely server-side features that maintain Notchian client compatibility.

Planned features are:
* Online, incremental, and purely functional world generation
* Content version control for blocks & chunks (snapshotting, rollback/undo, branching, etc.)
* Pervasive parallelism and concurrency support (multi-threaded, GPGPU, distributed)
* Significantly better sysadmin tools
* Modular, composable, and orthogonal DSL for good modding support and server customization.

## Installation

As of right now, you can only build Open Sandbox from source.
The following is with `stack`
```bash
git clone http://github.com/oldmanmike/opensandbox.git
cd opensandbox
stack build
```

or with `cabal`...

```bash
git clone http://github.com/oldmanmike/opensandbox.git
cd opensandbox
cabal sandbox init
cabal install
cabal build
```

## Usage

You currently can launch the server following a successful build via:
```bash
stack exec server
```

or...

```bash
cabal run
```
As of right now, it's not even possible to log onto the server. This will change soon.

## Contributing

Pull requests and bug reports are welcome even at this early date. Everything is subject to change right now, so many flaws are present and known right now. However, Haskellers interested in the project should feel free to find a role for themselves. I'm not a Haskell expert, so I don't expect a contributor to be either.
