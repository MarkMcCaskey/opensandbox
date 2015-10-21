<h1 align="center">
  <a href="https://github.com/oldmanmike/opensandbox">
    Open Sandbox
  </a>
</h1>

<p align="center">
  <a href="https://travis-ci.org/oldmanmike/opensandbox">
    <img alt="Tests"
      src="https://img.shields.io/travis/oldmanmike/opensandbox.svg?style=flat-square">
  </a>
</p>

<p align="center">
  A modular, user-centric Minecraft Server.
</p>

## Installation

As of right now, you can only build opensandbox from source.
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
stack exec opensandbox
```

or...

```bash
cabal run
```
As of right now, it's not even possible to log onto the server. This will change soon.
