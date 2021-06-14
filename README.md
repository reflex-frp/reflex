## [Reflex](https://reflex-frp.org/)

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex.svg)](https://hackage.haskell.org/package/reflex) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/reflex/badge)](https://matrix.hackage.haskell.org/#/package/reflex) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/reflex/blob/master/LICENSE)

### Practical Functional Reactive Programming

Reflex is a fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface.

[Reflex-DOM](https://github.com/reflex-frp/reflex-dom) is a framework built on Reflex that facilitates the development of web pages, including highly-interactive single-page apps.

A summary of Reflex functions is available in the [quick reference](Quickref.md).

**Visit https://reflex-frp.org/ for more information, tutorials, documentation and [examples](https://examples.reflex-frp.org/).**

### Resources
[Get started with Reflex](https://github.com/reflex-frp/reflex-platform)

[/r/reflexfrp](https://www.reddit.com/r/reflexfrp)

[irc.freenode.net #reflex-frp](http://webchat.freenode.net?channels=%23reflex-frp&uio=d4)

### Hacking

From the root of a [Reflex
Platform](https://github.com/reflex-frp/reflex-platform) checkout, run
`./scripts/hack-on haskell-overlays/reflex-packages/dep/reflex`. This
will check out the reflex source code into the
`haskell-overlays/reflex-packages/dep/reflex` directory. You can then
point that checkout at your fork, make changes, etc. Use the
`./try-reflex` or `./scripts/work-on` scripts to start a shell in
which you can test your changes.
