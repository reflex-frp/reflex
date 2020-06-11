## [Reflex](https://reflex-frp.org/)

[![Hackage](https://img.shields.io/hackage/v/reflex.svg)](http://hackage.haskell.org/package/reflex)

Build interactive programs without callbacks or side-effects. Functional Reactive Programming (FRP) uses composable events and time-varying values to describe interactive systems as pure functions. Just like other pure functional code, functional reactive code is easier to get right on the first try, maintain, and reuse.

Reflex is a fully-deterministic, higher-order Functional Reactive Programming interface and an engine that efficiently implements that interface.

**Visit https://reflex-frp.org for more information, tutorials, documentation and [examples](https://examples.reflex-frp.org/).**

### Resources

* [Official Website](https://reflex-frp.org)
* [Quick Reference](Quickref.md)
* [Reflex-DOM](https://github.com/reflex-frp/reflex-dom): A framework built on Reflex that facilitates the development of web pages, including highly-interactive single-page apps.
* [Obelisk](https://github.com/obsidiansystems/obelisk#obelisk): A framework built on Reflex and Reflex-DOM for functional reactive web and mobile applications, with batteries included.
* [Get started with Reflex](https://github.com/reflex-frp/reflex-platform)
* [/r/reflexfrp](https://www.reddit.com/r/reflexfrp)
* [irc.freenode.net #reflex-frp](http://webchat.freenode.net?channels=%23reflex-frp&uio=d4)

### Hacking

From the root of a [Reflex Platform](https://github.com/reflex-frp/reflex-platform) checkout, run `./scripts/hack-on haskell-overlays/reflex-packages/dep/reflex`. This will check out the reflex source code into the `haskell-overlays/reflex-packages/dep/reflex` directory. You can then point that checkout at your fork, make changes, etc. Use the `./try-reflex` or `./scripts/work-on` scripts to start a shell in which you can test your changes.
