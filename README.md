# [Reflex](https://reflex-frp.org/)

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex.svg)](https://hackage.haskell.org/package/reflex) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/reflex/badge)](https://matrix.hackage.haskell.org/#/package/reflex) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/reflex/blob/master/LICENSE)

Interactive programs without callbacks or side-effects. Functional Reactive Programming (FRP) uses composable events and time-varying values to describe interactive systems as pure functions. Just like other pure functional code, functional reactive code is easier to get right on the first try, maintain, and reuse.

Reflex is a fully-deterministic, higher-order Functional Reactive Programming interface and an engine that efficiently implements that interface.

**Visit https://reflex-frp.org for more information, tutorials, documentation and [examples](https://examples.reflex-frp.org/).**

## Resources

* [Official Website](https://reflex-frp.org)
* [Quick Reference](Quickref.md)
* [Reflex-DOM](https://github.com/reflex-frp/reflex-dom): A framework built on Reflex that facilitates the development of web pages, including highly-interactive single-page apps.
* [Obelisk](https://github.com/obsidiansystems/obelisk#obelisk): A framework built on Reflex and Reflex-DOM for functional reactive web and mobile applications, with batteries included.
* [Get started with Reflex](https://github.com/reflex-frp/reflex-platform)
* [/r/reflexfrp](https://www.reddit.com/r/reflexfrp)
* [irc.freenode.net #reflex-frp](http://webchat.freenode.net?channels=%23reflex-frp&uio=d4)

## Hacking

Use the [`./dev`](dev) script to enter a nix shell with all dependencies available.