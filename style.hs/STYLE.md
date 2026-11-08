# Obsidian Systems Haskell Style Guide

This is how we write Haskell at Obsidian Systems, and, more usefully, *why*. It's
the thinking behind [`style.hs`](./README.md). The configs in this repo handle
the mechanical formatting and lint some of the rest, so most of this guide is about
the judgment calls a tool can't make for you and the reasoning behind our
config decisions.

None of this is set in stone. We have a strong preference for abiding by the
conventions that exist or naturally arise in codebases we work within. If
you're adopting this for your own projects, we hope it helps. We're not really interested in bikeshedding about code formatting, we're primarily interested in not having to think about layout at all.

## Consistency

**Consistency is more important than any individual preference we may have.**
Make your code fit its surroundings, within a function, a file, a project. If a
module already does something one way, matching it is worth more than being
"right" according to this guide. A codebase that's uniformly slightly-off is
easier to read than one that's perfect in some places but overall inconsistent.

## Ease of Editing

- **A one-line change shouldn't trigger an N-line cleanup.** We avoid any
  formatting whose upkeep grows with the surrounding code. This is why we avoid
  aligning things into vertical columns (e.g., if we were to line up the `::`
  in a record, the day someone adds a longer field name, every other line has
  to move and the diff obscures the one real change). We prefer to indent by a
  fixed amount instead.
- **Not everyone has your tooling.** Someone will eventually read or edit this code
  without your editor or your formatter. We prefer rules that are simple to
  follow by hand.

## Formatting

[`style.hs`](./README.md) automates almost everything in this section.

- **Two-space indentation.** Never tabs.
- **Fixed indentation over alignment.** Indent a consistent step rather than
  lining up with something on the line above.
- **Leading commas.** When a list, record, or tuple spans multiple lines, every
  element after the first begins with a comma.
- **Aligned braces.** When a list, record, or tuple spans multiple lines, the
  opening brace, every comma, and the closing brace sit in the same column, one
  element per line. It may look unusual at first, but adding or removing an
  element becomes a one-line diff.
- **Leading arrows.** When a type signature breaks across lines, the `::` and
  each `->` lead their line.
- **A broken `forall` aligns its dot like an arrow.** When a `forall` breaks
  across lines, treat the `.` the way you'd treat a `->`.
- **`forall a.` spacing on one line:** a space after the dot, none before.
- **`where` on its own line,** indented two spaces, with its body starting on
  the next line and indented two further.
- **A `do` block's first statement** goes on the line *after* `do`, not beside
  it.
- **Operators are surrounded by whitespace.** A binary operator gets a space on
  each side: `x + y`, `f <$> x`.
- **Operator sections hug their parenthesis.** `(+ 1)`, `(1 +)` — no space
  between the paren and the operator, but keep the space to the argument.
- **A comma is followed by whitespace** — a space, or a line break. `(a, b)`,
  never `(a,b)`.
- **A comma carries whitespace before it only as a leading comma,** i.e. when
  it's the first non-whitespace character on its line. Otherwise it hugs the
  token before it: `(a, b)`, never `(a , b)`.
- **Single-constraint contexts are never parenthesized.** `Show a => a -> a`,
  not `(Show a) => a -> a`.
- **No stray spaces.** Don't leave extra whitespace between tokens beyond what
  these rules call for.
- **One blank line, never two.** Doubled blank lines tend to mean different
  things to different people, which is to say they mean nothing. If you want to
  mark off a large section, a comment header carries more information than
  extra whitespace.
- **Unix line endings.** Never carriage returns.
- **Every file ends with a single newline.**
- **No trailing blank lines** at the end of a file.
- **A space before `where`** in a module's export list.
- **No newline before the `=`** in a definition.

A few habits in the same spirit that the formatter won't do for you:

- **Prefer `$` to trailing parentheses** rather than wrapping the tail end of a line.
- **Prefer `$` to `.`** when either would read the same.
- **Parenthesize uncommon operators** whose precedence isn't obvious: clarity is worth a
  few characters.
- **Don't parenthesize plain function application.**
- **Append with `<>`, not `++`.** They behave identically on lists, but `<>` leaves the
  door open to changing your string or sequence type later without a rewrite.

## Imports

A handful of modules export names that collide with the `Prelude` or with each other, so
we always bring them in `qualified` — or with a small, explicit import list:

| Module                           | Imported as                                   |
| ---                              | ---                                           |
| `Data.ByteString`                | `qualified as BS`                             |
| `Data.ByteString.Lazy`           | `qualified as LBS`                            |
| `Data.Text`                      | `qualified as T`, plus `Data.Text (Text)`     |
| `Data.Map`                       | `qualified as Map`, plus `Data.Map (Map)`     |
| `Data.Map.Monoidal`              | `qualified as MMap`, plus the type            |
| `Data.Set`                       | `qualified as Set`, plus `Data.Set (Set)`     |

The aim is that an unqualified name is always one you can find a home for at a glance.

**Put `qualified` after the module name, not before.** Post-qualified imports line the
module names up in the same column whether or not they're qualified, which keeps the import
block easy to scan and to sort. (This needs the `ImportQualifiedPost` extension, on by
default since GHC 9.6.)

```haskell
-- Worse: qualified sits before the name, so module names don't line up
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- Better: the module name comes first in every import
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
```

## Safety: avoid partial functions

This is the rule we feel most strongly about. A *partial* function, one that
isn't defined for every input it accepts, fails far from the mistake that
caused it, usually with a message that tells you nothing (`Prelude.head: empty
list`, good luck finding *which* `head`). Errors should be identifiable and
locatable.

- **Don't use the well-known partial functions:** `head`, `tail`, `init`,
  `last`, `(!!)`, `Data.Map.!`, and the rest of the [standard list of partial
  functions](https://wiki.haskell.org/List_of_partial_functions). (The ones
  flagged as "only partial on infinite lists" are fine.)
- **`decodeUtf8` is partial** — use `decodeUtf8With lenientDecode` instead.
- **When you genuinely need to assert "this can't happen,"** prefer an
  incomplete pattern match (evaluated strictly) over a silent fallback, so a
  violated assumption fails immediately rather than limping along with wrong
  data. Better still, throw with a source location. Packages like
  [`loch-th`](https://hackage.haskell.org/package/loch-th) or `placeholders`
  make the error point at itself.
- **Don't use `undefined`.** If a branch really is unreachable, use `error`
  with a source location and a comment explaining *why* it can't happen.

## Types, warnings, and deriving

- **Give every top-level definition a type signature.** It's documentation that
  can't go stale, and it gives the type-checker a place to localize errors.
- **Build with `-Wall` and keep it quiet.** If you're working in a file that's
  already noisy, you don't owe it a full cleanup, but don't add new warnings,
  and tidy the ones in code you touch. If a warning really is unreasonable
  here, disable it narrowly with a pragma *and a comment saying why*. Never a
  bare disable.
- **Turn on `-Wmissing-methods`** so a half-finished instance is a compile
  error instead of a runtime surprise.
- **Derive everything GHC will give you** — `Eq`, `Ord`, `Show`, `Functor`,
  `Foldable`, `Traversable`, `Generic`, `Data`, `Typeable`, `NFData` (and
  `Read`, unless there's a strong reason not to). It's free, and it's correct
  by construction.
- **Don't bind a value you don't use just to silence the unused-binding
  warning** by prefixing it with `_`. Simply don't bind it. A short comment is
  fine if the omission is surprising. The one exception is a binding used under
  some CPP paths and not others.

## Extensions we avoid

Each of these removes a way for code to go wrong quietly. They're of a piece
with the safety theme above.

- **No `RecordWildCards`.** A binding site that doesn't name what it binds
  forces the reader to already know the field names, and makes scoping hard to
  trace.
- **No `NamedFieldPuns`.** It shadows existing names with differently-typed
  bindings, which makes type-checking an expression in your head much harder.
- **No `DeriveAnyClass`.** It doesn't guarantee the methods get an
  implementation, which becomes a runtime crash. It also clashes with
  `GeneralizedNewtypeDeriving`, sometimes silently leaving derived instances
  empty.

## Naming

Consistent names are names you can *guess*. Three conventions carry most of the
weight:

- The field of a `newtype MyNewtype`, if it's named, is `unMyNewtype`.
- Fields of a record `MyRecord` are named `myRecord_fieldName`.
- Constructors of a sum type `MySumType` are named `MySumType_ConstructorName`.

When you want lenses for a record, prefer
[`generic-lens`](https://hackage.haskell.org/package/generic-lens) rather than
Template Haskell's `makeLenses`. It builds lenses from the `Generic` instance
you're already deriving (see above): `record ^. #myRecord_fieldName`. There's
no generated code, no Template Haskell, and no need for the
underscore-prefixed field names that `makeLenses` expects.

Avoid primed names (`foo'`). A prime is easy to misread and easy to shadow by
accident. A real name almost always says more.

## Prefer `case` to multiple clauses

When a function pattern-matches, we lean toward a single clause built on `case`
(or `\case`) rather than several top-level clauses. It keeps the function's
name in one place and its branching in one place:

```haskell
-- Worse: the name and the structure are scattered across clauses
showMaybeCtor (Just _) = "Just"
showMaybeCtor Nothing = "Nothing"

-- Better: one definition, matching gathered in one place
showMaybeCtor m = case m of
  Just _ -> "Just"
  Nothing -> "Nothing"

-- Best: no need to name the argument at all
showMaybeCtor = \case
  Just _ -> "Just"
  Nothing -> "Nothing"
```

## Some general design notes

These are habits of mind that keep showing up in code we're happy with.

**Be deliberate about leniency.** "Be liberal in what you accept" (Postel's law) is good
advice less often than people repeat it. When we own *both* ends of an interface, we're
strict on both, the sooner a mismatch surfaces, the cheaper it is to fix. When we don't
own the other end, we weigh the failure modes: if a rejected message is a minor
inconvenience, stay strict. If a *wrongly accepted* message is the bigger danger, be
strict there, too. Avoid lenient interpretations that are ambiguous or non-obvious, and
when you do accept something loosely, log it so nonconforming clients can be found and
corrected rather than quietly relied upon.

**Reach for a more precise type, not a different name.** You'll sometimes hear that a
bare `Bool` is "blind" and the cure is more descriptive constructor names. We
don't find that to be particularly useful way of thinking about it: *every*
value leans on its context for meaning, and renaming constructors doesn't
change that. If a `Bool` genuinely feels error-prone somewhere, ask *why*: the
real fix is usually a more constructive type (say, `Maybe SomeWitness` instead
of `Bool`), which makes the illegal states unrepresentable rather than merely
relabeled.

## Extensions we'd turn on by default

These are defaults we prefer. Enabling them in the cabal file removes a lot of
per-module `LANGUAGE` boilerplate:

`ScopedTypeVariables`, `FlexibleContexts`, `FlexibleInstances`,
`FunctionalDependencies`, `GADTs`, `LambdaCase`, `DeriveDataTypeable`,
`DeriveFoldable`, `DeriveFunctor`, `DeriveGeneric`, `DeriveTraversable`,
`GeneralizedNewtypeDeriving`, `MultiParamTypeClasses`, `RankNTypes`,
`StandaloneDeriving`, `TypeFamilies`, `TypeApplications`,
`AllowAmbiguousTypes`, `OverloadedStrings`, `EmptyDataDeriving`.

One caveat: `RankNTypes` can interfere with GHC's ability to spot redundant or
simplifiable typeclass contexts.
