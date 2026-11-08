# style.hs

### Obsidian-style Haskell

shared formatter and linter configs · `nix run` anywhere · consistent Haskell across projects

![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white) [![Built with Nix](https://img.shields.io/static/v1?logo=nixos&logoColor=white&label=&message=Built%20with%20Nix&color=41439a)](https://nixos.org) [![Obsidian](https://img.shields.io/badge/Obsidian-Systems-white)](https://obsidian.systems) [![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](./LICENSE)

```console
$ nix run github:obsidiansystems/style.hs -- --mode inplace src/   # format your Haskell, Obsidian-style
$ nix run github:obsidiansystems/style.hs -- --mode check   src/   # or just check it, e.g. in CI

$ nix run github:obsidiansystems/style.hs#stylish-haskell -- -i -r src/   # the low-diff alternative
$ nix run github:obsidiansystems/style.hs#hlint -- src/                   # and lint it, Obsidian-style
```

A [Nix](https://nixos.org/) flake that wraps three tools, two formatters and a linter:
[`fourmolu`](https://github.com/fourmolu/fourmolu),
[`stylish-haskell`](https://github.com/haskell/stylish-haskell) and
[`hlint`](https://github.com/ndmitchell/hlint), with common configs:
[`fourmolu.yaml`](./fourmolu.yaml), [`stylish-haskell.yaml`](./stylish-haskell.yaml) and
[`hlint.yaml`](./hlint.yaml). Point any of your Haskell projects at this flake and they
all format and lint the same way, without each repo having to vendor and maintain its own
copy of the config. All three configs implement our [Haskell Style Guide](./STYLE.md).

The two formatters are alternatives: pick one per project. hlint is neither, and runs
alongside whichever you pick. A formatter decides layout, hlint decides what you write, so
hlint carries the half of the guide no formatter can check, above all
"[avoid partial functions](./STYLE.md#safety-avoid-partial-functions)".

## Why style.hs?

- **One config, every repo.** Reference the flake and your projects format identically.
- **Drop in however you like.** Run it directly with `nix run`, add it to a dev shell so
  the plain command just works, vendor it as a git submodule, or run the config file
  itself as an executable.

## Which formatter?

They're alternatives, not a pipeline. Pick one per project and stick with it: running
both over the same file means each will undo part of the other's work.

| | [fourmolu](./fourmolu.yaml) | [stylish-haskell](./stylish-haskell.yaml) |
| --- | --- | --- |
| What it rewrites | Everything, expression layout included | Imports, `LANGUAGE` pragmas, the module header, `data`/`newtype` declarations, whitespace |
| Function bodies | Reformatted | Left exactly as written |
| Cost to adopt | A full reformat of the repo | A small, mostly-imports diff |
| Style coverage | All of [STYLE.md](./STYLE.md)'s mechanical rules | The subset in [What stylish-haskell covers](#what-stylish-haskell-covers) |
| Flake output | `fourmolu`, and the `default` | `stylish-haskell` |

fourmolu is the default and the more complete of the two. Reach for stylish-haskell when
a codebase can't absorb an all-at-once reformat, or when you want the formatter to stay
out of the way of hand-tuned expression layout.

## Usage

### Run it directly

```sh
# Format in place, using the bundled Obsidian Systems config.
nix run github:obsidiansystems/style.hs -- --mode inplace src/

# Check formatting without writing (e.g. in CI).
nix run github:obsidiansystems/style.hs -- --mode check src/
```

Everything after `--` is forwarded straight to `fourmolu`, so any flag fourmolu
accepts works here too.

stylish-haskell lives at the `#stylish-haskell` attribute, and takes its own flags
(`-i` to write in place, `-r` to recurse into a directory):

```sh
nix run github:obsidiansystems/style.hs#stylish-haskell -- -i -r src/
```

It has no equivalent of fourmolu's `--mode check`, so to check without writing, format
and let git tell you whether anything moved:

```sh
nix run github:obsidiansystems/style.hs#stylish-haskell -- -i -r src/
git diff --exit-code
```

hlint lives at the `#hlint` attribute, and takes its own flags:

```sh
nix run github:obsidiansystems/style.hs#hlint -- src/
```

It exits non-zero the moment it finds anything, so it needs no separate check mode for
CI. `--refactor` applies the fixes in place instead of printing them, and
`--no-exit-code` reports without failing the build. `--with-group=generalise` adds the
optional rules of [The generalise group](#the-generalise-group).

### Add it to a project

Reference the flake as an input and use the `fourmolu` package as your formatter,
for example in a dev shell:

```nix
{
  inputs.style.url = "github:obsidiansystems/style.hs";

  outputs = { self, nixpkgs, style, ... }:
    let system = "x86_64-linux";
    in {
      devShells.${system}.default =
        nixpkgs.legacyPackages.${system}.mkShell {
          packages = [ style.packages.${system}.fourmolu ];
        };
    };
}
```

Inside that shell, plain `fourmolu` already uses the Obsidian Systems style, with
no `--config` needed. Swap in `style.packages.${system}.stylish-haskell` to get a
`stylish-haskell` that behaves the same way, and add `style.packages.${system}.hlint`
alongside either for a matching `hlint`.

### Vendor it as a git submodule

Add this repo as a submodule and symlink your project's config to the vendored one.
All three tools discover their config from the project root automatically, so editors,
formatters, and CI all pick up the style with no flags:

```sh
git submodule add https://github.com/obsidiansystems/style.hs style.hs

ln -s style.hs/fourmolu.yaml fourmolu.yaml                    # fourmolu
ln -s style.hs/stylish-haskell.yaml .stylish-haskell.yaml     # stylish-haskell
ln -s style.hs/hlint.yaml .hlint.yaml                         # hlint
```

Note the leading dots. stylish-haskell searches for `.stylish-haskell.yaml` in the current
directory and its ancestors, then `$XDG_CONFIG_HOME/stylish-haskell/config.yaml`, then
`$HOME/.stylish-haskell.yaml`. hlint searches for `.hlint.yaml` in the current directory
and its ancestors, then in your home directory.

Pull in later changes to the shared style by updating the submodule:

```sh
git submodule update --remote style.hs
```

### Use the config file as an executable

[`fourmolu.yaml`](./fourmolu.yaml), [`stylish-haskell.yaml`](./stylish-haskell.yaml) and
[`hlint.yaml`](./hlint.yaml) are each both the config *and* a runnable
[Nix shebang](https://nix.dev/manual/nix/stable/command-ref/new-cli/nix.html?highlight=shebang#shebang-interpreter)
script. Copy one into a project and run it directly to process files using itself as
the config:

```sh
./fourmolu.yaml --mode inplace src/
./stylish-haskell.yaml -i -r src/
./hlint.yaml src/
```

This needs `nix` on `PATH` with flakes enabled.

## Choosing a config

Each formatter wrapper resolves which config to use as follows:

- **No config flag**: the bundled `fourmolu.yaml` / `stylish-haskell.yaml` (the default).
- **`--config <path>`** / **`--config=<path>`**: your own config file. Repeated config
  flags collapse to the last one, since both tools reject duplicates themselves.
- **`--config -`**: read the config from stdin.

The stylish-haskell wrapper additionally accepts that tool's short form, `-c <path>` or
`-c<path>`, including at the end of a bundled cluster such as `-ic <path>`.

hlint spells the flag `--hint <path>`, `--hint=<path>`, `-h <path>` or `-h<path>`, and
its configs *layer* rather than replace. hlint always loads its own built-in hints first,
then the bundled `hlint.yaml`, then anything you pass, with later files winning. So
`--hint mine.yaml` adds to the Obsidian rules instead of discarding them, and
`- ignore: {name: ...}` is how you switch one of them off. `--hint -` reads the config
from stdin, which plain hlint does not accept.

Two consequences are worth knowing. Passing any `--hint` stops hlint hunting for a
project's own `.hlint.yaml`, so pass yours explicitly if you have one. And since the
wrapper injects the bundled config itself, `./hlint.yaml` run as an executable would
otherwise load it twice and report every custom rule twice over; the wrapper notices a
byte-identical copy and skips its own.

## What stylish-haskell covers

stylish-haskell is a fixed pipeline of nine steps, and that set is the hard limit on what
it can do: `imports`, `module_header`, `records`, `language_pragmas`, `simple_align`,
`squash`, `tabs`, `trailing_whitespace`, `unicode_syntax`. Only the first three rewrite
declarations at all. Nothing in the set looks at an expression, a function body, a type
signature, a `where` clause, a `do` block, or the blank lines between declarations.

So these [STYLE.md](./STYLE.md) rules are enforced:

| Rule | How |
| --- | --- |
| Post-qualified imports, `import Data.Text qualified as T` | `imports.post_qualify` |
| Imports sorted within each group | `imports` |
| Leading commas and aligned parens in import and export lists | `imports`, `module_header` |
| Leading commas and aligned braces in `data`/`newtype` declarations | `records` |
| Sorted `deriving` lists | `records.sort_deriving` |
| One `LANGUAGE` pragma per line, sorted, redundant ones dropped | `language_pragmas` |
| No column alignment anywhere the tool could introduce it | `simple_align` (all `never`), `imports.align: none`, `language_pragmas.align: false` |
| Two spaces, never tabs | `tabs` |
| No trailing whitespace | `trailing_whitespace` |
| Unix line endings | `newline: lf` |
| A space before `where` in the export list | `module_header.break_where` |

And these are out of reach. Use fourmolu, or your own discipline, for them:

- Leading `::` and `->` in a broken type signature, and a broken `forall`'s dot.
- Unparenthesized single-constraint contexts: `Show a => a -> a`.
- `where` on its own line, and the two-space step for its body.
- A `do` block's first statement on the line after `do`.
- Operator spacing (`x + y`) and section hugging (`(+ 1)`).
- Leading commas in *expression* lists, records and tuples. Only declarations and
  import/export lists get them here.
- `let`/`in` layout, haddock style, and the space inside record braces.
- "One blank line, never two."
- No newline before the `=` of a function definition.
- Import *grouping*. The Obsidian External/Common/Frontend/Backend/Local ordering isn't
  expressible: stylish-haskell emits groups in rule order with unmatched modules last,
  and has no notion of which modules are local to your package. The config therefore
  leaves your blank-line groups alone and only sorts within them.

## Where the two formatters differ

On imports the two agree exactly: sorting, deduplication, post-qualification and list
layout all produce byte-identical output. The same holds for any declaration or export
list already written in the multi-line form, which is the form both tools converge on.

Three differences remain. None of them breaks a STYLE.md rule, but they do mean a file
formatted by one tool is not always stable under the other:

- **`newtype` declarations break before the `=`.** fourmolu keeps them on one line.
  Avoiding this needs `records.equals: same_line`, which then aligns a multi-constructor
  type's `|` under its `=`, and column alignment is the one thing STYLE.md is emphatic
  about avoiding. `break_single_constructors` covers only `data`, so no combination
  avoids both.
- **A sum type written on one line gets expanded**, one constructor per line. fourmolu is
  `respectful` and leaves your line breaks as they are. Enums and nullary constructors
  (`data Colour = C_Red | C_Green`, `data Proxy = Proxy`) stay inline under both.
- **No blank line after the `LANGUAGE` pragma block.** fourmolu inserts one before
  `module`.

One hazard worth knowing: `post_qualify` rewrites `import qualified Data.Text as T` into
the post-qualified form *unconditionally*, even in a module where `ImportQualifiedPost`
isn't enabled, leaving code that needs the extension to parse. fourmolu only rewrites
when the extension is already in scope. The extension is on by default from GHC 9.6; on
anything older, put it in your cabal file's `default-extensions`.

## What hlint enforces

Whichever formatter you pick covers STYLE.md's layout rules. These are the rules hlint
adds on top, all of which a formatter is blind to:

| Rule | How |
| --- | --- |
| [Avoid partial functions](./STYLE.md#safety-avoid-partial-functions): `head`, `last`, `tail`, `init`, `(!!)`, `fromJust`, `foldr1`, `foldl1`, `maximum`, `minimum`, `NonEmpty.fromList` | `functions` restriction |
| `Data.Map.!` is partial, and so is `read` | `functions` restriction |
| `decodeUtf8` is partial: use `decodeUtf8With lenientDecode` | `functions` restriction |
| Don't use `undefined` | `functions` restriction |
| No `RecordWildCards`, `NamedFieldPuns` or `DeriveAnyClass` | `extensions` restriction |
| Post-qualified imports, `import Data.Text qualified as T` | `modules`, `qualifiedStyle: post` |
| The import alias table: `T`, `Map`, `Set`, `BS`, `LBS` | `modules`, `as:` |
| Append with `<>`, not `++` | a custom `warn` rule |
| Prefer `\case` to multiple clauses | hlint's built-in `Use lambda-case` |

The config spells the partial-function list out rather than enabling hlint's built-in
`partial` group. A `functions` restriction matches an identifier wherever it appears, so
it catches `map head` and `(!! 3)` as well as `head xs`; the group's rewrite rules only
fire on applied uses, and let a bare `map head` through.

It also silences the built-in hints that contradict the guide. `Use camelCase` fires on
every `myRecord_fieldName` and `MySumType_Ctor`, which is precisely the naming STYLE.md
mandates, and hints like `Use head` (`x !! 0` → `head x`) or `Use ++`
(`concat [a, b]` → `a ++ b`) would push you toward something the config just banned.

Out of reach, and left to GHC or to you: `-Wall` cleanliness, a type signature on every
top-level definition, `-Wmissing-methods`, deriving everything GHC will give you, the
`unMyNewtype` / `myRecord_field` / `MySumType_Ctor` naming beyond the camelCase
exemption, and avoiding primed names.

### The generalise group

Everything above is on by default. One set is not: preferring the more general of two
functions that already do the same thing, so loosening a type later costs no call site.
Turn it on per run:

```sh
nix run github:obsidiansystems/style.hs#hlint -- --with-group=generalise src/
```

| Prefer | Over |
| --- | --- |
| `<>` | `mappend` |
| `pure`, `liftA2`, `<*>` | `return`, `liftM2`, `ap` |
| `asum`, `<\|>`, `empty` | `msum`, `mplus`, `mzero` |
| `traverse`, `traverse_`, `for`, `for_` | `mapM`, `mapM_`, `forM`, `forM_` |
| `fold`, `foldMap` | `mconcat`, `concat`, `concatMap` |
| `fmap`, `toList` | `Data.Map.map`, `Data.Map.elems` |

It is off by default because a codebase adopts this on its own schedule. Every rewrite
preserves behaviour, but a large project turns up hundreds of call sites at once, and
that is a diff you want to choose the timing of.

`generalise` is hlint's own group name, so these rules join its built-in members rather
than replace them. Enabling it therefore also brings `map` to `fmap` and `sequence [a]`
to `pure <$> a`, which is the point: the whole idea sits behind one switch. Two
consequences follow. `++` is checked by default *and* by the built-in group, so a `++`
reports twice with the group on. And `liftM` is absent from the group, because hlint
suggests `fmap` for it either way.

The Map rules are qualified across `Data.Map`, `Data.Map.Strict` and
`Data.Map.Monoidal`, since a bare `map` or `elems` would match far more than a Map.

## What's in the repo

| File                                             | Purpose                                                                     |
| ---                                              | ---                                                                         |
| [`STYLE.md`](./STYLE.md)                         | The Obsidian Haskell Style Guide: the reasoning behind the rules.           |
| [`fourmolu.yaml`](./fourmolu.yaml)               | The shared style config for fourmolu: also a runnable Nix shebang script.   |
| [`fourmolu.nix`](./fourmolu.nix)                 | Builds the `fourmolu` wrapper that bundles the config and handles `--config`. |
| [`stylish-haskell.yaml`](./stylish-haskell.yaml) | The same style for stylish-haskell: also a runnable Nix shebang script.     |
| [`stylish-haskell.nix`](./stylish-haskell.nix)   | Builds the `stylish-haskell` wrapper, likewise.                             |
| [`hlint.yaml`](./hlint.yaml)                     | The same style for hlint: also a runnable Nix shebang script.               |
| [`hlint.nix`](./hlint.nix)                       | Builds the `hlint` wrapper, likewise.                                       |
| [`flake.nix`](./flake.nix)                       | Exposes the `fourmolu` (and `default`), `stylish-haskell` and `hlint` packages for every supported system. |
| [`inputs.nix`](./inputs.nix)                     | `flake-compat` shim so non-flake Nix can consume the inputs.                |

## About Obsidian Systems

style.hs is built and maintained by **[Obsidian Systems](https://obsidian.systems)**. We
provide frontier engineering for high-assurance systems, and we're long-time stewards of
open-source Nix, Haskell, and daml tooling, including [Obelisk](https://github.com/obsidiansystems/obelisk),
[Reflex](https://reflex-frp.org/), [nix-daml-sdk](https://github.com/obsidiansystems/nix-daml-sdk), and [nix-thunk](https://github.com/obsidiansystems/nix-thunk).

If you're building with Haskell or Nix and want a partner to help design, build, or ship
it, we'd love to hear from you.

- Website — <https://obsidian.systems>
- Blog — <https://blog.obsidian.systems>
- GitHub — <https://github.com/obsidiansystems>

## License

style.hs is released under the [BSD-3-Clause License](./LICENSE), © 2026 Obsidian Systems
LLC. fourmolu and the other tools it wraps are distributed under their own upstream
licenses.
