---
trigger: always_on
glob: *.hs
description: Use Hoogle to search for information about functions, types, modules, etc.
---

Hoogle is a code search tool for Haskell.

The Hoogle tool is a great way to learn about functions, types, modules, and other definitions among the project dependencies.

Use hoogle via `just hoogle <query>`.

By default the query string is used to fuzzy search function names and type names. Examples

- `just hoogle "runStateT"`
- `just hoogle "Lens"`

*Type queries*. You can use "::" to indicate that the query string after the "::" should be interpreted as a *type query*, which will be used to search for definitions that have that type. Example

- `just hoogle ":: (a -> b) -> a -> b"`
- `just hoogle "run :: IdentityT m a -> m a"`

*Package constrains*. You can use "+<package_name>" to constrain your search to only look for definitions in named package.

- `just hoogle "+prettyprinter pretty"`
- `just hoogle "+tasty Test`

*Detailed info*. You can prefix your search query with "--info" to get more detailed info about the first result found that matches the query string after "--info".

- `just hoogle "--info runStateT"`
- `just hoogle "--info ^."`
