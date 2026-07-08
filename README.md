<!-- [TODO]: replace this document with an approprate README for your project -->

# haskell-agentic-stack-template

## How to use this template

To use this template:

```
gh repo create <project_name> --template "rybla/haskell-agentic-stack-template" --public --clone
```

Replace all appearances of "haskell-agentic-stack-template" with your project name ("<project_name>").

Replace all template variables in [package.yaml](./package.yaml) with the appropriate values.

## Development dependencies

- [Haskell Stack](https://docs.haskellstack.org/en/stable/) (can be installed via [GHCUp](https://www.haskell.org/ghcup/))
- [Just](https://just.systems)

## Design

This template is for a Haskell SWE agent harness that leverages Haskell's type system and surrounding tooling while also providing some informal guidance.
Features are explained in the following subsections.

### Development commands

The [`Justfile`](./Justfile) defines a collection of common development recipes.
In particular, the recipe `just check` performs an extensive code checking workflow described in the next subsection.

### Code checking

The code checking workflow has several components, mainly:

- formatting
- linting
- building
- testing

The [code-check rule](./.agents/rules/code-check.md) requires an agent to ensure this workflow passes before submitting changes.

**Linting**.
One part of the code checking workflow is a linting workflow implemented by [`lint.sh`](./lint.sh) and [`.hlint.yaml`](./.hlint.yaml) that leverages [HLint](https://hackage.haskell.org/package/hlint) for linting source code and automatically refactoring it according to custom rewrite rules (written in `./hlint.yaml`).
The linting workflow has two phases: an iterative refactoring phase where refactorable linting rules are applied to idempotence, and a final diagnosis phase where all remaining linting diagnostics are collected in a final report.

These linting rules of [`.hlint.yaml`](./.hlint.yaml) are quite extensive, and ensure that source code follows certain conventions as well as avoiding dangerous functions such as `unsafePerformIO`.
There are also some custom linting rules for development use, such as the following linting rule that allows a human to use the "todo" function to indicate exactly where the agent should fill in an implementation according to a description.

```yaml
- warning:
    {
      name: "todo",
      lhs: "todo s",
      rhs: "replace with actual implementation"
    }
```

_(Note that the refactoring phase of the linting workflow does not use this rule for refactoring, which avoid divergence.)_

For some partial functions, such as `head`, a rewrite rule can assist the agent in handling the partiality by automatically refactoring `head` to use an exhaustive case analysis, where the empty list case must be further handled (which is encoded by an invocation of  `todo` which will notify the agent on the final linting pass)

```yaml
- warning:
    {
      lhs: head l,
      rhs: 'case l of x:_ -> x; [] -> todo "handle empty case of list"',
      name: Avoid partial function,
    }
```

### Hoogle

[Hoogle](https://hoogle.haskell.org) is a great tool for exploring dependencies with advanced queries.
The [`hoogle.sh`](./hoogle.sh) script and associated `just hoogle` recipe provides a simple interface to querying Hoogle and ensuring you're always accessing up-to-date information.
The [hoogle rule](./.agents/rules/hoogle.md) nudges the agent to use Hoogle when thinking about dependencies.

### Skills and Rules

The [`.agents/skills`](./.agents/skills) directory is populated with a collection of skills directly relevant to Haskell development with this project's setup. In particular, the [haskell-skill](https://github.com/rybla/haskell-skill/blob/main/SKILL.md) introduces best practices for using core Haskell language features and common libraries such as

- abstract data types
- newtypes
- record types
- type classes
- deriving type class instances
- monadic actions
- monad transformers
- lenses

The [`.agents/rules`](./.agents/rules) directory is populated with a collection of simple rules for how and when to use certain tools (such as `just check` and `just hoogle`).


