# Haskell Programming from first principles

Solutions to [Haskell Programming from first principles](http://haskellbook.com/) exercises

The primary branch is `source`. When PRs are merged to the `source` branch,
a CircleCI job builds the site, copies it to `/docs` on the `master` branch and
pushes it up where it's hosted on GitHub Pages at
[https://robertjlooby.github.io/haskellbook/](https://robertjlooby.github.io/haskellbook/).

### Building

1. `stack build` (only need to re-do this if `site.hs` has changed)
1. `stack exec haskellbook -- build`

### Executing problems

1. `stack repl`
1. `:load chapters/chapter-<number>.lhs`

### Running tests

1. `stack test`
