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
1. `:load chapters/Chapter<number>.lhs`

### Running tests

1. `stack test`

### Run Chapter 11 Vigenere Cipher

1. `stack exec vigenere-cipher`

### Playing Chapter 13 hangman game

1. `stack exec hangman`

### Running Chapter 13 palindrome checker

1. `stack exec palindrome`

### Running Chapter 13 person validator

1. `stack exec person`

### Running Chapter 14 morse code

1. `stack exec morse`

### Running Chapter 26 hit counter

1. `stack exec hit-counter -- <prefix>`
