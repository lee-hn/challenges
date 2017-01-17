# Haskell Tic-Tac-Toe

## Description

An implementation of a tic-tac-toe game in Haskell, allowing a user to play on a
3x3 board against an unbeatable computer through the console.

## Dependencies

Install [`stack`](https://github.com/commercialhaskell/stack/tree/master/doc).

If you do not already have [GHC](https://www.haskell.org/ghc/) on your
system, you can use `stack` to install it:

`stack setup`

## Installing the game

1. `git clone https://github.com/lee-hn/challenges.git`
2. `cd path/to/challenges/tictactoe`
3. `stack install`

## Running the game

`~/.local/bin/tictactoe-exe`

(If you have changed your local bin path setting in `stack`, replace
`~/.local/bin/` with the output to `stack path --local-bin-path`.)

## Running the test suite

`stack test`
