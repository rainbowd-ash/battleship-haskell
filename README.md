# A battleship game in haskell
A project for my functional programming class

## Dev Plan Overview
* Build data structures for board, players, ships
* Game setup: generate boards, place ships randomly and legally
* Display & Input
* Game loop: player turns, pick location, check results of pick, win condition

## AI
The AI for battleship has a few options. The easiest one will be random squares, until a hit, then picking squares near the hit. I don't think simulating all possible future board states is a viable option because of the size of the board.

## Running (on windows)
Build with >ghc --make .\battleship.hs
run with >.\battleship.exe
