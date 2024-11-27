# A battleship game in haskell
A project for my functional programming class

## Dev Plan Overview
* Build data structures for board, players, ships
* Game setup: generate boards, place ships randomly and legally
* Display & Input
* Game loop: player turns, pick location, check results of pick, win condition

### Game Board
Board made of datatype Square
A square is either water, a miss, a hit, or an unhit ship

### Program Flow
#### Setup:
For each player:
* create empty board
* randomly place ships

#### Gameplay
* Print current player's view of opponent board (opponents board but replace unhit ships with water)
* Print current player's board
* Prompt for input

## AI
The AI for battleship has a few options. The easiest one will be random squares, until a hit, then picking squares near the hit. I don't think simulating all possible future board states is a viable option because of the size of the board.

## Running
Build with > cabal build
run with > cabal run
