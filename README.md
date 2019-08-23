# Connect3

## Introduction

The game of "dynamic connect-4" is played on a 7x7 grid as follows: Starting from the initial position illustrated below, players take turns moving one piece of their colour by one square, either horizontally or vertically. White plays first. Pieces can only move to unoccupied squares. The winner is the first player to line up four of its pieces either horizontally, vertically, or diagonally. 


## Game Agent

Implement a game-playing agent to play the game of dynamic connect-4. Your agent must be capable of playing either white or black. The time limit for your agent to output its move is 20 seconds. It is strongly suggested that you display the game state using the text representation of a matrix of comma-separated characters, with O denoting a white piece, X denoting a black piece, and suitably formatted whitespace denoting an empty square. For example, the starting board configuration above would be represented as follows:

        1 2 3 4 5 6 7
    1  , , , , , ,X
    2 X, , , , , ,O
    3 O, , , , , ,X
    4 X, , , , , ,O
    5 O, , , , , ,X
    6 X, , , , , ,O
    7 O, , , , , ,
Each square in the grid can be labeled by its <x,y> position, with <1,1> corresponding to the top-left square. Moves are entered by specifying the position of the piece to move in the form <x,y> followed by one of the cardinal directions N, S, E, W.

For example, to move the black piece at the top right of the board one square to the left, the command would be 71W. These commands will be exchanged, either with a human opponent or an AI competitor, through the game server for which a sample client is provided in C. Note that each string sent to the game server must terminate with a carriage return ("\n"); this is done to facilitate testing by hand, e..g., using telnet <server host address> 12345. For the purposes of human-readability, your agent should echo to screen each of the moves being played.
