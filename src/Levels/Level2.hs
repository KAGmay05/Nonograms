module Levels.Level2 where

import Board
import Types

level2:: Board
level2 = setCellsFull [
                       (0,6),(0,8),(0,9),
                       (1,8),(1,9),
                       (2,7),
                       (3,9),
                       (4,5),(4,6),
                       (5,0),(5,1),(5,4),(5,5),(5,6),(5,7),
                       (6,0),(6,1),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),
                       (7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),
                       (8,4),(8,7),
                       (9,3),(9,4),(9,6),(9,7)
                       ] 
                       Filled 
                       (emptyBoard 10 10)

boardClues :: ([[Int]],[[Int]])
boardClues = computeBoardClues level2















