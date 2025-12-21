module Levels.Level3 where

import Board
import Types

level3:: Board
level3 = setCellsFull [
                       (0,2),(0,3),(0,4),(0,5),(0,6),(0,7),
                       (1,1),(1,2),(1,7),(1,8),
                       (2,1),(2,2),(2,7),(2,8),
                       (3,7),(3,8),
                       (4,6),(4,7),(4,8),
                       (5,4),(5,5),(5,6),(5,7),
                       (6,4),(6,5),
                       (8,4),(8,5),
                       (9,4),(9,5)
                       ] 
                       Filled 
                       (emptyBoard 10 10)

boardClues :: ([[Int]],[[Int]])
boardClues = computeBoardClues level3






