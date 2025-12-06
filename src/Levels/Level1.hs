module Levels.Level1 where

import Board
import Types

level1:: Board
level1 = setCellsFull [
                       (0,0),(0,4),
                       (1,0),(1,1),(1,2),(1,3),(1,4),
                       (2,0),(2,2),(2,4),(2,8),
                       (3,0),(3,1),(3,2),(3,3),(3,4),(3,9),
                       (4,2),(4,9),
                       (5,1),(5,2),(5,3),(5,9),
                       (6,0),(6,1),(6,2),(6,3),(6,4),(6,8),(6,9),
                       (7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,7),(7,8),
                       (8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),
                       (9,1),(9,2),(9,3),(9,4),(9,5),(9,6)
                       ] 
                       Filled 
                       (emptyBoard 10 10)

boardClues :: ([[Int]],[[Int]])
boardClues = computeBoardClues level1
















-- rowCluess:: [[Int]]
-- rowCluess = [[1,1],[5],[1,1,1,1],[5,1],[1,1],[3,1],[5,2],[6,2],[8],[6]]

-- colCluess:: [[Int]]
-- colCluess= [[4,3],[1,1,5],[9],[1,1,5],[4,4],[3],[2],[2],[1,2],[4]]

-- level1:: Game
-- level1 = initGame rowCluess colCluess

-- solveLevel1:: Maybe Board
-- solveLevel1 = solveGame level1