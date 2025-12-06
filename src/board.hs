module Board where

import Types
import Data.List (transpose)


-- Safe index: returns Nothing when out of bounds
safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
    | i < 0 || i >= length xs = Nothing
    | otherwise = Just (xs !! i)

-- Update element at index with function, safely
updateAt :: Int -> (a -> a) -> [a] -> Maybe [a]
updateAt i f xs
    | i < 0 || i >= length xs = Nothing
    | otherwise = case splitAt i xs of
        (pre, y:ys) -> Just (pre ++ (f y) : ys)
        _ -> Nothing

-- Construct an empty board filled with Unknown cells
emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = replicate rows (replicate cols Unknown)

initGame :: [[Int]] -> [[Int]] -> Game
initGame rc cc = Game
    { board = emptyBoard (length rc) (length cc)
    , rowClues = rc
    , colClues = cc
    }

getRowM :: Int -> Board -> Maybe Row
getRowM = safeIndex

getColM :: Int -> Board -> Maybe Row
getColM c board = mapM (safeIndex c) board

toggleCell :: (Int, Int) -> Board -> Maybe Board
toggleCell (r,c) b = do
    row <- safeIndex r b
    newRow <- updateAt c toggle row
    updateAt r (const newRow) b
  where
    toggle Filled  = Empty
    toggle Empty   = Filled
    toggle Unknown = Filled

setCell :: (Int, Int) -> Cell -> Board -> Maybe Board
setCell (r,c) val b = do
    row <- safeIndex r b
    newRow <- updateAt c (const val) row
    updateAt r (const newRow) b

setCellsFull :: [(Int,Int)] -> Cell -> Board -> Board
setCellsFull coords target board =
    [ [ if (r,c) `elem` coords
          then target
          else opposite target
      | c <- [0 .. cols board - 1] ]
    | r <- [0 .. rows board - 1] ]


opposite :: Cell -> Cell
opposite Filled  = Empty
opposite Empty   = Filled
opposite Unknown = Unknown 

rows :: Board -> Int
rows = length

cols :: Board -> Int
cols b = case b of
    [] -> 0
    (r:_) -> length r

boardSize :: Board -> (Int, Int)
boardSize b = (rows b, cols b)

-- Compute clues for a single row (treat Unknown as Empty)
computeClues :: Row -> [Int]
computeClues row = go row [] 0
  where
    go [] acc 0 = reverse acc
    go [] acc n = reverse (n : acc)
    go (Filled:xs) acc n = go xs acc (n+1)
    go (Empty:xs) acc 0 = go xs acc 0
    go (Empty:xs) acc n = go xs (n:acc) 0
    go (Unknown:xs) acc 0 = go xs acc 0
    go (Unknown:xs) acc n = go xs (n:acc) 0


computeBoardClues :: Board -> ([[Int]], [[Int]])
computeBoardClues board =
    let rowClues = map computeClues board
        colClues = map computeClues (transpose board)
    in (rowClues, colClues)


-- Pretty-print board to string for terminal/debugging
showBoard :: Board -> String
showBoard = unlines . map (map cellChar)
  where
    cellChar Filled  = '#'
    cellChar Empty   = '.'
    cellChar Unknown = '?'

-- Safe access to a single cell
getCellM :: Int -> Int -> Board -> Maybe Cell
getCellM r c b = do
        row <- safeIndex r b
        safeIndex c row

