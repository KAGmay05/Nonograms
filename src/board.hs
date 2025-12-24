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

isSolved :: Game -> Bool
isSolved g = all rowMatches (zip (rowClues g) (board g))
          && all colMatches (zip (colClues g) (transpose (board g)))

-- Comprueba si un tablero cumple todas las pistas dadas (filas y columnas).
boardSatisfies :: [[Int]] -> [[Int]] -> Board -> Bool
boardSatisfies rc cc b = all rowMatches (zip rc b)
                      && all colMatches (zip cc (transpose b))

rowMatches :: ([Int], Row) -> Bool
rowMatches (clues, row) = computeClues row == clues

colMatches :: ([Int], Row) -> Bool
colMatches (clues, col) = computeClues col == clues


-- ----------------------------
-- VALIDACIÓN PARCIAL DE PISTAS
-- ----------------------------
-- Detecta si una fila viola las pistas (existe o no solución parcial) considerando Unknown como flexible.
rowContradicts :: [Int] -> Row -> Bool
rowContradicts clues row = not (rowFeasible clues row)

-- ¿Existe alguna colocación de las pistas compatible con los Filled/Empty actuales?
rowFeasible :: [Int] -> Row -> Bool
rowFeasible [] r = not (any (== Filled) r)  -- sin pistas restantes: no deben quedar llenos
rowFeasible (k:ks) r = any tryStart [0 .. length r - k]
  where
    tryStart i =
        let (prefix, rest) = splitAt i r
            (block, suffix) = splitAt k rest
        in  -- no puede haber llenos antes del inicio del bloque
            not (any (== Filled) prefix)
            -- el bloque no puede pisar Empty
            && all (/= Empty) block
            -- si quedan más pistas, debe haber un separador (no Filled)
            && (null ks || case suffix of
                              []     -> False
                              (c:cs) -> c /= Filled)
            -- continuar con el resto (saltando separador si aplica)
            && rowFeasible ks (if null ks then suffix else drop 1 suffix)

-- Verifica filas y columnas; True si algún lado contradice.
boardContradicts :: [[Int]] -> [[Int]] -> Board -> Bool
boardContradicts rowC colC b =
    let rowsBad = any (uncurry rowContradicts) (zip rowC b)
        colsBad = any (uncurry rowContradicts) (zip colC (transpose b))
    in rowsBad || colsBad


-- ----------------------------
-- AUTOCOMPLETADO BÁSICO
-- ----------------------------
-- Reglas simples de autocompletado:
--  1) Si una fila no tiene pistas ( [] ), todas las celdas desconocidas se marcan como Empty.
--  2) Si la suma de pistas ya coincide con el número de Filled colocados, el resto Unknown se marcan Empty.
--  3) Si computeClues row == clues (las pistas ya están satisfechas), el resto Unknown se marcan Empty.
-- En presencia de contradicciones, no se realiza ninguna acción.

countFilled :: Row -> Int
countFilled = length . filter (== Filled)

replaceUnknownWithEmpty :: Row -> Row
replaceUnknownWithEmpty = map (\c -> case c of
    Unknown -> Empty
    x       -> x)

autoCompleteRow :: [Int] -> Row -> Row
autoCompleteRow clues row
    | rowContradicts clues row = row
    | null clues               = replaceUnknownWithEmpty row
    | sum clues == countFilled row = replaceUnknownWithEmpty row
    | computeClues row == clues = replaceUnknownWithEmpty row
    | otherwise                = row

autoCompleteStep :: [[Int]] -> [[Int]] -> Board -> Board
autoCompleteStep rc cc b =
    let b1  = zipWith autoCompleteRow rc b
        b2T = zipWith autoCompleteRow cc (transpose b1)
    in transpose b2T

-- Aplica autocompletado iterativamente hasta alcanzar un punto fijo.
autoCompleteBoard :: [[Int]] -> [[Int]] -> Board -> Board
autoCompleteBoard rc cc = go
  where
    go cur =
        let next = autoCompleteStep rc cc cur
        in if next == cur then cur else go next

-- Devuelve las posiciones que el autocompletado marcaría como Empty partiendo de Unknown.
-- Es decir, celdas que pasarían de Unknown -> Empty bajo las reglas actuales.
autoEmptyPositions :: [[Int]] -> [[Int]] -> Board -> [(Int,Int)]
autoEmptyPositions rc cc b =
    let bAuto   = autoCompleteBoard rc cc b
        (rs,cs) = boardSize b
    in [ (r,c)
       | r <- [0 .. rs - 1]
       , c <- [0 .. cs - 1]
       , getCellM r c b == Just Unknown
       , getCellM r c bAuto == Just Empty
       ]

-- ----------------------------
-- SOLVER AUTOMÁTICO (BACKTRACKING)
-- ----------------------------

-- Encuentra la primera celda Unknown
findUnknown :: Board -> Maybe (Int, Int)
findUnknown b =
    let (rs, cs) = boardSize b
    in case [ (r,c)
            | r <- [0 .. rs - 1]
            , c <- [0 .. cs - 1]
            , getCellM r c b == Just Unknown
            ] of
        []    -> Nothing
        (x:_) -> Just x


-- Solver principal: devuelve TODAS las soluciones posibles
solveBoard :: [[Int]] -> [[Int]] -> Board -> [Board]
solveBoard rc cc board =
    let board' = autoCompleteBoard rc cc board
    in
    -- poda temprana
    if boardContradicts rc cc board'
        then []
        else
            case findUnknown board' of
                -- No quedan incógnitas
                Nothing ->
                    if boardSatisfies rc cc board'
                        then [board']
                        else []
                -- Probar Filled y Empty
                Just (r,c) ->
                    let try val =
                            case setCell (r,c) val board' of
                                Nothing -> []
                                Just b' -> solveBoard rc cc b'
                    in try Filled ++ try Empty

