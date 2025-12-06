{-# LANGUAGE OverloadedStrings #-}
module Game (gamePage) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (forM_, forM, void)
import Board
import Types

-- Tablero 10x10 con pistas simples
rowCluesEx :: [[Int]]
rowCluesEx = replicate 10 [1,1]

colCluesEx :: [[Int]]
colCluesEx = replicate 10 [1,1]

gamePage :: Window -> [[Int]] -> [[Int]] -> UI (Element, Element)
gamePage window rowClues colClues = do
   
    -- ----------------------------
    -- FONDO PASTEL EN DEGRADADO ANIMADO
    -- ----------------------------
    gameRoot <- UI.div # set UI.style
        [ ("margin","0")
        , ("height","100vh")
        , ("width","100vw")
        , ("display","flex")
        , ("flex-direction","row")
        , ("justify-content","center")
        , ("align-items","center")
        , ("position","relative")
        , ("font-family","Segoe UI, Arial, sans-serif")
        , ("background","linear-gradient(135deg, #87CEFA 0%, #87CEFA 30%, #BA55D3 50%, #FF69B4 70%, #FF69B4 100%)")
        , ("background-size","120% 120%")
        , ("animation","pastelMove 25s ease infinite")
        , ("overflow","hidden")
        , ("gap", "30px")
        ]

    void $ UI.addStyleSheet window $
        "data:text/css,@keyframes pastelMove { \
        \0% { background-position: 0% 0%; } \
        \50% { background-position: 100% 100%; } \
        \100% { background-position: 0% 0%; } \
        \}"

    -- ----------------------------
    -- INICIALIZAR JUEGO Y TABLERO
    -- ----------------------------
    let game = initGame rowClues colClues
    boardRef <- liftIO $ newIORef (board game)
    let (rCount, cCount) = boardSize (board game)
    cellButtonsRef <- liftIO $ newIORef ([] :: [[Element]])

    -- Contenedor general en grid 2x2
    container <- UI.div # set UI.style
        [ ("display","grid")
        , ("grid-template-columns","140px 500px")
        , ("grid-template-rows","140px 500px")
        , ("gap","0px")
        , ("justify-items","center")
        , ("align-items","center")
        ]

    titleContainer <- UI.h1 #+ 
        [UI.span #+ [string "NONOGRAMAS"] # set UI.style
            [ ("color","transparent")
            , ("-webkit-text-stroke","2.5px white")
            , ("text-stroke","2.5px white")
            , ("font-size","70px")
            , ("font-weight","900")
            , ("letter-spacing","2px")
            , ("text-transform","uppercase")
            ]
        ]
        
    -- ----------------------------
    -- Pistas de columnas (arriba derecha)
    -- ----------------------------
    colDiv <- UI.div # set UI.style
        [ ("display","grid")
        , ("grid-template-columns","repeat(10,50px)")  -- más estrechas
        , ("grid-template-rows","repeat(5,22px)")
        , ("gap","0px")  -- menos espacio
        ]

    forM_ [0..4] $ \r -> do
        forM_ [0..9] $ \c -> do
            let clueNums = reverse $ take 5 $ reverse (colClues !! c) ++ repeat 0
            cell <- UI.div # set text (if clueNums !! r == 0 then "" else show (clueNums !! r))
                        # set UI.style [("text-align","center"),("color","white"),("font-size","20px"),("line-height","25px")]
            element colDiv #+ [element cell]

    -- ----------------------------
    -- Pistas de filas (abajo izquierda)
    -- ----------------------------
    rowDiv <- UI.div # set UI.style
        [ ("display","grid")
        , ("grid-template-columns","repeat(5,22px)")  -- más estrechas
        , ("grid-template-rows","repeat(10,50px)")
        , ("gap","0px")
        ]

    forM_ [0..9] $ \r -> do
        forM_ [0..4] $ \c -> do
            let clueNums = reverse $ take 5 $ reverse (rowClues !! r) ++ repeat 0
            cell <- UI.div # set text (if clueNums !! c == 0 then "" else show (clueNums !! c))
                        # set UI.style [("text-align","right"),("color","white"),("font-size","20px"),("line-height","25px")]
            element rowDiv #+ [element cell]

    -- ----------------------------
    -- Tablero principal 10x10 (abajo derecha)
    -- ----------------------------
    boardDiv <- UI.div # set UI.style
        [ ("display","grid")
        , ("grid-template-columns","repeat(10,50px)")
        , ("grid-template-rows","repeat(10,50px)")
        , ("gap","0px")
        ]

    forM_ [0..9] $ \r -> do
        rowElems <- forM [0..9] $ \c -> do
            cellBtn <- UI.button # set UI.style
                [ ("width","50px"),("height","50px"),("background-color","#fff"),("border","1.4px solid #ccc") ]
            element boardDiv #+ [element cellBtn]
            on UI.click cellBtn $ \_ -> do
                liftIO $ modifyIORef boardRef $ \b -> toggleCellDefault r c b
                updateBoardUI boardRef cellButtonsRef rCount cCount
            return cellBtn
        liftIO $ modifyIORef cellButtonsRef (++ [rowElems])

    -- ----------------------------
    -- Esquina superior izquierda (vacía)
    -- ----------------------------
    emptyCorner <- UI.div # set UI.style [("width","250px"),("height","250px")]
    element container #+ [element emptyCorner, element colDiv, element rowDiv, element boardDiv]

    -- ----------------------------
    -- Botones modernos
    -- ----------------------------
    newBtn <- UI.button #+ [string "Nuevo Juego"] # set UI.style modernGreen
    resetBtn <- UI.button #+ [string "Reiniciar"] # set UI.style modernRed
    backBtn <- UI.button #+ [string "←"] # set UI.style modernGreen

    -- Contenedor para botones debajo del tablero
    buttonsContainer <- UI.div # set UI.style
        [ ("display","flex")
        , ("flex-direction","column") 
        , ("gap","12px")
        , ("justify-content","center")
        , ("align-items","center")   -- centra los botones horizontalmente
        , ("margin-top","10px")          -- espacio con el tablero
        ]

    element buttonsContainer #+ [element titleContainer, element resetBtn, element newBtn, element backBtn]


    on UI.click newBtn $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount

    on UI.click resetBtn $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount

    
    element gameRoot #+ [element buttonsContainer, element container]
    return (gameRoot, backBtn)

-- ----------------------------
-- Toggle simple de celda
-- ----------------------------
toggleCellDefault :: Int -> Int -> Board -> Board
toggleCellDefault r c b =
    let row = b !! r
        newRow = take c row ++ [toggleCellState (row !! c)] ++ drop (c+1) row
    in take r b ++ [newRow] ++ drop (r+1) b

toggleCellState :: Cell -> Cell
toggleCellState Filled = Empty
toggleCellState Empty = Filled
toggleCellState Unknown = Filled


-- ----------------------------
-- ESTILOS DE BOTONES
-- ----------------------------
modernGreen :: [(String, String)]
modernGreen =
    [ ("padding","10px 20px")
    , ("background","transparent")
    , ("color","white")
    , ("border","3px solid white")
    , ("border-radius","8px")
    , ("cursor","pointer")
    , ("font-size","25px")
    , ("transition","0.2s")
    , ("box-shadow","0 5px 15px rgba(0, 0, 0, 0.3)")
    , ("hover","background:rgba(255,255,255,0.1); box-shadow:0 6px 15px rgba(0, 0, 0, 0.4)")
    ]

modernRed :: [(String, String)]
modernRed =
    [ ("padding","10px 20px")
    , ("background","transparent")
    , ("color","white")
    , ("border","3px solid white")
    , ("border-radius","8px")
    , ("cursor","pointer")
    , ("font-size","25px")
    , ("transition","0.2s")
    , ("box-shadow","0 5px 15px rgba(0, 0, 0, 0.3)")
    , ("hover","background:rgba(255,255,255,0.1); box-shadow:0 6px 15px rgba(0, 0, 0, 0.4)")
    ]

-- ----------------------------
-- ACTUALIZAR TABLERO VISUAL
-- ----------------------------
updateBoardUI :: IORef Board -> IORef [[Element]] -> Int -> Int -> UI ()
updateBoardUI boardRef buttonsRef rCount cCount = do
    b <- liftIO $ readIORef boardRef
    btns <- liftIO $ readIORef buttonsRef
    forM_ (zip [0..] btns) $ \(r, rowElems) ->
        forM_ (zip [0..] rowElems) $ \(c, cellBtn) -> do
            let color = case getCellM r c b of
                    Just Filled  -> "#d3d3d3"
                    Just Empty   -> "#ffffff"
                    Just Unknown -> "#fafafa"
                    Nothing      -> "#fafafa"
            void $ element cellBtn # set UI.style [("background-color", color)]

