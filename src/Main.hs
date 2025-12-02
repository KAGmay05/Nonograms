{-# LANGUAGE OverloadedStrings #-}
module Main where

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

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    void $ return window # set UI.title "NONOGRAM UI"
    body <- getBody window

    -- ----------------------------
    -- FONDO PASTEL EN DEGRADADO ANIMADO
    -- ----------------------------
    -- FONDO PASTEL EN DEGRADADO ANIMADO
    element body # set UI.style
        [ ("margin","0")
        , ("height","100vh")
        , ("width","100vw")
        , ("display","flex")
        , ("justify-content","center")
        , ("align-items","center")
        , ("position","relative")
        , ("font-family","Segoe UI, Arial, sans-serif")
        , ("background","linear-gradient(135deg, #87CEFA 0%, #87CEFA 30%, #BA55D3 50%, #FF69B4 70%, #FF69B4 100%)")
        , ("background-size","120% 120%")
        , ("animation","pastelMove 25s ease infinite")
        , ("overflow","hidden")
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
    let game = initGame rowCluesEx colCluesEx
    boardRef <- liftIO $ newIORef (board game)
    let (rCount, cCount) = boardSize (board game)
    cellButtonsRef <- liftIO $ newIORef ([] :: [[Element]])

    -- Contenedor principal
    mainDiv <- UI.div #. "container" # set UI.style
        [ ("display","flex")
        , ("flex-direction","column")
        , ("align-items","center")
        , ("gap","10px")
        , ("margin","20px")
        ]

    -- ----------------------------
    -- TITULO CON NEÓN INTERACTIVO
    -- ----------------------------
    titleLabel <- UI.h1 #+ 
        [ UI.span #+ [string "✨"] # set UI.style
            [ ("font-size","80px")
            , ("margin-right","10px")
            , ("margin-left","10px")
            ]
        , UI.span #+ [string "NONOGRAM"] # set UI.style 
            [ ("color","transparent")          -- texto transparente
            , ("-webkit-text-stroke","2px white") -- contorno blanco
            , ("text-stroke","2px white")         -- contorno blanco (para compatibilidad)
            , ("font-size","64px")
            , ("font-weight","900")
            , ("letter-spacing","3px")
            , ("text-transform","uppercase")
            , ("text-shadow","0 0 2px #eae7e70f, 0 0 4px #f5f2f20a") -- sombra sutil
            ]
        , UI.span #+ [string "✨"] # set UI.style
            [ ("font-size","80px")
            , ("margin-right","10px")
            , ("margin-left","10px")
            ]
        ] # set UI.style
            [ ("padding","10px 20px")
            , ("margin-bottom","0px")
            , ("text-align","center")
            ]



    element mainDiv #+ [element titleLabel]

    -- ----------------------------
    -- TABLERO
    -- ----------------------------
    boardWrapper <- UI.div # set UI.style [("padding","0px")]
    boardDiv <- UI.div # set UI.style
        [ ("display","grid")
        , ("grid-template-columns", "repeat(" ++ show cCount ++ ", 50px)")
        , ("grid-gap","0px")
        ]
    liftIO $ writeIORef cellButtonsRef []

    forM_ [0..rCount-1] $ \r -> do
        rowElems <- forM [0..cCount-1] $ \c -> do
            cellBtn <- UI.button # set UI.style
                [ ("width","50px")
                , ("height","50px")
                , ("border","1px solid #ccc")
                , ("background-color","#fff")
                , ("cursor","pointer")
                ]
            -- Hover
            element cellBtn # set UI.style
                [ ("onmouseover","this.style.backgroundColor='#e6f2ff'")
                , ("onmouseout","this.style.backgroundColor=''")
                ]
            -- Click para alternar
            on UI.click cellBtn $ \_ -> do
                liftIO $ modifyIORef boardRef $ \b -> case toggleCell (r,c) b of
                    Just b' -> b'; Nothing -> b
                updateBoardUI boardRef cellButtonsRef rCount cCount
            element boardDiv #+ [element cellBtn]
            return cellBtn
        liftIO $ modifyIORef cellButtonsRef (++ [rowElems])

    element boardWrapper #+ [element boardDiv]
    element mainDiv #+ [element boardWrapper]

    -- ----------------------------
    -- BOTONES MODERNOS
    -- ----------------------------
    buttons <- UI.div # set UI.style [("display","flex"), ("gap","12px")]
    newBtn <- UI.button #+ [string "Nuevo Juego"] # set UI.style modernGreen
    resetBtn <- UI.button #+ [string "Reiniciar"] # set UI.style modernRed

    on UI.click newBtn $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount

    on UI.click resetBtn $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount

    element buttons #+ [element newBtn, element resetBtn]
    element mainDiv #+ [element buttons]

    void $ getBody window #+ [element mainDiv]

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
    , ("font-size","15px")
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
    , ("font-size","15px")
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
