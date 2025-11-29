{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (forM, forM_, when, void)
import Board
import Types

-- Ejemplo de pistas
rowCluesEx :: [[Int]]
rowCluesEx = [[3],[1,1],[5],[1,1],[3]]

colCluesEx :: [[Int]]
colCluesEx = [[3],[1,1],[5],[1,1],[3]]

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Nonogram Modern UI"

    -- Inicializar juego
    let game = initGame rowCluesEx colCluesEx
    boardRef <- liftIO $ newIORef (board game)
    let (rCount, cCount) = boardSize (board game)

    -- Lista de botones para actualizar colores
    cellButtonsRef <- liftIO $ newIORef ([] :: [[Element]])

    -- Contenedor principal
    mainDiv <- UI.div #. "container" # set UI.style
        [ ("display","flex")
        , ("flex-direction","column")
        , ("align-items","center")
        , ("font-family","Arial, sans-serif")
        , ("margin","20px")
        ]

    -- Título
    title <- UI.h1 #+ [string "Nonogram "] # set UI.style
        [("color","#333"), ("margin-bottom","20px")]
    element mainDiv #+ [element title]

    -- Contenedor tablero
    boardDiv <- UI.div #. "board" # set UI.style
        [ ("display","grid")
        , ("grid-template-columns", "repeat(" ++ show cCount ++ ", 40px)")
        , ("grid-gap","2px")
        ]

    -- Crear celdas y guardar botones en cellButtonsRef
    liftIO $ writeIORef cellButtonsRef []  -- inicializar

    forM_ [0..rCount-1] $ \r -> do
        rowElems <- forM [0..cCount-1] $ \c -> do
            cellBtn <- UI.button # set UI.style
                [ ("width","40px")
                , ("height","40px")
                , ("border-radius","6px")
                , ("border","1px solid #ccc")
                , ("background-color","#f0f0f0")
                , ("cursor","pointer")
                ]
            on UI.click cellBtn $ \_ -> do
                liftIO $ modifyIORef boardRef $ \b ->
                    case toggleCell (r,c) b of
                        Just b' -> b'
                        Nothing -> b
                updateBoardUI boardRef cellButtonsRef rCount cCount
            element boardDiv #+ [element cellBtn]
            return cellBtn
        liftIO $ modifyIORef cellButtonsRef (++ [rowElems])

    element mainDiv #+ [element boardDiv]

    -- Botones de acción
    buttonsDiv <- UI.div # set UI.style [("margin-top","20px"),("display","flex"),("gap","10px")]
    newBtn <- UI.button #+ [string "Nuevo Juego"] # set UI.style
        [("padding","8px 16px"),("border-radius","6px"),("border","none"),("background-color","#4CAF50"),("color","white"),("cursor","pointer")]
    resetBtn <- UI.button #+ [string "Reiniciar"] # set UI.style
        [("padding","8px 16px"),("border-radius","6px"),("border","none"),("background-color","#f44336"),("color","white"),("cursor","pointer")]

    -- Eventos botones
    on UI.click newBtn $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount

    on UI.click resetBtn $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount

    element buttonsDiv #+ [element newBtn, element resetBtn]
    element mainDiv #+ [element buttonsDiv]

    void $ getBody window #+ [element mainDiv]

-- Actualiza los colores del tablero
updateBoardUI :: IORef Board -> IORef [[Element]] -> Int -> Int -> UI ()
updateBoardUI boardRef buttonsRef rCount cCount = do
    b <- liftIO $ readIORef boardRef
    btns <- liftIO $ readIORef buttonsRef
    forM_ (zip [0..] btns) $ \(r, rowElems) ->
        forM_ (zip [0..] rowElems) $ \(c, cellBtn) -> do
            let color = case getCellM r c b of
                            Just Filled  -> "#4CAF50"
                            Just Empty   -> "#fff"
                            Just Unknown -> "#f0f0f0"
                            Nothing      -> "#f0f0f0"
            void $ element cellBtn # set UI.style [("background-color", color)]
