{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Welcome
import Control.Monad (void)
import Game
import LevelSelection
import Levels.Level1 as L1
import Levels.Level2 as L2
import Levels.Level3 as L3
import Levels.Level4 as L4
import Levels.Level5 as L5
import Levels.Level6 as L6


main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = mdo
    return window # set UI.title "NONOGRAM UI"
    body <- getBody window

    -- Página bienvenida
    (welcomeDiv, startBtn) <- welcomePage window

    -- Contenedor único donde va el juego
    gameContainer <- UI.div # set UI.style
        [ ("display","none")
        , ("width","100%")
        , ("height","100%")
        , ("justify-content","center")
        , ("align-items","center")
        ]

    -- Página de selección de niveles
    levelDiv <- levelPage window welcomeDiv $ \levelName -> do

        -- Crear pantalla de juego
        let (rowClues, colClues) = case levelName of
                "nono1" -> L1.boardClues
                "nono2" -> L2.boardClues
                "nono3" -> L3.boardClues
                "nono4" -> L4.boardClues
                "nono5" -> L5.boardClues
                "nono6" -> L6.boardClues


        (gameDiv, backBtn) <- gamePage window rowClues colClues



        -- Limpiar contenedor del juego y meter el nuevo juego
        void $ element gameContainer # set children []
        void $ element gameContainer #+ [element gameDiv]

        -- Mostrar juego y ocultar niveles
        void $ element gameContainer # set UI.style [("display","flex")]
        void $ element levelDiv      # set UI.style [("display","none")]

        -- Botón volver
        on UI.click backBtn $ \_ -> do
            void $ element gameContainer # set UI.style [("display","none")]
            void $ element levelDiv      # set UI.style [("display","flex")]
            void $ element gameContainer # set children []

    -- Mostrar solo bienvenida al inicio
    void $ element levelDiv # set UI.style [("display","none")]
    void $ element body #+ [element welcomeDiv, element levelDiv, element gameContainer]

    -- Botón comenzar
    on UI.click startBtn $ \_ -> do
        void $ element welcomeDiv # set UI.style [("display","none")]
        void $ element levelDiv   # set UI.style [("display","flex")]
