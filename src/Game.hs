{-# LANGUAGE OverloadedStrings #-}
module Game (gamePage) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (forM_, forM, void)
import Data.List (transpose)
import Board
import Types

data ClickMode = PaintFilled | PaintEmpty

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
    modeRef  <- liftIO $ newIORef PaintFilled
    let (rCount, cCount) = boardSize (board game)
    cellButtonsRef <- liftIO $ newIORef ([] :: [[Element]])
    statusSpan <- UI.span # set text "" # set UI.style
        [ ("color","#ff2f45")
        , ("font-size","16px")
        , ("font-weight","700")
        , ("display","block")      -- siempre ocupa espacio
        , ("min-height","24px")     -- altura fija para evitar saltos
        , ("max-height","24px")
        , ("line-height","24px")
        , ("white-space","nowrap")
        , ("text-align","center")
        , ("visibility","hidden")   -- oculto sin colapsar
        , ("flex-shrink","0")
        ]

    -- Overlay de victoria
    winOverlay <- UI.div # set UI.style
        [ ("position","fixed")
        , ("top","0")
        , ("left","0")
        , ("width","100vw")
        , ("height","100vh")
        , ("background","rgba(0,0,0,0.65)")
        , ("display","none")
        , ("justify-content","center")
        , ("align-items","center")
        , ("z-index","999")
        ]

    winCard <- UI.div # set UI.style
        [ ("background","#0f0f0fcc")
        , ("color","white")
        , ("padding","24px 32px")
        , ("border-radius","12px")
        , ("backdrop-filter","blur(4px)")
        , ("box-shadow","0 12px 40px rgba(0,0,0,0.55)")
        , ("display","flex")
        , ("flex-direction","column")
        , ("align-items","center")
        , ("gap","16px")
        ]

    winTitle <- UI.h2 # set text "Nivel completado" # set UI.style
        [ ("margin","0")
        , ("font-size","28px")
        , ("letter-spacing","1px")
        ]

    winButtonsRow <- UI.div # set UI.style
        [ ("display","flex")
        , ("gap","12px")
        ]

    winRetry <- UI.button #+ [string "Volver a jugar"] # set UI.style modernGreen
    winBack  <- UI.button #+ [string "Elegir otro nivel"] # set UI.style modernRed

    element winButtonsRow #+ [element winRetry, element winBack]
    element winCard #+ [element winTitle, element winButtonsRow]
    element winOverlay #+ [element winCard]

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
                mode <- liftIO $ readIORef modeRef
                liftIO $ modifyIORef boardRef $ \b ->
                    let current = getCellM r c b
                    in case mode of
                        PaintFilled ->
                            case current of
                                Just Filled -> maybe b id (setCell (r,c) Unknown b)
                                _           -> maybe b id (setCell (r,c) Filled b)
                        PaintEmpty  ->
                            case current of
                                Just Empty  -> maybe b id (setCell (r,c) Unknown b)
                                _           -> maybe b id (setCell (r,c) Empty b)

                updateBoardUI boardRef cellButtonsRef rCount cCount statusSpan winOverlay rowClues colClues
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
    newBtn <- UI.button #+ [string "Pista"] # set UI.style modernGreen
    resetBtn <- UI.button #+ [string "Reiniciar"] # set UI.style modernRed
    backBtn <- UI.button #+ [string "←"] # set UI.style modernGreen
    modeBtn <- UI.button # set UI.style modernGreen

    let
        goBack :: UI ()
        goBack = return ()

        applyModeStyle m =
            case m of
                PaintFilled ->
                    void $ element modeBtn # set UI.style
                        [ ("padding","10px 20px")
                        , ("background","linear-gradient(to right, rgba(100,100,100,0.35) 0%, rgba(100,100,100,0.35) 50%, transparent 50%, transparent 100%)")
                        , ("color","white")
                        , ("border","3px solid white")
                        , ("border-radius","8px")
                        , ("cursor","pointer")
                        , ("font-size","25px")
                        , ("transition","0.2s")
                        , ("box-shadow","0 5px 15px rgba(0,0,0,0.3)")
                        ]

                PaintEmpty ->
                    void $ element modeBtn # set UI.style
                        [ ("padding","10px 20px")
                        , ("background","linear-gradient(to right, transparent 0%, transparent 50%, rgba(100,100,100,0.35) 50%, rgba(100,100,100,0.35) 100%)")
                        , ("color","white")
                        , ("border","3px solid white")
                        , ("border-radius","8px")
                        , ("cursor","pointer")
                        , ("font-size","25px")
                        , ("transition","0.2s")
                        , ("box-shadow","0 5px 15px rgba(0,0,0,0.3)")
                        ]


    applyModeStyle PaintFilled

    modeIconsContainer <- UI.div # set UI.style
        [ ("display", "flex")
        , ("flex-direction", "row")
        , ("align-items", "center")
        , ("gap", "10px")  -- espacio entre íconos
        , ("height", "25px") 
        ]

    filledDiv <- UI.div # set UI.style
        [ ("width","25px")
        , ("height","25px")
        , ("background-color","white")  -- cuadrado sólido
        , ("border-radius","3px")
        , ("margin-right","5px")
        ]

    divider <- UI.div # set UI.style
        [ ("width","2.3px")
        , ("height","45px")
        , ("background-color","white")
        , ("margin-left","-4px")
        , ("margin-top","-10px")
        , ("align-self","stretch") 
        ]


   -- Cruz para "vacío"
    emptyDiv <- UI.div # set UI.style
        [ ("width","25px")
        , ("height","25px")
        , ("position","relative")
        , ("background-color","transparent")
        ]

    -- Crear las dos líneas de la cruz
    line1 <- UI.div # set UI.style
        [ ("position","absolute")
        , ("width","2px")
        , ("height","100%")
        , ("background-color","white")
        , ("transform","rotate(45deg)")
        , ("top","0")
        , ("left","50%")
        , ("transform-origin","center")
        ]

    line2 <- UI.div # set UI.style
        [ ("position","absolute")
        , ("width","2px")
        , ("height","100%")
        , ("background-color","white")
        , ("transform","rotate(-45deg)")
        , ("top","0")
        , ("left","50%")
        , ("transform-origin","center")
        ]

    element emptyDiv #+ [element line1, element line2]
    

    element modeIconsContainer #+ [element filledDiv, element divider, element emptyDiv]
    element modeBtn #+ [element modeIconsContainer]
   

    filledRef <- liftIO $ newIORef filledDiv
    emptyRef  <- liftIO $ newIORef emptyDiv
    

    on UI.click modeBtn $ \_ -> do
        liftIO $ modifyIORef modeRef $ \m ->
            case m of
                PaintFilled -> PaintEmpty
                PaintEmpty  -> PaintFilled

        m <- liftIO $ readIORef modeRef
        applyModeStyle m



    -- Contenedor para botones debajo del tablero
    buttonsContainer <- UI.div # set UI.style
        [ ("display","flex")
        , ("flex-direction","column") 
        , ("gap","12px")
        , ("justify-content","center")
        , ("align-items","center")   -- centra los botones horizontalmente
        , ("margin-top","10px")          -- espacio con el tablero
        ]


    element buttonsContainer #+ [element titleContainer, element modeBtn,element resetBtn, element newBtn, element backBtn]
    element buttonsContainer #+ [element statusSpan]

    -- Overlay listeners reutilizando los botones existentes
    on UI.click winRetry $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount statusSpan winOverlay rowClues colClues
        void $ element winOverlay # set UI.style [("display","none")]
    on UI.click winBack $ \_ -> do
        void $ element winOverlay # set UI.style [("display","none")]
        void $ element statusSpan  # set UI.style [("display","block"),("visibility","hidden")]
        runFunction $ ffi "setTimeout(function(){%1.click()},10)" backBtn




    on UI.click newBtn $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount statusSpan winOverlay rowClues colClues
        void $ element winOverlay # set UI.style [("display","none")]

    on UI.click resetBtn $ \_ -> do
        liftIO $ writeIORef boardRef (board $ initGame rowCluesEx colCluesEx)
        updateBoardUI boardRef cellButtonsRef rCount cCount statusSpan winOverlay rowClues colClues
        void $ element winOverlay # set UI.style [("display","none")]


    
    element gameRoot #+ [element winOverlay, element buttonsContainer, element container]
    updateBoardUI boardRef cellButtonsRef rCount cCount statusSpan winOverlay rowClues colClues
    return (gameRoot, backBtn)

-- ----------------------------
-- Toggle simple de celda
-- ----------------------------
-- toggleCellDefault :: Int -> Int -> Board -> Board
-- toggleCellDefault r c b =
--     let row = b !! r
--         newRow = take c row ++ [toggleCellState (row !! c)] ++ drop (c+1) row
--     in take r b ++ [newRow] ++ drop (r+1) b

-- toggleCellState :: Cell -> Cell
-- toggleCellState Filled = Empty
-- toggleCellState Empty = Filled
-- toggleCellState Unknown = Filled


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
updateBoardUI :: IORef Board -> IORef [[Element]] -> Int -> Int -> Element -> Element -> [[Int]] -> [[Int]] -> UI ()
updateBoardUI boardRef buttonsRef rCount cCount statusSpan winOverlay rowClues colClues = do
    b <- liftIO $ readIORef boardRef
    btns <- liftIO $ readIORef buttonsRef
    let badRows = [i | (i,(cl,row)) <- zip [0..] (zip rowClues b), rowContradicts cl row]
        badCols = [i | (i,(cl,col)) <- zip [0..] (zip colClues (transpose b)), rowContradicts cl col]
        bad = not (null badRows) || not (null badCols)
        solved = not bad && boardSatisfies rowClues colClues b

    forM_ (zip [0..] btns) $ \(r, rowElems) ->
        forM_ (zip [0..] rowElems) $ \(c, cellBtn) -> do
            let (bgcolor, icon, fontSize) = case getCellM r c b of
                    Just Filled  -> ("#d3d3d3", "", "24px")
                    Just Empty   -> ("#fafafa", "\x2717", "32px")
                    Just Unknown -> ("#fafafa", "", "24px")
                    Nothing      -> ("#fafafa", "", "24px")
                isBad = r `elem` badRows || c `elem` badCols
                borderColor = if isBad then "#ff6b6b" else "#ccc"
                shadow = if isBad then "inset 0 0 6px 2px rgba(255,0,0,0.45)" else "none"
            void $ element cellBtn # set UI.style 
                [ ("background-color", bgcolor)
                , ("color", "#d3d3d3")
                , ("font-size", fontSize)
                , ("border", "1.4px solid " ++ borderColor)
                , ("box-shadow", shadow)
                ]
            void $ element cellBtn # set text icon

    if bad
        then do
            void $ element statusSpan # set text "Error" # set UI.style [("color","#ff2f45"),("font-size","16px"),("font-weight","700"),("display","block"),("visibility","visible")]
            void $ element winOverlay # set UI.style [("display","none")]
        else if solved
            then do
                void $ element statusSpan # set text "" # set UI.style [("display","block"),("visibility","hidden")]
                void $ element winOverlay # set UI.style [("display","flex")]
            else do
                void $ element statusSpan # set text "" # set UI.style [("display","block"),("visibility","hidden")]
                void $ element winOverlay # set UI.style [("display","none")]

