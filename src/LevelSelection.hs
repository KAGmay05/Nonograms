{-# LANGUAGE OverloadedStrings #-}
module LevelSelection (levelPage) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

-- levelPage recibe la ventana, la página de bienvenida, y una función para iniciar el juego
levelPage :: Window -> Element -> (String -> UI ()) -> UI Element
levelPage window welcomeDiv startGame = do
    divL <- UI.div # set UI.style
        [ ("display","flex")
        , ("flex-direction","column")
        , ("justify-content","center")
        , ("align-items","center")
        , ("height","100vh")
        , ("width","100vw")
        , ("font-family","Segoe UI, Arial, sans-serif")
        , ("background","linear-gradient(135deg, #87CEFA 0%, #87CEFA 30%, #BA55D3 50%, #FF69B4 70%, #FF69B4 100%)")
        , ("background-size","120% 120%")
        , ("animation","pastelMove 25s ease infinite")
        ]

    -- Título
    title <- UI.h1 #+ [string "Elija un Nonograma"] # set UI.style
        [ ("color","transparent")
        , ("-webkit-text-stroke","2.5px white")
        , ("text-stroke","2.5px white")
        , ("font-size","75px")            
        , ("font-weight","900")
        , ("letter-spacing","2px")
        , ("text-transform","uppercase")
        ]

    -- Botones de niveles
    btn1 <- UI.button #+ [string "Nono 1"] # set UI.style levelBtnStyle
    btn2 <- UI.button #+ [string "Nono 2"] # set UI.style levelBtnStyle
    btn3 <- UI.button #+ [string "Nono 3"] # set UI.style levelBtnStyle
    btn4 <- UI.button #+ [string "Nono 4"] # set UI.style levelBtnStyle
    btn5 <- UI.button #+ [string "Nono 5"] # set UI.style levelBtnStyle
    btn6 <- UI.button #+ [string "Nono 6"] # set UI.style levelBtnStyle

    -- Contenedor de botones
    -- Fila 1
    row1 <- UI.div # set UI.style
        [ ("display","flex")
        , ("gap","15px")
        , ("margin-bottom","20px")
        ]

    element row1 #+ [element btn1, element btn2, element btn3]

    -- Fila 2
    row2 <- UI.div # set UI.style
        [ ("display","flex")
        , ("gap","15px")
        ]

    element row2 #+ [element btn4, element btn5, element btn6]

    -- Contenedor vertical de filas
    btnContainer <- UI.div # set UI.style
        [ ("display","flex")
        , ("flex-direction","column")
        , ("align-items","center")
        ]

    element btnContainer #+ [element row1, element row2]


    -- Botón de regreso
    backBtn <- UI.button #+ [string "←"] # set UI.style levelBackBtnStyle

    -- Agregamos todo al contenedor principal
    element divL #+ [element title, element btnContainer, element backBtn]

    -- Eventos
    on UI.click btn1 $ \_ -> startGame "nono1"
    on UI.click btn2 $ \_ -> startGame "nono2"
    on UI.click btn3 $ \_ -> startGame "nono3"
    on UI.click btn4 $ \_ -> startGame "nono4"
    on UI.click btn5 $ \_ -> startGame "nono5"
    on UI.click btn6 $ \_ -> startGame "nono6"

    on UI.click backBtn $ \_ -> do
        element divL # set UI.style [("display","none")]
        element welcomeDiv # set UI.style [("display","flex")]

    return divL

-- Estilos
levelBtnStyle :: [(String,String)]
levelBtnStyle =
    [ ("padding","12px 25px")
    , ("background","transparent")
    , ("color","white")
    , ("border","3px solid white")
    , ("border-radius","8px")
    , ("cursor","pointer")
    , ("font-size","40px")
    , ("transition","0.2s")
    , ("box-shadow","0 5px 15px rgba(0, 0, 0, 0.3)")
    , ("hover","background:rgba(255,255,255,0.1); box-shadow:0 6px 15px rgba(0, 0, 0, 0.4)")
    ]

levelBackBtnStyle :: [(String,String)]
levelBackBtnStyle =
    [ ("padding","10px 20px")
    , ("background","transparent")
    , ("color","white")
    , ("border","3px solid white")
    , ("border-radius","8px")
    , ("cursor","pointer")
    , ("font-size","15px")
    , ("margin-top","30px")
    , ("transition","0.2s")
    , ("box-shadow","0 5px 15px rgba(0, 0, 0, 0.3)")
    , ("hover","background:rgba(255,255,255,0.1); box-shadow:0 6px 15px rgba(0, 0, 0, 0.4)")
    ]
