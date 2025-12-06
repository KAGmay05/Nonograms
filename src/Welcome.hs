{-# LANGUAGE OverloadedStrings #-}
module Welcome (welcomePage) where

import Graphics.UI.Threepenny.Core
import Control.Monad (void)
import qualified Graphics.UI.Threepenny as UI

welcomePage :: Window -> UI (Element, Element)
welcomePage window = do
    divW <- UI.div # set UI.style
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

    -- Agregar animación pastel
    void $ UI.addStyleSheet window $
        "data:text/css,@keyframes pastelMove { \
        \0% { background-position: 0% 0%; } \
        \50% { background-position: 100% 100%; } \
        \100% { background-position: 0% 0%; } \
        \}"

    -- Título con estilo
    title <- UI.h1 #+ 
        [UI.span #+ [string "¡Bienvenido a Nonogramas!"] # set UI.style
            [ ("color","transparent")
            , ("-webkit-text-stroke","2.5px white")
            , ("text-stroke","2.5px white")
            , ("font-size","80px")
            , ("font-weight","900")
            , ("letter-spacing","2px")
            , ("text-transform","uppercase")
            ]
        ]
        

    -- Botón "Comenzar"
    startBtn <- UI.button #+ [string "Comenzar"] # set UI.style
        [ ("padding","12px 25px")
        , ("background","transparent")
        , ("color","white")
        , ("border","3px solid white")
        , ("border-radius","10px")
        , ("cursor","pointer")
        , ("font-size","30px")
        , ("margin-top","40px")
        , ("transition","0.2s")
        , ("box-shadow","0 5px 15px rgba(0, 0, 0, 0.3)")
        , ("hover","background:rgba(255,255,255,0.1); box-shadow:0 6px 15px rgba(0, 0, 0, 0.4)")
        ]

    element divW #+ [element title, element startBtn]
    return (divW, startBtn)
