{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Svg where

import Protolude
import Data.String (String)
import qualified Data.Text as Text
import Graphics.Svg

import Types

wrapper :: Size -> Element -> Element
wrapper (Size (Width w, Height h)) element =
  doctype <> with
    (svg11_ element)
    [ Version_ <<- "1.1"
    , Width_ <<- intToText w
    , Height_ <<- intToText h
    ]

bornToDie :: BornToDie -> Element
bornToDie (BornToDie l1 l2 l3 l4 l5) =
  wrapper (Size (Width 500, Height 600)) $
    content (Point (X 250, Y 200)) (Radius 150)
  where
    content :: Point -> Radius -> Element
    content (Point (X cX, Y cY)) (Radius r) =
      mconcat
        [ circle_
          [ Cx_ <<- (intToText cX)
          , Cy_ <<- (intToText cY)
          , Fill_ <<- "transparent"
          , R_ <<- (intToText r)
          , Stroke_ <<- "black"
          , Stroke_width_ <<- "5"
          ]
        , mkSymbol False (cX, cY + (div r 2)) mainSymbolSize "λ"
        , mkSymbol False (cX + (div r 2), cY + r + 10) outerSymbolSize "»"
        , mkSymbol True (cX, cY - r + 25) outerSymbolSize "→"
        , mkSymbol False (cX - r, cY) outerSymbolSize "::"
        , mkSymbol False (cX - (div r 2), cY + r + 25) outerSymbolSize "⇒"
        , mkSymbol False (cX + r, cY) outerSymbolSize "∀"
        , mkText (cX, cY + r + 50) 40 l1
        , mkText (cX, cY + r + 90) 35 l2
        , mkText (cX, cY + r + 125) 30 l3
        , mkText (cX, cY + r + 155) 25 l4
        , mkText (cX, cY + r + 180) 20 l5
        ]
      where
        mainSymbolSize = (*) 85 $ div (r * 2) 100
        outerSymbolSize = (*) 3 $ div r 4
    mkSymbol :: Bool -> (Int, Int) -> Int -> Element -> Element
    mkSymbol bolder (x, y) size t =
      text_
        [ X_ <<- (intToText x)
        , Y_ <<- (intToText y)
        , Font_family_ <<- "Times New Roman"
        , Font_size_ <<- (intToText size)
        , Font_weight_ <<- (if bolder then "1000" else "700")
        , Text_anchor_ <<- "middle"
        , Fill_ <<- "black"
        ]
        t
    mkText :: (Int, Int) -> Int -> String -> Element
    mkText (x, y) size s =
      text_
        [ X_ <<- (intToText x)
        , Y_ <<- (intToText y)
        , Font_family_ <<- "Sans serif"
        , Font_size_ <<- (intToText size)
        , Font_weight_ <<- "700"
        , Text_anchor_ <<- "middle"
        , Fill_ <<- "black"
        ]
        $ toElement s

intToText :: Int -> Text
intToText = Text.pack . show
