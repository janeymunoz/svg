{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Svg where

import Protolude hiding (link)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Graphics.Svg
import System.Random (randomRIO)

import Types

showPack :: Show a => a -> Text
showPack = Text.pack . show

wrapper :: Size -> Element -> Element
wrapper (Size (Width w, Height h)) element =
  doctype <> with
    (svg11_ element)
    [ Version_ <<- "1.1"
    , ViewBox_ <<- (mconcat ["0 0 ", showPack w, " ", showPack h])
    ]

bornToDie :: BornToDie -> Element
bornToDie (BornToDie l1 l2 l3 l4 l5) =
  content (Point (X 250, Y 200)) (Radius 150)
  where
    content :: Point -> Radius -> Element
    content (Point (X cX, Y cY)) (Radius r) =
      mconcat
        [ circle_
          [ Cx_ <<- (showPack cX)
          , Cy_ <<- (showPack cY)
          , Fill_ <<- "transparent"
          , R_ <<- (showPack r)
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
        [ X_ <<- (showPack x)
        , Y_ <<- (showPack y)
        , Font_family_ <<- "Times New Roman"
        , Font_size_ <<- (showPack size)
        , Font_weight_ <<- (if bolder then "1000" else "700")
        , Text_anchor_ <<- "middle"
        , Fill_ <<- "black"
        ]
        t
    mkText :: (Int, Int) -> Int -> Text -> Element
    mkText (x, y) size s =
      text_
        [ X_ <<- (showPack x)
        , Y_ <<- (showPack y)
        , Font_family_ <<- "Sans serif"
        , Font_size_ <<- (showPack size)
        , Font_weight_ <<- "700"
        , Text_anchor_ <<- "middle"
        , Fill_ <<- "black"
        ]
        $ toElement s

pie :: PieChart -> IO Element
pie pieChart = do
  coloredSlices <- addRandomRGBs $ slices pieChart
  pure . makePie $ pieChart { slices = coloredSlices }
    where
      makePie :: PieChart -> Element
      makePie (PieChart slices title radius) =
        fst $ Protolude.foldr accum ("", 0.0) (Set.toList slices)
        where
          totalPie :: Amount
          totalPie = Protolude.foldr
            (\(Slice {portion = (Amount a)}) -> \(Amount b) -> (Amount $ a + b))
              (Amount 0) slices
          accum :: Slice -> (Element, Float) -> (Element, Float)
          accum s (e, degTot) =
            (e <> makeSlice totalPie radius degTot s, degOffset)
              where
                Percent p = percentSlice s totalPie
                degOffset = degTot + (p * 360)
          makeSlice :: Amount -> Radius -> Float -> Slice -> Element
          makeSlice (Amount totalPie) (Radius radius) deg slice = mconcat
            [ circle_
                [ R_ <<- rText
                , Cx_ <<- rTextX
                , Cy_ <<- rTextY
                , Fill_ <<- "transparent"
                ]
            , circle_
                [ R_ <<- rHidden
                , Cx_ <<- rTextX
                , Cy_ <<- rTextY
                , Fill_ <<- "transparent"
                , Stroke_ <<- Maybe.fromMaybe "transparent" mColor
                , Stroke_width_ <<- rText
                , Stroke_dasharray_ <<- mconcat
                    [ showPack $
                        (fromIntegral portion') * pi * radius' / (fromIntegral totalPie)
                    , " "
                    , showPack (radius' * pi)
                    ]
                , Transform_ <<- mconcat
                    [ "rotate("
                    , showPack deg
                    , " "
                    , rTextX
                    , " "
                    , rTextY
                    , ")"
                    ]
                ]
            , makeLabel (Amount totalPie) (Radius radius) deg slice
            ]
            where
              (Amount portion') = portion slice
              mColor = color slice
              radius' = fromIntegral radius
              rText = showPack radius
              rTextX = showPack (radius' * 2)
              rTextY = showPack (radius' * 1.5)
              rHidden = showPack (radius' / 2)
          makeLabel :: Amount -> Radius -> Float -> Slice -> Element
          makeLabel (Amount t) (Radius r) degTot s =
            polyline_
              [ Points_ <<- pointsStr
              , Fill_ <<- "none"
              , Stroke_ <<- "darkgrey"
              , Stroke_width_ <<- "1"
              ]
            <>
            case l of
              Nothing -> fieldText
              Just l' -> a_ [ XlinkHref_ <<- l' ] fieldText
            where
              fieldText =
                text_
                  [ X_ <<- (showPack $ x3 + offset)
                  , Y_ <<- (showPack $ y3 + fontOffsetY)
                  , Text_anchor_ <<- if offset > 0 then "start" else "end"
                  , Font_size_ <<- (showPack fontSize)
                  , Fill_ <<- "dimgrey"
                  ] 
                  (toElement f)
              (Field f) = field s
              l = link s
              pointsStr = mconcat
                [ showPack x1
                , ","
                , showPack y1
                , " "
                , showPack x2
                , ","
                , showPack y3
                , " "
                , showPack x3
                , ","
                , showPack y3
                ]
              Points ((x1,y1),(x2,_),(x3,y3)) =
                case quadrant degTot of
                  I -> Points ((x, y), (xPlusF, yMinusF), (x+factor3, yMinusF))
                  II -> Points ((x, y), (xMinusF, yMinusF), (x-factor3, yMinusF))
                  III -> Points ((x, y), (xMinusF, yPlusF), (x-factor3, yPlusF))
                  IV -> Points ((x, y), (xPlusF, yPlusF), (x+factor3, yPlusF))
                where
                  xPlusF = x + factor
                  xMinusF = x - factor
                  yPlusF = y + factor
                  yMinusF = y - factor
                  factor3 = 3 * factor
              x = cX + (r' + (abs offset)) * (cos (pi / 180 * degLabelAbs))
              y = cY + (r' + (abs offset)) * (sin (pi / 180 * degLabelAbs))
              fontSize = 12
              fontOffsetY = fontSize / 3
              factor = 15
              cX = r' * 2
              cY = r' * 1.5
              offset = sign * 5
                where sign = case quadrant degTot of
                               I -> 1
                               II -> (-1)
                               III -> (-1)
                               IV -> 1
              quadrant :: Float -> Quadrant
              quadrant deg
                | modDeg <= 360 && modDeg > 270 = I
                | modDeg <= 270 && modDeg > 180 = II
                | modDeg <= 180 && modDeg > 90 = III
                | otherwise = IV
                where modDeg = (fromIntegral $ mod (truncate deg :: Integer) 360) + degLabelRel
              degLabelRel = (fromIntegral a) / (fromIntegral t) * 360 / 2
              degLabelAbs = degLabelRel + degTot
              r' = fromIntegral r
              Amount a = portion s
          percentSlice :: Slice -> Amount -> Percent
          percentSlice s total = percent (portion s) total
          percent :: Amount -> Amount -> Percent
          percent (Amount part) (Amount tot) =
            Percent (fromIntegral part / fromIntegral tot)


getRandomRGB :: IO (Int, Int, Int)
getRandomRGB = do
  r <- randomRIO (0, 255)
  b <- randomRIO (0, 255)
  g <- randomRIO (0, 255)
  pure (r, g, b)

addRandomRGB :: Slice -> IO Slice
addRandomRGB s = case color s of
  Just _ -> pure s
  Nothing -> do
    (r, g, b) <- getRandomRGB
    let rgb = mconcat
          [ "rgb("
          , Text.intercalate "," $ map showPack [r, g, b]
          , ")"
          ]
    pure $ s { color = Just rgb }

addRandomRGBs :: Set.Set Slice -> IO (Set.Set Slice)
addRandomRGBs ss = do
  ssColored <- mapM addRandomRGB $ Set.toList ss
  pure $ Set.fromList ssColored

