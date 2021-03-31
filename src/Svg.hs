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
          makeSlice (Amount totalPie) (Radius radius) deg slice = mconcat $
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
            ] <>
              case field slice of
                Nothing -> []
                Just (Field field'') -> [ makeLabel (Amount totalPie) (Radius radius) deg slice field'' ]
            where
              (Amount portion') = portion slice
              mColor = color slice
              radius' = fromIntegral radius
              rText = showPack radius
              rTextX = showPack (radius' * 2)
              rTextY = showPack (radius' * 1.5)
              rHidden = showPack (radius' / 2)
          makeLabel :: Amount -> Radius -> Float -> Slice -> Text -> Element
          makeLabel (Amount t) (Radius r) degTot s field''' =
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
                  (toElement field''')
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

rgbToHue :: (Int, Int, Int) -> Int
rgbToHue (r, g, b)
  | max' == min' = 0 -- achromatic
  | otherwise = if hue' < 0 then (+) 360 $ hue' * 60 else hue' * 60
  where
    r' = div r 255
    g' = div g 255
    b' = div b 255
    max' = maximum [r', g', b']
    min' = minimum [r', g', b']
    diff' = max' - min'
    hue'
      | max' == r' = div (g' - b') diff'
      | max' == g' = (+) 2 $ div (b' - r') diff'
      | otherwise = (+) 4 $ div (r' - g') diff'

sortedRgbArr :: [((Int, Int, Int), Int)]
sortedRgbArr = sortBy (\((_,_,_),h1) -> \((_,_,_),h2) -> compare h1 h2) . zip rgbs $ map rgbToHue rgbs
  where
    rgbs = [ (r, g, b) | r <-[0..255], g <- [0..255], b <- [0..255] ]

colorWheel' :: PieChart
colorWheel' = PieChart
  { slices = slices
  , title = Nothing
  , radius = Radius 100
  }
  where
    rgbs = sortBy (\((_,_,_),h1) -> \((_,_,_),h2) -> compare h1 h2) $ [ ((r, g, b), rgbToHue (r, g, b)) | r <-[0..25], g <- [75..100], b <- [175..200] ] <> [ ((r, g, b), rgbToHue (r, g, b)) | r <-[75..100], g <- [230..255], b <- [140..160] ]
    slices :: Set Slice
    slices = Set.fromList $ map (\(rgb,_) -> mkSlice'  rgb) rgbs
    mkSlice' :: (Int, Int, Int) -> Slice
    mkSlice' (r,g,b) = Slice
      { field = Nothing
      , portion = Amount 1
      , color = Just $ mconcat ["rgb(", Text.intercalate "," $ map showPack [r, g, b], ")" ]
      , link = Nothing
      }

colorWheel :: IO Element
colorWheel = do
  p <- pie colorWheel'
  pure $ wrapper (Size (Width 400, Height 275)) p 

x :: Element
x =
  path_ [ D_ <<- "M 50 50 L 50 0 A 50 50, 0, 0, 1, 100, 50 Z"
        , Stroke_ <<- "black"
        , Fill_ <<- "green"
        , Stroke_width_ <<- "2"
        ]
  --path_ [ D_ <<- "M 10 315\nL 110 215\nA 30 50 0 0 1 162.55 162.45\nL 172.55 152.45\nL 315 10"
  --      , Stroke_ <<- "black"
  --      , Fill_ <<- "green"
  --      , Stroke_width_ <<- "2"
  --      , Fill_opacity_ <<- "0.5"
  --      ]

-- x ^ 2 + y ^ 2 = 1
-- c = 2 pi r
-- A = pi r ^ 2
-- 
-- The 12:00 position is considered zero, and increases positively clockwise.
pointsOnCircle :: Float -> [Float] -> [(Float, Float)]
pointsOnCircle radius percents = snd $ foldr f (0, []) percents
  where
    f :: Float -> (Float, [(Float, Float)]) -> (Float, [(Float, Float)])
    f percent (radPrevious, coords) = (radNew, (xOrY True ,xOrY False):coords)
      where
        radNew = (+) radPrevious $ percentToRad percent
        xOrY isX = if abs v < 0.1 then 0 else v
          where
            v = (*) radius $ (if isX then cos else sin) radNew

pointsOnCircleAdjusted :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
pointsOnCircleAdjusted (centerX, centerY) points = map (\(x,y) -> (x+centerX, centerY - y)) points

-- 360 deg in a circle
-- I know what percent of the circle
--
--             (0, r)
--
-- (-r, 0)     (0, 0)     (r, 0)
--
--             (0, -r)
--
-- sin = o/h  cos = a/h  tan = o/a  h==r
--
-- cos(T) = X / R -> X = cos(T) * R
-- sin(T) = Y / R -> Y = sin(T) * R
percentToRad :: Float -> Float
percentToRad percent = percent * 2 * pi

arc :: Element
arc = foldr x ""
  [ (200.0,100.0, 0, 0, "red")
  , (180.90173,158.7785,200.0,100.0, "orange")
  , (130.90172,195.10565, 180.90173,158.7785, "yellow")
  , (100.0,200.0, 130.90172,195.10565, "green")
  , (0.0,100.0, 100.0,200.0, "blue")
  ]
  --foldr mkSlice' "" [(50, 0, 100, 50, "blue"), (100, 50, 50, 100, "green")]
  where
    x :: (Float, Float, Float, Float, Text) -> Element -> Element
    x (xStart, yStart, xEnd, yEnd, color) elem = mconcat [ elem,
      path_
        [ D_ <<- mconcat
            ["M 50 50 L "
            , showPack xStart
            , " "
            , showPack yStart
            , " A 50 50, 0, 0, 1, "
            , showPack xEnd
            , " "
            , showPack yEnd
            , " Z"
            ]
        , Fill_ <<- color
        ]
                                                         ]
    mkSlice' :: (Float, Float, Float, Float, Text) -> Element -> Element
    mkSlice' (xStart, yStart, xEnd, yEnd, color) elem = mconcat [ elem,
      path_
        [ D_ <<- mconcat
            ["M 50 50 L "
            , showPack xStart
            , " "
            , showPack yStart
            , " A 50 50, 0, 0, 1, "
            , showPack xEnd
            , " "
            , showPack yEnd
            , " Z"
            ]
        , Fill_ <<- color
        ]
                                                                ]
    mkSlice :: (Float, Float) -> (Float, Float) -> Text -> Element
    mkSlice (xStart, yStart) (xEnd, yEnd) color = mconcat [
      path_
        [ D_ <<- mconcat
            ["M 50 50 L "
            , showPack xStart
            , " "
            , showPack yStart
            , " A 50 50, 0, 0, 1, "
            , showPack xEnd
            , " "
            , showPack yEnd
            , " Z"
            ]
        , Fill_ <<- color
        ]
      , path_
        [ D_ <<- mconcat
            ["M 50 50 L "
            , showPack xEnd
            , " "
            , showPack yEnd
            , " A 50 50, 0, 0, 1, "
            , showPack 50
            , " "
            , showPack 100
            , " Z"
            ]
        , Fill_ <<- "green"
        ]
                                                          ]
