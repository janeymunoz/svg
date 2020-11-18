{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Protolude

newtype SavePath = SavePath (Maybe Text)

data Options = Options SavePath Command

data Command
  = BornToDie' BornToDie
  | PieChart' [Slice]

data BornToDie =
  BornToDie
    { line1 :: Text
    , line2 :: Text
    , line3 :: Text
    , line4 :: Text
    , line5 :: Text
    }

newtype Point = Point (XCoordinate, YCoordinate)

newtype Radius = Radius Int
  deriving (Eq, Ord, Show)

newtype Height = Height Int

newtype Size = Size (Width, Height)

newtype Width = Width Int

newtype XCoordinate = X Int

newtype YCoordinate = Y Int

newtype Field = Field Text
  deriving (Eq, Ord, Read)

newtype Unit = Unit Text

newtype Title = Title Text

newtype Amount = Amount Int
  deriving (Eq, Ord, Read)

newtype Percent = Percent Float
  deriving (Eq, Ord)

data Slice = Slice
  { field :: Field
  , portion :: Amount
  , color :: Maybe Text
  , link :: Maybe Text
  }
  deriving (Eq, Ord)

data PieChart = PieChart
  { slices :: Set Slice
  , title :: Maybe Title
  , radius :: Radius
  }

newtype Points = Points (Coord, Coord, Coord)

type Coord = (Float, Float)

data Quadrant = I | II | III | IV
