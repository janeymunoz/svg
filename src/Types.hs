{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Protolude
import Data.String (String)

data BornToDie =
  BornToDie
    { line1 :: String
    , line2 :: String
    , line3 :: String
    , line4 :: String
    , line5 :: String
    }

newtype Point = Point (XCoordinate, YCoordinate)

newtype Radius = Radius Int

newtype Height = Height Int

newtype Size = Size (Width, Height)

newtype Width = Width Int

newtype XCoordinate = X Int

newtype YCoordinate = Y Int

