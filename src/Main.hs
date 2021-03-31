{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import qualified Parser
import qualified Svg
import Types

main :: IO ()
main = do
  setLocaleEncoding utf8
  run =<< execParser (info (helper <*> Parser.parseOptions) $ progDesc "Generate some svgs!")

run :: Options -> IO ()
run (Options (SavePath sp) cmd) = do
  cwd <- getCurrentDirectory
  svg <- case cmd of
    BornToDie' options -> do
      pure $ Svg.wrapper (Size (Width 500, Height 600)) $ Svg.bornToDie options
    PieChart' options -> do
      out <- Svg.pie $ PieChart (Set.fromList options) Nothing (Radius 100)
      pure $ Svg.wrapper (Size (Width 400, Height 275)) out
    Test' _ -> do
      let out = Svg.arc
      pure $ Svg.wrapper (Size (Width 400, Height 275)) out
  writeFile (Maybe.fromMaybe (cwd </> "out.svg") $ fmap Text.unpack sp) . show $ svg
