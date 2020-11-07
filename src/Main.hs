{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import qualified Parser
import qualified Svg

main :: IO ()
main = do
  setLocaleEncoding utf8
  cwd <- getCurrentDirectory
  p <- execParser cliParser 
  writeFile (cwd </> "out.svg") . show $ Svg.bornToDie p

cliParser = info
  (Parser.bornToDie <**> helper)
  ( fullDesc <>
    header "An SVG generator")

