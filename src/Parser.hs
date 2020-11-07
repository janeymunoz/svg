{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import Protolude
import Options.Applicative
import Types

bornToDie :: Parser BornToDie
bornToDie = BornToDie
      <$> strOption
          ( long "line1"
         <> short '1'
         <> help "Text for the first line"
         <> showDefault
         <> value "BORN TO APPLY" )
      <*> strOption
          ( long "line2"
         <> short '2'
         <> help "Text for the second line"
         <> showDefault
         <> value "IO IS A FUCK" )
      <*> strOption
          ( long "line3"
         <> short '3'
         <> help "Text for the third line"
         <> showDefault
         <> value "鬼神 Haskell98" )
      <*> strOption
          ( long "line4"
         <> short '4'
         <> help "Text for the fourth line"
         <> showDefault
         <> value "I am trash monad" )
      <*> strOption
          ( long "line5"
         <> short '5'
         <> help "Text for the fifth line"
         <> showDefault
         <> value "410,757,864,530 LANGUAGE PRAGMAS" )
