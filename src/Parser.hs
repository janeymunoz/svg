{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Protolude hiding (option)
import qualified Data.Text as Text
import Options.Applicative
import Options.Applicative.Text (text)
import Types

parseOptions :: Parser Options
parseOptions = Options <$> parseSavePath <*> parseCommand

parseSavePath :: Parser SavePath
parseSavePath = fmap SavePath .
  optional . optionText $
    ( long "path"
    <> short 'p'
    <> help "Absolute path at which to save the generated svg file"
    )

parseTest :: Parser Test
parseTest = optionText $
    ( long "test"
    <> short 't'
    <> help "test!"
    )

parseCommand :: Parser Command
parseCommand = subparser . mconcat $ map addInfo
    [ ("bornToDie", fmap BornToDie' parseBornToDie, "Born to die meme")
    , ("pie", fmap PieChart' parsePieChart, "A pretty pie chart")
    , ("test", fmap Test' parseTest, "test!!!!")
    ]
  where
  addInfo :: (Text, Parser Command, Text) -> Mod CommandFields Command
  addInfo (cmdStr, parser', desc') = 
    command (Text.unpack cmdStr) $ info (helper <*> parser') $ progDesc $ Text.unpack desc'

parseBornToDie :: Parser BornToDie
parseBornToDie = BornToDie
      <$> optionText
          ( long "line1"
         <> short '1'
         <> help "Text for the first line"
         <> showDefault
         <> value "BORN TO APPLY" )
      <*> optionText
          ( long "line2"
         <> short '2'
         <> help "Text for the second line"
         <> showDefault
         <> value "IO IS A FUCK" )
      <*> optionText
          ( long "line3"
         <> short '3'
         <> help "Text for the third line"
         <> showDefault
         <> value "鬼神 Haskell98" )
      <*> optionText
          ( long "line4"
         <> short '4'
         <> help "Text for the fourth line"
         <> showDefault
         <> value "I am trash monad" )
      <*> optionText
          ( long "line5"
         <> short '5'
         <> help "Text for the fifth line"
         <> showDefault
         <> value "410,757,864,530 LANGUAGE PRAGMAS" )

parsePieChart :: Parser [Slice]
parsePieChart = many parseSlice

parseSlice :: Parser Slice
parseSlice = Slice
  <$> (optional . fmap Field . optionText $ long "title" <> short 't')
  <*> (fmap Amount . optionInt $ long "amount" <> short 'a')
  <*> (optional . optionText $ long "color" <> short 'c')
  <*> (optional . optionText $ long "link" <> short 'l')

optionText :: Mod OptionFields Text -> Parser Text
optionText = option text

optionInt :: Mod OptionFields Int -> Parser Int
optionInt = option auto
