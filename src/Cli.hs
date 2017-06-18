module Cli ( cliArgumentsParser
           , CliArguments(..)
           ) where

import Data.Default
import Data.Monoid
import Options.Applicative
import Data.Colour.RGBSpace as RGBSpace
import Data.Colour.Names as ColourNames
import Data.Colour.SRGB as SRGB
import Rendering
import GameSettings

horizontalReflectionParser :: Parser Bool
horizontalReflectionParser = switch $ long "reflect-horizontally" <> help "Reflect field horizontally"

verticalReflectionParser :: Parser Bool
verticalReflectionParser = switch $ long "reflect-vertically" <> help "Reflect field vertically"

gridThicknessParser :: Parser Int
gridThicknessParser = option auto $ long "grid-thickness" <> metavar "THICKNESS" <> help "Field grid thickness" <> value (dsGridThickness def)

parseRGB :: ReadM (RGBSpace.RGB Double)
parseRGB = eitherReader $ \s ->
  case ColourNames.readColourName s of
    Nothing ->
      case filter (null . snd) $ sRGB24reads s of
        [(colour, [])] -> Right $ SRGB.toSRGB colour
        _ -> Left "Failed to parse colour"
    Just colour -> Right $ SRGB.toSRGB colour

gridColorParser :: Parser (RGBSpace.RGB Double)
gridColorParser = option parseRGB $ long "grid-color" <> metavar "COLOR" <> help "Grid color" <> value (dsGridColor def)

backgroundColorParser :: Parser (RGBSpace.RGB Double)
backgroundColorParser = option parseRGB $ long "background-color" <> metavar "COLOR" <> help "Background color" <> value (dsBackgroundColor def)

redColorParser :: Parser (RGBSpace.RGB Double)
redColorParser = option parseRGB $ long "red-color" <> metavar "COLOR" <> help "Red color" <> value (dsRedColor def)

blackColorParser :: Parser (RGBSpace.RGB Double)
blackColorParser = option parseRGB $ long "black-color" <> metavar "COLOR" <> help "Black color" <> value (dsBlackColor def)

pointRadiusParser :: Parser Double
pointRadiusParser = option auto $ long "point-radius" <> metavar "RADIUS" <> help "Point radius" <> value (dsPointRadius def)

fillingAlphaParser :: Parser Double
fillingAlphaParser = option auto $ long "filling-alpha" <> metavar "ALPHA" <> help "Filling alpha" <> value (dsFillingAlpha def)

fullFillParser :: Parser Bool
fullFillParser = fmap not $ switch $ long "no-full-fill" <> help "Reflect field horizontally"

drawSettingsParser :: Parser DrawSettings
drawSettingsParser =
  DrawSettings <$>
  horizontalReflectionParser <*>
  verticalReflectionParser <*>
  gridThicknessParser <*>
  gridColorParser <*>
  backgroundColorParser <*>
  redColorParser <*>
  blackColorParser <*>
  pointRadiusParser <*>
  fillingAlphaParser <*>
  fullFillParser

widthParser :: Parser Int
widthParser = option auto $ long "width" <> short 'w' <> metavar "WIDTH" <> help "Field width" <> value (gsWidth def)

heightParser :: Parser Int
heightParser = option auto $ long "height" <> short 'h' <> metavar "HEIGHT" <> help "Field width" <> value (gsHeight def)

genMoveTypeParser :: Parser GenMoveType
genMoveTypeParser =
  flag' Simple (long "simple" <> short 's' <> help "Use gen_move") <|>
  fmap WithTime (option auto $ long "time" <> short 't' <> metavar "TIME" <> help "Use gen_move_with_time") <|>
  fmap WithComplexity (option auto $ long "complexity" <> short 'c' <> metavar "COMPLEXITY" <> help "Use gen_move_with_complexity") <|>
  pure (gsGenMoveType def)

beginPatternParser :: Parser BeginPattern
beginPatternParser = option auto $ long "pattern" <> short 'p' <> metavar "PATTERN" <> help "Begin pattern" <> value (gsBeginPattern def)

redBotPathParser :: Parser (Maybe String)
redBotPathParser = optional $ strOption $ long "red" <> metavar "BOT" <> help "Red bot"

blackBotPathParser :: Parser (Maybe String)
blackBotPathParser = optional $ strOption $ long "black" <> metavar "BOT" <> help "Black bot"

gameSettingsParser :: Parser GameSettings
gameSettingsParser =
  GameSettings <$>
  widthParser <*>
  heightParser <*>
  genMoveTypeParser <*>
  beginPatternParser <*>
  redBotPathParser <*>
  blackBotPathParser

data CliArguments = CliArguments { cliDawSettings :: DrawSettings
                                 , cliGameSettings :: GameSettings
                                 }

cliArgumentsParser :: Parser CliArguments
cliArgumentsParser =
  CliArguments <$>
  drawSettingsParser <*>
  gameSettingsParser
