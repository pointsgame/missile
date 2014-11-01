module Settings where

import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.ConfigFile
import Data.Either.Utils
import System.Directory
import qualified System.IO.Strict as Strict
import Control.Exception

data GenMoveType = Simple | WithTime Int | WithComplexity Int
    deriving (Eq, Show, Read)

data Settings = Settings { gameWidth :: Int,
                           gameHeight :: Int,
                           gameName :: String,
                           redName :: String,
                           blackName :: String,
                           redColor :: RGB Double,
                           blackColor :: RGB Double,
                           backgroundColor :: RGB Double,
                           gridColor :: RGB Double,
                           fillingAlpha :: Double,
                           pointRadius :: Double,
                           horizontalReflection :: Bool,
                           verticalReflection :: Bool,
                           fullFill :: Bool,
                           gridThickness :: Int,
                           aiPresent :: Bool,
                           aiPath :: String,
                           aiRespondent :: Bool,
                           aiGenMoveType :: GenMoveType }

defaultSettings :: Settings
defaultSettings = Settings { gameWidth = 39,
                             gameHeight = 32,
                             gameName = "Game",
                             redName = "Red",
                             blackName = "Black",
                             redColor = RGB 1 0 0,
                             blackColor = RGB 0 0 0,
                             backgroundColor = RGB 1 1 1,
                             gridColor = RGB 0.3 0.3 0.3,
                             fillingAlpha = 0.5,
                             pointRadius = 1,
                             horizontalReflection = False,
                             verticalReflection = False,
                             fullFill = True,
                             gridThickness = 2,
                             aiPresent = False,
                             aiPath = "./ai",
                             aiRespondent = True,
                             aiGenMoveType = Simple }

readSettings :: String -> IO Settings
readSettings fileName = do fileExist <- doesFileExist fileName
                           cfgEither <- if fileExist
                                        then do cfgStrEither <- try (Strict.readFile fileName) :: IO (Either SomeException String)
                                                return $ case cfgStrEither of
                                                  Left ex      -> Left (OtherProblem (show ex), fileName)
                                                  Right cfgStr -> readstring emptyCP cfgStr
                                        else return (Left (OtherProblem "File doesn't exists." , fileName))
                           return $ case cfgEither of
                             Left _    -> defaultSettings
                             Right cfg -> let gameWidthConf = get cfg "Game" "game_width"
                                              gameHeightConf = get cfg "Game" "game_height"
                                              gameNameConf = get cfg "Game" "game_name"
                                              redNameConf = get cfg "Players" "red_name"
                                              blackNameConf = get cfg "Players" "black_name"
                                              redColorConf = get cfg "Players" "red_color"
                                              blackColorConf = get cfg "Players" "black_color"
                                              backgroundColorConf = get cfg "Other" "background_color"
                                              gridColorConf = get cfg "Other" "grid_color"
                                              fillingAlphaConf = get cfg "Other" "filling_alpha"
                                              pointRadiusConf = get cfg "Other" "point_radius"
                                              horizontalReflectionConf = get cfg "Other" "horizontal_reflection"
                                              verticalReflectionConf = get cfg "Other" "vertical_reflection"
                                              fullFillConf = get cfg "Other" "full_fill"
                                              gridThicknessConf = get cfg "Other" "grid_thickness"
                                              aiPresentConf = get cfg "AI" "ai_present"
                                              aiPathConf = get cfg "AI" "ai_path"
                                              aiRespondentConf = get cfg "AI" "ai_respondent"
                                              aiGenMoveTypeConf = get cfg "AI" "ai_gen_move_type"
                                          in Settings { gameWidth = case gameWidthConf of
                                                                      Left _    -> gameWidth defaultSettings
                                                                      Right int -> int,
                                                        gameHeight = case gameHeightConf of
                                                                      Left _    -> gameHeight defaultSettings
                                                                      Right int -> int,
                                                        gameName = case gameNameConf of
                                                                      Left _    -> gameName defaultSettings
                                                                      Right str -> str,
                                                        redName = case redNameConf of
                                                                    Left _    -> redName defaultSettings
                                                                    Right str -> str,
                                                        blackName = case blackNameConf of
                                                                      Left _    -> blackName defaultSettings
                                                                      Right str -> str,
                                                        redColor = case redColorConf of
                                                                     Left _    -> redColor defaultSettings
                                                                     Right str -> toSRGB $ sRGB24read str,
                                                        blackColor = case blackColorConf of
                                                                       Left _    -> blackColor defaultSettings
                                                                       Right str -> toSRGB $ sRGB24read str,
                                                        backgroundColor = case backgroundColorConf of
                                                                            Left _    -> backgroundColor defaultSettings
                                                                            Right str -> toSRGB $ sRGB24read str,
                                                        gridColor = case gridColorConf of
                                                                      Left _    -> gridColor defaultSettings
                                                                      Right str -> toSRGB $ sRGB24read str,
                                                        fillingAlpha = case fillingAlphaConf of
                                                                         Left _    -> fillingAlpha defaultSettings
                                                                         Right int -> int,
                                                        pointRadius = case pointRadiusConf of
                                                                        Left _    -> pointRadius defaultSettings
                                                                        Right val -> val,
                                                        horizontalReflection = case horizontalReflectionConf of
                                                                                 Left _    -> horizontalReflection defaultSettings
                                                                                 Right val -> val,
                                                        verticalReflection = case verticalReflectionConf of
                                                                                 Left _    -> verticalReflection defaultSettings
                                                                                 Right val -> val,
                                                        fullFill = case fullFillConf of
                                                                     Left _    -> fullFill defaultSettings
                                                                     Right val -> val,
                                                        gridThickness = case gridThicknessConf of
                                                                          Left _    -> gridThickness defaultSettings
                                                                          Right val -> val,
                                                        aiPresent = case aiPresentConf of
                                                                      Left _    -> aiPresent defaultSettings
                                                                      Right val -> val,
                                                        aiPath = case aiPathConf of
                                                                   Left _    -> aiPath defaultSettings
                                                                   Right val -> val,
                                                        aiRespondent = case aiRespondentConf of
                                                                         Left _    -> aiRespondent defaultSettings
                                                                         Right val -> val,
                                                        aiGenMoveType = case aiGenMoveTypeConf of
                                                                          Left _    -> aiGenMoveType defaultSettings
                                                                          Right val -> val }

writeSettings :: Settings -> String -> IO ()
writeSettings settings fileName = let cfgStr = forceEither $ do let cfg = emptyCP
                                                                cfg1  <- add_section cfg "Game"
                                                                cfg2  <- add_section cfg1 "Players"
                                                                cfg3  <- add_section cfg2 "AI"
                                                                cfg4  <- add_section cfg3 "Other"
                                                                cfg5  <- setshow cfg4 "Game" "game_width" (gameWidth settings)
                                                                cfg6  <- setshow cfg5 "Game" "game_height" (gameHeight settings)
                                                                cfg7  <- set cfg6 "Game" "game_name" (gameName settings)
                                                                cfg8  <- set cfg7 "Players" "red_name" (redName settings)
                                                                cfg9  <- set cfg8 "Players" "black_name" (blackName settings)
                                                                cfg10 <- set cfg9 "Players" "red_color" (sRGB24show $ uncurryRGB sRGB $ redColor settings)
                                                                cfg11 <- set cfg10 "Players" "black_color" (sRGB24show $ uncurryRGB sRGB $ blackColor settings)
                                                                cfg12 <- set cfg11 "Other" "background_color" (sRGB24show $ uncurryRGB sRGB $ backgroundColor settings)
                                                                cfg13 <- set cfg12 "Other" "grid_color" (sRGB24show $ uncurryRGB sRGB $ gridColor settings)
                                                                cfg14 <- setshow cfg13 "Other" "filling_alpha" (fillingAlpha settings)
                                                                cfg15 <- setshow cfg14 "Other" "point_radius" (pointRadius settings)
                                                                cfg16 <- setshow cfg15 "Other" "horizontal_reflection" (horizontalReflection settings)
                                                                cfg17 <- setshow cfg16 "Other" "vertical_reflection" (verticalReflection settings)
                                                                cfg18 <- setshow cfg17 "Other" "full_fill" (fullFill settings)
                                                                cfg19 <- setshow cfg18 "Other" "grid_thickness" (gridThickness settings)
                                                                cfg20 <- setshow cfg19 "AI" "ai_present" (aiPresent settings)
                                                                cfg21 <- set cfg20 "AI" "ai_path" (aiPath settings)
                                                                cfg22 <- setshow cfg21 "AI" "ai_respondent" (aiRespondent settings)
                                                                cfg23 <- setshow cfg22 "AI" "ai_gen_move_type" (aiGenMoveType settings)
                                                                return $ to_string cfg23
                                  in writeFile fileName cfgStr