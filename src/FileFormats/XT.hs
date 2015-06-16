module FileFormats.XT where

import Data.Maybe
import qualified Data.ByteString as BS
import Control.Exception
import Data.Encoding
import Data.Encoding.CP1251
import Player
import Field
import Settings
import Game
import Auxiliary
import FileFormats.Common

load :: String -> Settings -> IO (Maybe Game)
load name settings =
  do fileEither <- try (BS.readFile name) :: IO (Either SomeException BS.ByteString)
     case fileEither of
       Left ex  -> do print ex
                      return Nothing
       Right bs -> return $ load' bs settings

load' :: BS.ByteString -> Settings-> Maybe Game
load' byteString settings =
  if length fields == 1
  then Nothing
  else Just Game { curPlayer = nextPlayer $ snd $ head $ moves $ head fields,
                   gameFields = fields,
                   gameTree = toTreeInv fields,
                   gameSettings = newSettings }
    where newSettings = settings { gameWidth = 39,
                                   gameHeight = 32,
                                   redName = trim $ decodeStrictByteString CP1251 $ BS.take 9 $ BS.drop 11 byteString,
                                   blackName = trim $ decodeStrictByteString CP1251 $ BS.take 9 $ BS.drop 20 byteString,
                                   horizontalReflection = False,
                                   verticalReflection = True }
          fields = load'' (BS.drop 58 byteString) [emptyField 39 32]
          load'' bs fields' | BS.length bs < 4 = fields'
                            | otherwise = let pos = (fromIntegral $ BS.head bs, fromIntegral $ BS.index bs 1)
                                              player | BS.index bs 3 == 0x00 = Just Black
                                                     | BS.index bs 3 == 0xFF = Just Red
                                                     | otherwise = Nothing
                                              nextFields = putPoint pos (fromJust player) (head fields') : fields'
                                          in if isPuttingAllowed (head fields') pos && isJust player
                                             then load'' (BS.drop 13 bs) nextFields
                                             else fields'

save :: String -> Game -> IO Bool
save name game =
  let bs = save' game
  in if BS.null bs
     then return False
     else do fileEither <- try (BS.writeFile name bs) :: IO (Either SomeException ())
             case fileEither of
               Left ex -> do print ex
                             return False
               Right _ -> return True

save' :: Game -> BS.ByteString
save' game = if gameWidth settings /= 39 || gameHeight settings /= 32 || gameIsEmpty game
             then BS.empty
             else BS.pack $ reverse $ appendMoves (reverse movesList)
                      (0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       reverse (BS.unpack $ encodeStrictByteString CP1251 $ padRight ' ' 9 $ take 9 $ blackName settings) ++ --Black name
                       reverse (BS.unpack $ encodeStrictByteString CP1251 $ padRight ' ' 9 $ take 9 $ redName settings) ++ --Red name
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       (if snd (head movesList) == Red then 0xFF else 0x00) : (if snd (head movesList) == Red then 0xFF else 0x00) : --Last player.
                       fromIntegral ((length movesList - 1) `div` 256) : fromIntegral ((length movesList - 1) `mod` 256) : --Moves length.
                       121 :  --Version.
                       [])
             where settings = gameSettings game
                   movesList = getMovesList game
                   appendMoves [] str = str
                   appendMoves (((x, y), player):t) str = appendMoves t $
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       0x00 : 0x00 : -- ?
                       (if player == Red then 0xFF else 0x00) : (if player == Red then 0xFF else 0x00) : --Last player.
                       0x01 :
                       fromIntegral y : --y
                       fromIntegral x : --x
                       str