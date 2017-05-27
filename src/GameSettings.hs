module GameSettings where

import Data.Default

data GenMoveType = Simple | WithTime Int | WithComplexity Int
  deriving (Eq, Show, Read)

instance Default GenMoveType where
  def = Simple

data BeginPattern = Empty | Cross
  deriving (Eq, Show, Read)

instance Default BeginPattern where
  def = Cross

data GameSettings = GameSettings { gsWidth :: Int
                                 , gsHeight :: Int
                                 , gsGenMoveType :: GenMoveType
                                 , gsBeginPattern :: BeginPattern
                                 , gsRedBotPath :: Maybe String
                                 , gsBlackBotPath :: Maybe String
                                 }

instance Default GameSettings where
  def = GameSettings { gsWidth = 39
                     , gsHeight = 32
                     , gsGenMoveType = def
                     , gsBeginPattern = def
                     , gsRedBotPath = Nothing
                     , gsBlackBotPath = Nothing
                     }
