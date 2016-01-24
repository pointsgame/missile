module Field where

import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Auxiliary
import Player

--TODO: объединение тректорий захвата.

type Pos = (Int, Int)

n :: Pos -> Pos
n (x, y) = (x, y + 1)

s :: Pos -> Pos
s (x, y) = (x, y - 1)

w :: Pos -> Pos
w (x, y) = (x - 1, y)

e :: Pos -> Pos
e (x, y) = (x + 1, y)

nw :: Pos -> Pos
nw (x, y) = (x - 1, y + 1)

ne :: Pos -> Pos
ne (x, y) = (x + 1, y + 1)

sw :: Pos -> Pos
sw (x, y) = (x - 1, y - 1)

se :: Pos -> Pos
se (x, y) = (x + 1, y - 1)

data Point = EmptyPoint |
             PlayerPoint Player |
             BasePoint Player Bool |
             EmptyBasePoint Player
  deriving (Eq, Show, Read)

data Field = Field { scoreRed :: Int,
                     scoreBlack :: Int,
                     moves :: [(Pos, Player)],
                     lastSurroundChain :: Maybe ([Pos], Player),
                     points :: Array Pos Point }

isInField :: Field -> Pos -> Bool
isInField = inRange . bounds . points

isPuttingAllowed :: Field -> Pos -> Bool
isPuttingAllowed field pos | not $ isInField field pos = False
                           | otherwise =
  case points field ! pos of
    EmptyPoint       -> True
    EmptyBasePoint _ -> True
    _                -> False

isPlayer :: Field -> Pos -> Player -> Bool
isPlayer field pos player | not $ isInField field pos = False
                          | otherwise =
  case points field ! pos of
    PlayerPoint player' -> player' == player
    BasePoint player' _ -> player' == player
    _                   -> False

isPlayersPoint :: Field -> Pos -> Player -> Bool
isPlayersPoint field pos player | not $ isInField field pos = False
                                | otherwise = points field ! pos == PlayerPoint player

isCapturedPoint :: Field -> Pos -> Player -> Bool
isCapturedPoint field pos player | not $ isInField field pos = False
                                 | otherwise = points field ! pos == BasePoint (nextPlayer player) True

isEmptyBase :: Field -> Pos -> Player -> Bool
isEmptyBase field pos player | not $ isInField field pos = False
                             | otherwise = points field ! pos == EmptyBasePoint player

wave :: Field -> Pos -> (Pos -> Bool) -> S.Set Pos
wave field startPos f = wave' S.empty (S.singleton startPos)
  where wave' passed front | S.null front = passed
                           | otherwise = wave' (S.union passed front) (nextFront passed front)
        nextFront passed front = S.filter f $ S.fromList (filter (isInField field) $ concatMap neighborhood $ S.elems front) S.\\ passed
        neighborhood pos = [n pos, s pos, w pos, e pos]

emptyField :: Int -> Int -> Field
emptyField width height = Field { scoreRed = 0,
                                  scoreBlack = 0,
                                  moves = [],
                                  lastSurroundChain = Nothing,
                                  points = listArray ((0, 0), (width - 1, height - 1)) (repeat EmptyPoint) }

getFirstNextPos :: Pos -> Pos -> Pos
getFirstNextPos centerPos pos =
  let dx = fst pos - fst centerPos
      dy = snd pos - snd centerPos
  in case (dx, dy) of
       (-1, -1) -> se centerPos
       ( 0, -1) -> ne centerPos
       ( 1, -1) -> ne centerPos
       (-1,  0) -> se centerPos
       ( 0,  0) -> se centerPos
       ( 1,  0) -> nw centerPos
       (-1,  1) -> sw centerPos
       ( 0,  1) -> sw centerPos
       ( 1,  1) -> nw centerPos
       _        -> error ("getFirstNextPos: not adjacent points: " ++ show centerPos ++ " and " ++ show pos ++ ".")

getNextPos :: Pos -> Pos -> Pos
getNextPos centerPos pos =
  let dx = fst pos - fst centerPos
      dy = snd pos - snd centerPos
  in case (dx, dy) of
       (-1, -1) -> e pos
       ( 0, -1) -> e pos
       ( 1, -1) -> n pos
       (-1,  0) -> s pos
       ( 0,  0) -> s pos
       ( 1,  0) -> n pos
       (-1,  1) -> s pos
       ( 0,  1) -> w pos
       ( 1,  1) -> w pos
       _        -> error ("getNextPos: not adjacent points: " ++ show centerPos ++ " and " ++ show pos ++ ".")

square :: [Pos] -> Int
square chain = square' chain 0
  where square' [a] acc = acc + fiberBundle a (head chain)
        square' (h:t) acc = square' t (acc + fiberBundle h (head t))
        square' _ _ = error "square: bug."
        fiberBundle (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

buildChain :: Field -> Pos -> Pos -> Player -> Maybe [Pos]
buildChain field startPos nextPos player = if length chain > 2 && square chain > 0 then Just chain else Nothing
  where chain = getChain startPos [nextPos, startPos]
        getChain start list@(h:_) = let nextPos' = getNextPlayerPos h (getFirstNextPos h start)
                                    in if | nextPos' == startPos -> list
                                          | elem nextPos' list   -> getChain h $ dropWhile (/= nextPos') list
                                          | otherwise            -> getChain h $ nextPos' : list
        getChain _ _ = error "buildChain: bug."
        getNextPlayerPos centerPos pos | pos == startPos = pos
                                       | isPlayer field pos player = pos
                                       | otherwise = getNextPlayerPos centerPos (getNextPos centerPos pos)

getInputPoints :: Field -> Pos -> Player -> [(Pos, Pos)]
getInputPoints field pos player =
  let list1 = if not $ isPlayer field (w pos) player then
                if | isPlayer field (sw pos) player -> [(sw pos, w pos)]
                   | isPlayer field (s pos) player  -> [(s pos, w pos)]
                   | otherwise                          -> []
              else
                []
      list2 = if not $ isPlayer field (n pos) player then
                if | isPlayer field (nw pos) player -> (nw pos, n pos) : list1
                   | isPlayer field (w pos) player  -> (w pos, n pos) : list1
                   | otherwise                          -> list1
              else
                list1
      list3 = if not $ isPlayer field (e pos) player then
                if | isPlayer field (ne pos) player -> (ne pos, e pos) : list2
                   | isPlayer field (n pos) player  -> (n pos, e pos) : list2
                   | otherwise                          -> list2
              else
                list2
      list4 = if not $ isPlayer field (s pos) player then
                if | isPlayer field (se pos) player -> (se pos, s pos) : list3
                   | isPlayer field (e pos) player  -> (e pos, s pos) : list3
                   | otherwise                          -> list3
              else
                list3
  in list4

posInsideRing :: Pos -> [Pos] -> Bool
posInsideRing (x, y) ring =
  let ring' = removeNearSame $ map snd $ filter ((<= x) . fst) ring
      ring'' | last ring' == y = ring' ++ [head $ if head ring' == y then tail ring' else ring']
             | head ring' == y = last ring' : ring'
             | otherwise       = ring'
  in odd $ count (\(a, b, c) -> b == y && ((a < b && c > b) || (a > b && c < b))) $ zip3 ring'' (tail ring'') (tail $ tail ring'')

getInsideRing :: Field -> Pos -> [Pos] -> S.Set Pos
getInsideRing field startPos ring =
  let ringSet = S.fromList ring
  in wave field startPos $ flip S.notMember ringSet

getEmptyBase :: Field -> Pos -> Player -> ([Pos], [Pos])
getEmptyBase field startPos player = (emptyBaseChain, filter (\pos -> isEmptyBase field pos player) $ S.elems $ getInsideRing field startPos emptyBaseChain)
  where emptyBaseChain = getEmptyBaseChain (w startPos)
        getEmptyBaseChain pos | not $ isPlayer field pos player = getEmptyBaseChain (w pos)
                              | otherwise = let inputPoints = getInputPoints field pos player
                                                chains = mapMaybe (\(chainPos, _) -> buildChain field pos chainPos player) inputPoints
                                                result = find (posInsideRing startPos) chains
                                            in fromMaybe (getEmptyBaseChain (w pos)) result

capture :: Point -> Player -> Point
capture point player =
  case point of
    EmptyPoint                                  -> BasePoint player False
    PlayerPoint player' | player' == player     -> PlayerPoint player'
                        | otherwise             -> BasePoint player True
    BasePoint player' enemy | player' == player -> BasePoint player' enemy
                            | enemy             -> PlayerPoint player
                            | otherwise         -> BasePoint player False
    EmptyBasePoint _                            -> BasePoint player False

putPoint :: Pos -> Player -> Field -> Field
putPoint pos player field | not (isPuttingAllowed field pos) = error "putPos: putting in the pos is not allowed."
                          | otherwise = let enemyPlayer = nextPlayer player
                                            point = points field ! pos
                                            (enemyEmptyBaseChain, enemyEmptyBase) = getEmptyBase field pos enemyPlayer
                                            inputPoints = getInputPoints field pos player
                                            captures = mapMaybe (\(chainPos, capturedPos) ->
                                              do chain <- buildChain field pos chainPos player
                                                 let captured = S.elems $ getInsideRing field capturedPos chain
                                                     capturedCount' = count (\pos' -> isPlayersPoint field pos' enemyPlayer) captured
                                                     freedCount' = count (\pos' -> isCapturedPoint field pos' player) captured
                                                 return (chain, captured, capturedCount', freedCount')) inputPoints
                                            (realCaptures, emptyCaptures) = partition ((/= 0) . thd'') captures
                                            capturedCount = sum $ map thd'' realCaptures
                                            freedCount = sum $ map fth'' realCaptures
                                            newEmptyBase = concatMap snd'' emptyCaptures
                                            realCaptured = concatMap snd'' realCaptures
                                            captureChain = concatMap (reverse . fst'') realCaptures
                                            newScoreRed = if player == Red then scoreRed field + capturedCount else scoreRed field - freedCount
                                            newScoreBlack = if player == Black then scoreBlack field + capturedCount else scoreBlack field - freedCount
                                            newMoves = (pos, player) : moves field
                                        in if point == EmptyBasePoint enemyPlayer
                                           then if not $ null captures
                                                then Field { scoreRed = newScoreRed,
                                                             scoreBlack = newScoreBlack,
                                                             moves = newMoves,
                                                             lastSurroundChain = Just (captureChain, player),
                                                             points = points field // (zip enemyEmptyBase (repeat EmptyPoint) ++
                                                                                       (pos, PlayerPoint player) :
                                                                                       map (\pos' -> (pos', capture (points field ! pos') player)) realCaptured) }
                                                else Field { scoreRed = if player == Red then scoreRed field else scoreRed field + 1,
                                                             scoreBlack = if player == Black then scoreBlack field else scoreBlack field + 1,
                                                             moves = newMoves,
                                                             lastSurroundChain = Just (enemyEmptyBaseChain, enemyPlayer),
                                                             points = points field // (zip enemyEmptyBase (repeat $ BasePoint enemyPlayer False) ++
                                                                                       [(pos, BasePoint enemyPlayer True)]) }
                                           else if point == EmptyBasePoint player
                                           then field { moves = newMoves,
                                                        lastSurroundChain = Nothing,
                                                        points = points field // [(pos, PlayerPoint player)] }
                                           else Field { scoreRed = newScoreRed,
                                                        scoreBlack = newScoreBlack,
                                                        moves = newMoves,
                                                        lastSurroundChain = if null captureChain then Nothing else Just (captureChain, player),
                                                        points = points field // ((pos, PlayerPoint player) :
                                                                                  zip newEmptyBase (repeat $ EmptyBasePoint player) ++
                                                                                  map (\pos' -> (pos', capture (points field ! pos') player)) realCaptured) }

fieldWidth :: Field -> Int
fieldWidth field =
  let ((x1, _), (x2, _)) = bounds (points field)
  in x2 - x1 + 1

fieldHeight :: Field -> Int
fieldHeight field =
  let ((_, y1), (_, y2)) = bounds (points field)
  in y2 - y1 + 1
