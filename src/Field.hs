module Field where

import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Auxiliary
import Player

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

type Point = Maybe Player

data Field = Field { scoreRed :: Int,
                     scoreBlack :: Int,
                     moves :: [(Pos, Player)],
                     lastSurroundChain :: ([Pos], Player),
                     points :: Array Pos Point,
                     redEmptyBasePoints :: S.Set Pos,
                     blackEmptyBasePoints :: S.Set Pos }

inField :: Field -> Pos -> Bool
inField field pos = inRange (bounds (points field)) pos

puttingAllow :: Field -> Pos -> Bool
puttingAllow field pos | not $ inField field pos = False
                       | (points field) ! pos /= Nothing = False
                       | otherwise = True

playersPoint :: Field -> Pos -> Player -> Bool
playersPoint field pos player | not $ inField field pos = False
                              | (points field) ! pos /= Just player = False
                              | otherwise = True

wave :: Field -> Pos -> (Pos -> Bool) -> [Pos]
wave field startPos f = wave' S.empty (S.singleton startPos)
  where wave' passed front | S.null front = S.elems passed
                           | otherwise = wave' (S.union passed front) (nextFront passed front)
        nextFront passed front = S.filter f $ (S.fromList $ filter (inField field) $ concatMap neighborhood $ S.elems front) S.\\ passed
        neighborhood pos = [n pos, s pos, w pos, e pos]

{--
wave :: Field -> Pos -> (Pos -> Point -> Bool) -> [Pos]
wave field startPos f = wave' startPos [] where
                         wave' pos list | not $ inField field pos = list
                                        | elem pos list = list
                                        | not (f pos ((points field) ! pos)) = list
                                        | otherwise = let list1 = pos : list
                                                          list2 = wave' (e pos) list1
                                                          list3 = wave' (n pos) list2
                                                          list4 = wave' (w pos) list3
                                                          list5 = wave' (s pos) list4
                                                      in list5
--}

emptyField :: Int -> Int -> Field
emptyField width height = Field { scoreRed = 0,
                                  scoreBlack = 0,
                                  moves = [],
                                  lastSurroundChain = ([], Black),
                                  points = listArray ((0, 0), (width - 1, height - 1)) (repeat Nothing),
                                  redEmptyBasePoints = S.empty,
                                  blackEmptyBasePoints = S.empty }

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
                                          | elem nextPos' list   -> getChain h (cut list nextPos')
                                          | otherwise            -> getChain h (nextPos' : list)
        getChain _ _ = error "buildChain: bug."
        getNextPlayerPos centerPos pos | pos == startPos = pos
                                       | playersPoint field pos player = pos
                                       | otherwise = getNextPlayerPos centerPos (getNextPos centerPos pos)
        cut list@(h:t) pos | h == pos = list
                           | otherwise = cut t pos
        cut _ _ = error "buildChain: bug."

getEmptyBaseColor :: Field -> Pos -> Maybe Player
getEmptyBaseColor field pos | S.member pos (redEmptyBasePoints field) = Just Red
                            | S.member pos (blackEmptyBasePoints field) = Just Black
                            | otherwise = Nothing

getInputPoints :: Field -> Pos -> Player -> [(Pos, Pos)]
getInputPoints field pos player =
  let list1 = if not $ playersPoint field (w pos) player then
                if | playersPoint field (sw pos) player -> [(sw pos, w pos)]
                   | playersPoint field (s pos) player  -> [(s pos, w pos)]
                   | otherwise                          -> []
              else
                []
      list2 = if not $ playersPoint field (n pos) player then
                if | playersPoint field (nw pos) player -> (nw pos, n pos) : list1
                   | playersPoint field (w pos) player  -> (w pos, n pos) : list1
                   | otherwise                          -> list1
              else
                list1
      list3 = if not $ playersPoint field (e pos) player then
                if | playersPoint field (ne pos) player -> (ne pos, e pos) : list2
                   | playersPoint field (n pos) player  -> (n pos, e pos) : list2
                   | otherwise                          -> list2
              else
                list2
      list4 = if not $ playersPoint field (s pos) player then
                if | playersPoint field (se pos) player -> (se pos, s pos) : list3
                   | playersPoint field (e pos) player  -> (e pos, s pos) : list3
                   | otherwise                          -> list3
              else
                list3
  in list4

posInsideRing :: Pos -> [Pos] -> Bool
posInsideRing (x, y) ring =
  let ring' = removeNearSame $ map snd $ filter ((<= x) . fst) ring
      ring'' | last ring' == y = ring' ++ [if head ring' == y then head $ tail ring' else head ring']
             | head ring' == y = last ring' : ring'
             | otherwise       = ring'
  in odd $ count (\(a, b, c) -> b == y && ((a < b && c > b) || (a > b && c < b))) $ zip3 ring'' (tail ring'') (tail $ tail ring'')

getInsideRing :: Field -> Pos -> [Pos] -> [Pos]
getInsideRing field startPos ring =
  let ringSet = S.fromList ring
  in wave field startPos $ flip S.notMember ringSet

getEmptyBase :: Field -> Pos -> Player -> ([Pos], [Pos])
getEmptyBase field startPos player = (emptyBaseChain, filter (isNothing . (points field !)) $ getInsideRing field startPos emptyBaseChain)
  where emptyBaseChain = getEmptyBaseChain (w startPos)
        getEmptyBaseChain pos | not $ playersPoint field pos player = getEmptyBaseChain (w pos)
                              | otherwise = let inputPoints = getInputPoints field pos player
                                                chains = catMaybes (map (\(chainPos, _) -> buildChain field pos chainPos player) inputPoints)
                                                result = find (posInsideRing startPos) chains
                                            in case result of
                                                 Just chain -> chain
                                                 Nothing    -> getEmptyBaseChain (w pos)

fst' :: (a1, a2, a3) -> a1
fst' (a, _, _) = a
snd' :: (a1, a2, a3) -> a2
snd' (_, a, _) = a
thd' :: (a1, a2, a3) -> a3
thd' (_, _, a) = a

putPoint :: Pos -> Player -> Field -> Field
putPoint pos player field | not (puttingAllow field pos) = error "putPos: putting in the pos is not allowed."
                          | otherwise = let enemyPlayer = nextPlayer player
                                            inEnemyEmptyBase = (player == Black && S.member pos (redEmptyBasePoints field)) || (player == Red && S.member pos (blackEmptyBasePoints field))
                                            inCurEmptyBase = (player == Red && S.member pos (redEmptyBasePoints field)) || (player == Black && S.member pos (blackEmptyBasePoints field))
                                            (enemyEmptyBaseChain, enemyEmptyBase) = getEmptyBase field pos (nextPlayer player)
                                            inputPoints = getInputPoints field pos player
                                            captures = catMaybes $ map (\(chainPos, capturedPos) ->
                                              do chain <- buildChain field pos chainPos player
                                                 let captured = getInsideRing field capturedPos chain
                                                     capturedCount = count (\pos' -> playersPoint field pos' enemyPlayer) captured
                                                 return (chain, captured, capturedCount)) inputPoints
                                            headCaptures = head captures
                                            deltaScore = sum $ map thd' captures
                                            newEmptyBase = concat $ map snd' $ filter ((== 0) . thd') captures
                                            realCaptured = concat $ map snd' $ filter ((/= 0) . thd') captures
                                            captureChain = concat $ map (reverse . fst') $ filter ((/= 0) . thd') captures
                                        in if inEnemyEmptyBase
                                           then if not $ null captures
                                                then Field { scoreRed = if player == Red then scoreRed field + thd' headCaptures else scoreRed field,
                                                             scoreBlack = if player == Black then scoreBlack field + thd' headCaptures else scoreBlack field,
                                                             moves = (pos, player) : moves field,
                                                             lastSurroundChain = (fst' headCaptures, player),
                                                             points = points field // ((pos, Just player) : zip (snd' headCaptures) (repeat (Just player))),
                                                             redEmptyBasePoints = if player == Red then redEmptyBasePoints field else redEmptyBasePoints field S.\\ S.fromList enemyEmptyBase,
                                                             blackEmptyBasePoints = if player == Black then blackEmptyBasePoints field else blackEmptyBasePoints field S.\\ S.fromList enemyEmptyBase }
                                                else field { scoreRed = if player == Red then scoreRed field - 1 else scoreRed field,
                                                             scoreBlack = if player == Black then scoreBlack field - 1 else scoreBlack field,
                                                             moves = (pos, player) : moves field,
                                                             lastSurroundChain = (enemyEmptyBaseChain, enemyPlayer),
                                                             points = points field // zip enemyEmptyBase (repeat (Just enemyPlayer)) }
                                           else if inCurEmptyBase
                                           then field { moves = (pos, player) : moves field,
                                                        lastSurroundChain = ([], player),
                                                        points = points field // [(pos, Just player)] }
                                           else Field { scoreRed = if player == Red then scoreRed field + deltaScore else scoreRed field,
                                                        scoreBlack = if player == Black then scoreBlack field + deltaScore else scoreBlack field,
                                                        moves = (pos, player) : moves field,
                                                        lastSurroundChain = (captureChain, player),
                                                        points = points field // ((pos, Just player) : zip realCaptured (repeat (Just player))),
                                                        redEmptyBasePoints = if player == Red then redEmptyBasePoints field `S.union` S.fromList newEmptyBase else redEmptyBasePoints field,
                                                        blackEmptyBasePoints = if player == Black then blackEmptyBasePoints field `S.union` S.fromList newEmptyBase else blackEmptyBasePoints field } --fix

fieldWidth :: Field -> Int
fieldWidth field =
  let ((x1, _), (x2, _)) = bounds (points field)
  in x2 - x1 + 1

fieldHeight :: Field -> Int
fieldHeight field =
  let ((_, y1), (_, y2)) = bounds (points field)
  in y2 - y1 + 1
