module Day15 where

import Data.List (minimumBy, partition, sortBy)
import qualified Data.Map.Strict as S
import qualified Data.Matrix as M
import Debug.Trace
import Fabs.LazyStdin (runOnInput)

data Tile
  = Goblin
  | Elf
  | Wall
  | Open
  | Gap
  | Number Int
  deriving (Eq)

instance Show Tile where
  show Goblin = "G"
  show Elf = "E"
  show Open = "."
  show Wall = "#"
  show Gap = " "
  show (Number x) = show x

otherRace :: Tile -> Tile
otherRace Goblin = Elf
otherRace Elf = Goblin
otherRace x = error $ show x ++ ": is not a race"

data Unit = Unit
  { race :: Tile
  , attack :: Int
  , life :: Int
  } deriving (Eq, Show)

data PUnit = PUnit
  { pos :: (Int, Int)
  , unit :: Unit
  } deriving (Eq, Show)

data Game = Game
  { units :: S.Map (Int, Int) Unit
  , board :: M.Matrix Tile
  , score :: Int
  , turn :: Int
  , over :: Bool
  } deriving (Eq, Show)

data Move
  = Move (Int, Int)
  | Stay
  deriving (Show)

aElf :: Unit
aElf = Unit Elf 3 200

aGoblin :: Unit
aGoblin = Unit Goblin 3 200

hit :: Unit -> Unit -> Maybe Unit
hit (Unit _ atk _) (Unit r' a' l')
  | l' - atk > 0 = Just $ Unit r' a' (l' - atk)
  | otherwise = Nothing

star1 :: Int -> IO ()
star1 e = print . (format e) . head . reverse =<< runOnInput runGame
  where
    format e (Game u b s t o) =
      M.prettyMatrix leMatrix ++ show (s, t, s * t, e - s * t)
      where
        rows = M.nrows b
        cols = M.ncols b
        leMatrix = M.matrix rows (2 * cols + 2) paint
        paint (i, j)
          | j == cols + 1 || j == cols + 2 = Gap
          | j > cols + 2 =
            case (S.!?) u (i, j - cols - 2) of
              Just x -> Number (life x)
              Nothing -> Open
          | otherwise = M.getElem i j b

runGame :: [String] -> [Game]
runGame ls = turns $ Game initEnemies initBoard 0 0 False
  where
    initEnemies = foldr makeUnit S.empty (elems initBoard)
    makeUnit (i, j, Elf) s = S.insert (i, j) aElf s
    makeUnit (i, j, Goblin) s = S.insert (i, j) aGoblin s
    makeUnit _ s = s
    initBoard = M.fromLists $ (fmap . fmap) tileMap ls
    tileMap 'G' = Goblin
    tileMap 'E' = Elf
    tileMap '#' = Wall
    tileMap '.' = Open

turns :: Game -> [Game]
turns g@(Game _ _ _ _ True) = [g]
turns g = g : (turns $ next g)

next :: Game -> Game
next g = turn' . foldr play g $ (unitList g)
  where
    play :: ((Int, Int), Unit) -> Game -> Game
    play i = fight . pick . walk . search i
    turn' (Game u b s t o) = Game u b s (t + 1) (gameOver u t)

unitList :: Game -> [((Int, Int), Unit)]
unitList (Game uns _ _ _ _) = ordered
  where
    ordered = sortBy reading $ S.toList uns

search :: ((Int, Int), Unit) -> Game -> (Game, (Int, Int), Unit, Move)
search (p, u) g
  | length (enemies g (otherRace . race $ u) p) /= 0 = (g, p, u, Stay)
  | otherwise = (g, p, u, bestMove)
  where
    bestMove = bfs (otherRace (race u)) (board g) [[p]] (S.singleton p True)

walk :: (Game, (Int, Int), Unit, Move) -> (Game, (Int, Int), Unit)
walk (g, p, u, Stay) = (g, p, u)
walk (g, p, u, (Move np)) = mgame g p np u

mgame :: Game -> (Int, Int) -> (Int, Int) -> Unit -> (Game, (Int, Int), Unit)
mgame g p np u = (ng, np, u)
  where
    ng = Game movedUnits movedBoard (score g) (turn g) (over g)
    movedUnits = S.insert np u $ S.delete p (units g)
    movedBoard = M.setElem (race u) np $ M.setElem Open p (board g)

pick :: (Game, (Int, Int), Unit) -> (Game, PUnit, Maybe PUnit)
pick (g, p, u)
  | null (enemies g (otherRace (race u)) p) = (g, PUnit p u, Nothing)
  | otherwise = (g, PUnit p u, Just enemy)
  where
    enemy = minBy . enemies g (otherRace (race u)) $ p
      where
        minBy = minimumBy conserve
          where
            conserve a b =
              case compare (life . unit $ a) (life . unit $ b) of
                LT -> LT
                GT -> GT
                EQ ->
                  case compare (fst . pos $ a) (fst . pos $ b) of
                    EQ -> compare (snd . pos $ a) (snd . pos $ b)
                    cp -> cp

fight :: (Game, PUnit, Maybe PUnit) -> Game
fight (g, _, Nothing) = g
fight (g, atk, Just def) = ng
  where
    ng = Game nUnits nBoard nScore (turn g) over'
    nScore = calculateScore nUnits
    over' = gameOver nUnits (turn g)
    ndef = hit (unit atk) (unit def)
    nUnits =
      case ndef of
        Nothing -> S.delete (pos def) (units g)
        Just u -> S.insert (pos def) u (units g)
    nBoard =
      case ndef of
        Nothing -> M.setElem Open (pos def) (board g)
        _ -> board g

calculateScore :: S.Map (Int, Int) Unit -> Int
calculateScore s = sum . filter (>= 0) $ life . snd <$> S.toList s

gameOver :: S.Map (Int, Int) Unit -> Int -> Bool
gameOver s t = left == 0 || right == 0
  where
    (ls, rs) = partition ((== Elf) . race . snd) $ S.toList s
    left = length ls
    right = length rs

dist1 :: Int -> Int -> [(Int, Int)]
dist1 i' j' = [(i' - 1, j'), (i', j' - 1), (i', j' + 1), (i' + 1, j')]

enemies :: Game -> Tile -> (Int, Int) -> [PUnit]
enemies g o (i, j) = es
  where
    es = fmap asUnits . filter enemy . fmap tiles $ arround
    asUnits (_, p) = PUnit p ((S.!) (units g) p)
    enemy (t, _) = t == o
    tiles p'@(i', j') = (M.getElem i' j' (board g), p')
    arround = dist1 i j

elems :: M.Matrix a -> [(Int, Int, a)]
elems m =
  [(i, j, M.getElem i j m) | i <- [1 .. (M.nrows m)], j <- [1 .. (M.ncols m)]]

insideMatrix :: M.Matrix a -> (Int, Int) -> Bool
insideMatrix m (i, j) = i >= 1 && j >= 1 && j <= M.ncols m && i <= M.nrows m

bfs :: Tile -> M.Matrix Tile -> [[(Int, Int)]] -> S.Map (Int, Int) Bool -> Move
bfs _ _ [] _ = Stay
bfs o b (p':ps) vs
  | uncurry M.getElem (head p') b == o =
    Move .
    head .
    sortBy reading2 .
    fmap (\l -> head . drop (length l - 2) $ l) .
    filter ((== length p') . length) $
    (p' : ps)
  | otherwise = bfs o b (ps ++ paths) (joinSet vs nextStep)
  where
    nextStep = pms o b vs (head p')
    paths = fmap (\step -> step : p') nextStep
    pms o b vs (i', j') = filter (possibles o b vs) (dist1 i' j')
    possibles o' b' vs' (i', j') =
      insideMatrix b' (i', j') &&
      (not $ S.member (i', j') vs') &&
      (M.getElem i' j' b' == o' || M.getElem i' j' b' == Open)
    joinSet = foldr (\k m -> S.insert k True m)

reading :: ((Int, Int), Unit) -> ((Int, Int), Unit) -> Ordering
reading ((i, j), _) ((i', j'), _) =
  case compare i i' of
    EQ -> compare j j'
    cp -> cp

reading2 :: (Int, Int) -> (Int, Int) -> Ordering
reading2 (i, j) (i', j') =
  case compare i i' of
    EQ -> compare j j'
    cp -> cp
