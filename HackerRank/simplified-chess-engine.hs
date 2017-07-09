-- 2016-10-17
-- simplified chess engine, week of code 24
{-# LANGUAGE BangPatterns #-}
import Data.List
import Control.Monad
import Control.Applicative
import Data.Tuple

data Piece = Piece !Char !Char !Char deriving (Eq)
data Pos = Pos !Char !Char deriving (Show, Eq)
type Board = ([Piece], [Piece])
instance Show Piece where show (Piece c i j) = [c,' ',i,j]

pos :: Piece -> Pos
pos (Piece _ i j) = Pos i j

setPos :: Pos -> Piece -> Piece
setPos (Pos i j) (Piece c _ _) = Piece c i j

valid :: Pos -> Bool
valid (Pos i j) = i >= 'A' && i <= 'D' && j >= '1' && j <= '4'

moves :: Board -> [Board]
moves (ws, bs) = concatMap f ws
  where
  f w = concatMap go (paths w)
    where
    go [] = []
    go (p:ps) = case (at p ws, at p bs) of
      (Nothing, Nothing) -> (w' : ws', bs) : go ps
      (Nothing, Just b) -> [(w' : ws', delete b bs)]
      _ -> []
      where
      w' = setPos p w
    ws' = delete w ws
  at p = find ((== p) . pos)

paths :: Piece -> [[Pos]]
paths (Piece c i j) = case c of
  'N' -> knightPaths
  'R' -> rookPaths
  'B' -> bishopPaths
  'Q' -> rookPaths ++ bishopPaths
  where
  rookPaths   = map walk [[id, succ], [id, pred], [succ, id], [pred, id]]
  bishopPaths = map walk . replicateM 2 $ [succ, pred]
  knightPaths = map walk [ [a f, b g]
                         | [f, g] <- replicateM 2 [succ, pred]
                         , [a, b] <- permutations [id, \x -> x.x] ]
  walk [f, g] = takeWhile valid . iterate (\(Pos x y) -> Pos (f x) (g y)) $ Pos (f i) (g j)

main = do
  g <- read <$> getLine
  replicateM_ g $ do
    [w, b, m] <- map read . words <$> getLine
    ws <- replicateM w (f <$> getLine)
    bs <- replicateM b (f <$> getLine)
    let m' = if odd m then m else pred m
    putStrLn $ if minMax True m' (ws, bs) then "YES" else "NO"
  where f [c,' ',i,' ',j] = Piece c i j

minMax :: Bool -> Int -> Board -> Bool
minMax white m b
  | gone = False
  | m == 0 = not white
  | canCaptureQueen b = True
  | otherwise = or . map (not . minMax (not white) (m - 1) . swap) $ moves b
  where gone = all (\(Piece c _ _) -> c /= 'Q') (fst b)

canCaptureQueen :: Board -> Bool
canCaptureQueen (ws, bs) = or $ map f ws
  where
  f w = or $ map go (paths w)
  go [] = False
  go (p:ps) = case (at p ws, at p bs) of
    (Nothing, Nothing) -> go ps
    (Nothing, Just (Piece 'Q' _ _)) -> True
    _ -> False
  at p = find ((== p) . pos)
