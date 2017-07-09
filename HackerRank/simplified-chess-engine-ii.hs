-- 2016-12-23
-- simplified-chess-engine-ii, week of code 27
import Data.List
import Control.Monad
import Data.Maybe

data Piece = Piece !Char !Char !Char deriving (Eq)
data Loc = Loc !Char !Char deriving (Eq)
type Board = ([Piece], [Piece])
type Flipped = Bool -- if True then pawns move in the opposite direction
type Move = (Maybe Piece, Board) -- (maybe captured piece, resulting board)

loc :: Piece -> Loc
loc (Piece _ j i) = Loc j i

setLoc :: Loc -> Piece -> Piece
setLoc (Loc j i) (Piece c _ _) = Piece c j i

valid :: Loc -> Bool
valid (Loc j i) = j >= 'A' && j <= 'D' && i >= '1' && i <= '4'

at :: [Piece] -> Loc -> Maybe Piece
at ps l = find ((== l). loc) ps

-- returns all possible moves for white
boardMoves :: Board -> Flipped -> [Move]
boardMoves board@(ws, _) flipped = concatMap (pieceMoves board flipped) ws

-- returns all possible moves for one piece
pieceMoves :: Board -> Flipped -> Piece -> [Move]
pieceMoves (ws, bs) flipped w@(Piece c j i) = case c of
  'N' -> knightMoves
  'R' -> rookMoves
  'B' -> bishopMoves
  'Q' -> rookMoves ++ bishopMoves
  'P' -> pawnMoves
  where
  bishopMoves = concatMap mkMoves. replicateM 2 $ [succ, pred]
  rookMoves   = concatMap mkMoves [[id, succ], [id, pred], [succ, id], [pred, id]]
  knightMoves = concatMap mkMoves [ [a f, b g]
                                  | [f, g] <- replicateM 2 [succ, pred]
                                  , [a, b] <- permutations [id, \x -> x.x] ]
  pawnMoves = moves1 ++ moves2
    where 
    i' = if flipped then pred i else succ i
    moves1 = if isNothing (at (ws ++ bs) (Loc j i')) then mkPawnMoves (Loc j i') Nothing else []
    moves2 = concatMap f [Loc (pred j) i', Loc (succ j) i']
    f l = if valid l && isNothing (at ws l) && isJust bMb then mkPawnMoves l bMb else []
      where bMb = at bs l
  mkMoves [f, g] = go. iterate (\(Loc x y) -> Loc (f x) (g y)) $ Loc (f j) (g i)
    where
    go (l:ls)
      | valid l = case (at ws l, at bs l) of
        (Nothing, Nothing) -> (mkMove (setLoc l w) Nothing) : go ls
        (Nothing, x) -> [mkMove (setLoc l w) x]
        _ -> []
      | otherwise = []
  mkMove :: Piece -> Maybe Piece -> Move
  mkMove w' Nothing = (Nothing, (bs, w':ws'))
  mkMove w' (Just b) = (Just b, (delete b bs, w':ws'))
  mkPawnMoves :: Loc -> Maybe Piece -> [Move]
  mkPawnMoves (Loc x y) bMb
    | y == (if flipped then '1' else '4') = [mkMove (Piece c' x y) bMb | c' <- "RBN"]
    | otherwise = [mkMove (Piece c x y) bMb]
  ws' = delete w ws

-- if flipped is False, returns whether white can force a win in m moves (m >= 1)
-- if flipped is True, returns whether white can prevent black from winning in m moves
minMax :: Int -> Flipped -> Board -> Bool
minMax m flipped board
  | any capturesQueen moves = True
  | m == 1 = flipped
  | m == 2 && not flipped = False
  | otherwise = or $ map (not. minMax (m - 1) (not flipped). snd) moves
  where 
  moves = boardMoves board flipped
  capturesQueen (Just (Piece 'Q' _ _), _) = True
  capturesQueen _ = False

main :: IO ()
main = do
  g <- fmap read getLine
  replicateM_ g $ do
    [w, b, m] <- fmap (map read. words) getLine
    ws <- replicateM w (fmap f getLine)
    bs <- replicateM b (fmap f getLine)
    putStrLn $ if minMax m False (ws, bs) then "YES" else "NO"
  where f [c,' ',j,' ',i] = Piece c j i