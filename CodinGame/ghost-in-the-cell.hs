-- 2017-07-06
-- ghost-in-the-cell
-- compiled with GHC 8.0.2
-- league: wood 3 - bronze
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.IntMap.Strict as Map
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.IO

-- Abbreviations
----------------
-- c count
-- d distance
-- f factory
-- fr from
-- ix index
-- s string
-- t troop
-- v vector

type Army = (Int, Side)
type D = Int
type Ds = U.Vector D
type FIx = Int
type Fs = Map.IntMap F
type Ix = Int
type Prod = Int
type Side = Int
type Ts = [T]

data F = F { fArmy :: Army, fProd :: Prod } deriving (Show)
data Grid = Grid Ds Fs Ts
data Move = Move { mFr :: FIx, mTo :: FIx, mSize :: Int }
data T = T { tFIx :: FIx, tArmy :: Army, tD :: D } deriving (Show)

instance Show Move where
  show (Move fr to size) = "MOVE " ++ unwords (map show [fr, to, size])

-----------------------------------------------------------------------------------

run :: Ds -> IO ()
run ds = do
  c <- fmap read getLine
  (fs, ts) <- fmap readEntities $ replicateM c getLine
  let grid = Grid ds fs ts
  let attacks = sortBy (compare `on` moveD ds) $ findAttacks grid
  putStrLn $ maybe "WAIT" show $ listToMaybe attacks
  run ds

findAttacks :: Grid -> [Move]
findAttacks (Grid ds fs ts) =
  [ Move fr to c_me
  | fr <- [0 .. Map.size fs - 1]
  , let F (c_fr, side_fr) prod_fr = fsF fs fr
  , side_fr == 1
  , to <- [0 .. Map.size fs - 1]
  , let d = dsD ds fr to
  , 1 <= d
  , let f_to@(F _ prod_to) = fsF fs to
  , 1 <= prod_to
  , let (c_he, side_he) = calcArmy f_to (tsFilter ts to) d
  , side_he /= 1
  , let c_me = c_he + 1
  , c_me <= c_fr
  , let f = F (c_fr - c_me, 1) prod_fr
  , let ts1 = tsFilter ts fr
  , let (_, side) = calcArmy f ts1 $ tD $ last ts1
  , null ts1 || side == 1 ]

-----------------------------------------------------------------------------------

calcArmy :: F -> Ts -> D -> Army
calcArmy f ts d = produce prod army
  where (F army prod, _, _) = iterate sim (f, ts, 1) !! d

sim :: (F, Ts, D) -> (F, Ts, D)
sim (F army prod, ts, d) = (F army2 prod, ts2, d + 1)
  where
  (ts1, ts2) = span ((== d). tD) ts
  army1 = produce prod army
  army2 = foldl' fight army1 $ map tArmy ts1

fight :: Army -> Army -> Army
fight (x, p) (y, q)
  | p /= q = if x < y then (y - x, q) else (x - y, p)
  | otherwise = (x + y, p)

produce :: Prod -> Army -> Army
produce _ (x, 0) = (x, 0)
produce x (y, p) = (x + y, p)

tsFilter :: Ts -> FIx -> Ts
tsFilter ts ix = filter ((== ix). tFIx) ts

fsProd :: Fs -> FIx -> Int
fsProd fs = fProd. (fsF fs)

fsF :: Fs -> FIx -> F
fsF fs = fromJust. (`Map.lookup` fs)

maxFs :: Int
maxFs = 15

maxD :: D
maxD = 20

getIx :: FIx -> FIx -> Ix
getIx x y = x + y * maxFs

dsD :: Ds -> FIx -> FIx -> D
dsD ds x y = ds U.! getIx x y

moveD :: Ds -> Move -> D
moveD ds (Move fr to _) = dsD ds fr to

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  _ <- getLine
  d_c <- fmap read getLine
  fmap readDs (replicateM d_c getLine) >>= run

readDs :: [String] -> Ds
readDs input = U.create $ do
  v <- UM.replicate (maxFs * maxFs) 0
  sequence_
    [ UM.write v (getIx x y) z >> UM.write v (getIx y x) z
    | s <- input
    , let [x, y, z] = map read $ words s ]
  return v

readEntities :: [String] -> (Fs, Ts)
readEntities input = (fs,  ts)
  where
  fs = Map.fromList
    [ (ix, F (size, side) prod)
    | s <- input
    , isInfixOf "FACTORY" s
    , let [ix, _, side, size, prod, _, _] = map read $ words s ]
  ts = sortBy (compare `on` tD) $ 
    [ T to (size, side) d
    | s <- input
    , isInfixOf "TROOP" s
    , let [_, _, side, _, to, size, d] = map read $ words s ]
