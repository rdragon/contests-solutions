-- 2016-07-08
import Data.Maybe
import Data.List
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

type B = (V.Vector [Int], Tree (V.Vector [Int]))

data Tree a = Tree (Tree a) a (Tree a)

instance Functor Tree where
  fmap f (Tree l a r) = Tree (fmap f l) (f a) (fmap f r)

treeLookup :: Tree a -> Int -> a
treeLookup (Tree _ a _) 0 = a
treeLookup (Tree l _ r) n = treeLookup (if b == 0 then l else r) n' where (n', b) = n `divMod` 2

indexTree :: Tree Int
indexTree = go 0 1 where
  go n m = Tree (go n (m * 2)) n (go (n + m) (m * 2))

makeTree :: V.Vector Int -> Tree (V.Vector [Int])
makeTree as = fmap (calcF as vH) indexTree
  where
  vH = calcH as

md :: Int
md = 1000000007

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  -- door een extra type muntje toe te voegen bereken je direct F(0) + ... + F(N) ipv F(N)
  as <- fmap (V.fromList . reverse . (1 :) . sort) $ rd n
  [l, r] <- rd 2
  print $ solve as l r

solve :: V.Vector Int -> Int -> Int -> Int
solve as l r = (ways as r - ways as (l - 1)) `mod` md

ways :: V.Vector Int -> Int -> Int
ways _ 0 = 1
ways as r
  | r < 0 = 0
  | (length $ nub $ init $ V.toList $ as) == 1 = ways1 as r
  | otherwise = ans
  where
  vH = calcH as
  vF = calcF as vH r
  n = V.length as
  ans = modsum [fromMaybe 0 (listToMaybe (f 0 j)) | j <- [0 .. pred n]]
  f = getF vF n

-- enkel voor als alle munten zelfde waarde hebben
ways1 :: V.Vector Int -> Int -> Int
ways1 as r = prod (foldl' prod 1 [k + i | i <- [1 .. pred n]]) (inv (product [1 .. pred n]))
  where
  k = r `quot` a
  n = V.length as
  a = V.head as

-- we maken veel gebruik van [Int]. in plaats van te berekenen op hoeveel manieren we som r kunnen krijgen (noem x_r), berekenen we telkens meer waarden, namelijk [x_r, x_{r+1}, ..., x_*]. hier staat zo'n [Int] dan voor
-- h r i j geeft dan zo'n lijst, met i de kleinst mogelijke type van een muntje, en j de type van het laatste muntje. de lijst heeft maximaal A_j waarden (want door het laatste muntje mag de som nog hoger worden, als de som daarvoor nog strikt kleiner dan r was)
-- deze functie geeft een paar, met als eerste element een vector met voor kleine waarden van r de lijsten, en als tweede element een boom waarin voor willekeurig grote r een lijst gevonden kan worden, alleen doet muntje type 0 niet mee in de waarden uit de boom (we doen alsof er een muntje minder is)
calcH :: V.Vector Int -> B
calcH as = (w, makeTree (V.tail as))
  where
  n = V.length as
  mx = getMx as
  w = V.generate (mx * n * n) gen
  h = getH (w, undefined) as n
  gen ix
    | i == j = replicate (((r + a - 1) `quot` a) * a - r) 0 ++ [1]
    | otherwise = arSum [if a < r then h (r - a) i j else [], h r (succ i) j]
    where
    (r, t1) = ix `divMod` (n * n)
    (i, j) = t1 `divMod` n
    a = as V.! i

-- hier bereken we opnieuw zo'n lijst, maar nu recursief. we splitsen de som r op in drie, waarvan de eerste twee dezelfde grootte hebben waardoor we daarvoor maar 1 berekening hoeven te doen, en de laatste klein is en we daar gebruik kunnen maken van 'vH'
-- doordat we de grootte van de eerste twee beperken (maximaal grootte 'rMax'), kunnen we gebruik maken van de lijsten die we krijgen. het maakt dan niet uit dat door het laatste muntje de som van beide delen iets groter kan worden, de som zal nooit groter worden dan r (in andere woorden, het derde deel is groot genoeg)
calcF :: V.Vector Int -> B -> Int -> V.Vector [Int]
calcF as vH r
  | 2 * m0 > rMax = altCalcF as vH r
  | otherwise = vF
  where
  n = V.length as
  (m0, m1) = getMs as
  -- door te eindigen met ' - 1' zorgen we ervoor dat er altijd nog een muntje in het derde deel moet zitten (zorgt voor een speciaal geval minder)
  rMax = r - 2 * m1 + 2 - 1
  p = rMax `quot` (2 * m0)
  -- door een m0 veelvoud te gebruiken zal de ruimte die het extra muntje inneemt nooit groter zijn dan m1, want de muntjes van grootte m0 komen altijd vooraan te liggen en zorgen nooit voor extra benodigde ruimte
  r1 = p * m0
  vF1 = calcF as vH r1
  vF = V.generate (n * n) gen
  f1 = getF vF1 n
  h r' = getH vH as n r'
  gen ix = arSum $ [go k l | k <- [i..j], l <- [k..j]]
    where
    (i, j) = ix `divMod` n
    go k l = arSum $ map (\(q, x) -> (map (prod x) $ h (r - 2 * r1 - q) l j)) vals
      where
      -- dit zijn paren met als eerste element de extra benodigde ruimte en als tweede element het aantal manieren waarop we precies deze hoeveelheid extra benodigde ruimte nodig hebben (eigenlijk dus zo'n [Int] lijst, maar dan in een andere vorm)
      vals = filter ((> 0) . snd) $ getVals start
      getVals (_, _, []) = []
      getVals (q, ar1, ar2) = (q, modsum $ zipWith prod ar1 ar2) : getVals (pred q, ar1, tail ar2)
      start = 
        if n1 < n2
        then (n3, ar2, replicate (n2 - 1) 0 ++ reverse ar1)
        else (n3, ar1, replicate (n1 - 1) 0 ++ reverse ar2)
        where
        ar1 = f1 i k
        ar2 = f1 k l
        n1 = length ar1
        n2 = length ar2
        n3 = n1 + n2 - 2 -- de maximaal benodigde extra ruimte

-- hier bereken we zo'n lijst voor lage waarden van r
altCalcF :: V.Vector Int -> B -> Int -> V.Vector [Int]
altCalcF as vH r = vF
  where
  h = getH vH as n
  vF = V.generate (n * n) gen
  gen ix = h r i j
    where
    (i, j) = ix `divMod` n
  n = V.length as

getMs :: V.Vector Int -> (Int, Int)
getMs as = case nub $ V.toList $ as of
  (m0:m1:_) -> (m0, m1)
  _ -> (V.head as, as V.! 1)

getMx :: V.Vector Int -> Int
getMx as = let (m0, m1) = getMs as in min 2000 (2 * (m0 + m1 + 2))

getH :: B -> V.Vector Int -> Int -> Int -> Int -> Int -> [Int]
getH (w, tree) as n r i j = if r < mx then w V.! (r * n * n + i * n + j) else t1
  where
  t1 =
    if j == 0
    then (replicate (((r + m0 - 1) `quot` m0) * m0 - r) 0 ++ [1])
    else arSum [(treeLookup tree r1) V.! ((max 0 (i - 1)) * (n - 1) + (j - 1)) | r1 <- if i == 0 then [r, r - m0 .. 1] else [r]]
  mx = getMx as
  (m0, _) = getMs as

getF :: V.Vector [Int] -> Int -> Int -> Int -> [Int]
getF v n i j = v V.! (i * n + j)

arSum :: [[Int]] -> [Int]
arSum [] = []
arSum (xs1:xss) = foldl' add xs1 xss
  where
  add xs [] = xs
  add [] ys = ys
  add (x:xs) (y:ys) = ((x + y) `mod` md) : add xs ys

prod :: Int -> Int -> Int
prod x y = (x * y) `mod` md

modsum :: [Int] -> Int
modsum xs = foldl' f 0 xs
  where f x y = (x + y) `mod` md

modpow :: Int -> Int -> Int
modpow a b = go 1 b a
  where
  go x 0 _ = x
  go x b1 a1 = go y q (prod a1 a1)
    where
    (q, r) = b1 `divMod` 2
    y = if r == 0 then x else prod x a1

inv :: Int -> Int
inv x = modpow x (md - 2)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)

--main :: IO ()
--main = do
  --test [1] 10 10
  --test [1, 1] 10 65
  --test [1, 1, 1] 100 176850
  --test [2, 2] 11 20
  --test [1, 1, 1, 1, 1, 1, 1, 1, 1, 1] 1000000000000000 208697697 -- not sure
  --test [1, 2, 3] 6 22
  --test [2, 3] 6 6
  --test [1, 2] 1 1
  --test [1, 2] 2 3
  --test [1, 2] 3 5
  --test [1, 3] 3 4
  --test [13, 6] 19 5
  --test [13, 6] 37 13
  --test [13, 6] 85554 46930756
  --test [58364, 1, 1, 1, 1, 1, 1, 1, 1, 1] 8151797111733526 20409002
  --where
  --test as r expe = if (ans /= expe)
  --    then putStrLn $ "error: got " ++ show ans ++ " but expected " ++ show expe
  --    else putStrLn $ "success: " ++ show ans
  --  where
  --  ans = solve (V.fromList $ reverse $ sort $ (1 :) $ as) 1 r
