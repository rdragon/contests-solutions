-- 2016-09-26
-- recording episodes, world codesprint 7
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
import Control.Monad.ST
import Control.Monad
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Data.STRef
import Data.Bool

data E = E { eI :: Int, eS :: Int, eE :: Int}

instance Show E where show (E i s e) = show (i, s, e)

main :: IO ()
main = do
  rd <- intReader
  [q] <- rd 1
  replicateM_ q $ do
  [n] <- rd 1
  es <- fmap (V.fromList . map (\(i, [s, e]) -> E i s e) . zip [0..]) $ replicateM (n * 2) $ rd 2
  putStrLn $ compute n $ V.generate (2 * n) (\i -> let e = es V.! i in [e1 | e1 <- V.toList es, hit e e1, eN e /= eN e1])

compute :: Int -> V.Vector [E] -> String
compute n g = let Just (a, b) = findIv 0 1 in show (succ a) ++ " " ++ show (a + b)
  where
  findIv a b
    | a + b > n = Nothing
    | checkIv a b = Just $ fromMaybe (a, b) (findIv a (succ b))
    | otherwise = findIv (succ a) b

  checkIv a b = runST $ M.replicate n False >>= runCheckIv a b
    
  runCheckIv :: forall s. Int -> Int -> M.MVector s Bool -> ST s Bool
  runCheckIv a b v = checkFrom a
    where
    checkFrom :: Int -> ST s Bool
    checkFrom c
      | c == a + b = return True
      | otherwise = handle c >> liftM2 (&&) (M.read v c) (checkFrom (succ c))

    handle c = do
      x <- M.read v c
      when (not x) $ fmap catMaybes (sequence $ map (try c) [0..1]) >>= handle'
    
    handle' [] = return ()
    handle' (xs:_) = mapM_ (\c -> M.write v c True) xs

    try :: Int -> Int -> ST s (Maybe [Int])
    try r m = do
      w <- M.replicate n Nothing
      u <- newSTRef [(r, m)]
      try' w u

    try' w u = do
      loop
      xs <- readSTRef u
      if null xs then fmap (Just . map fst . catMaybes . zipWith fmap (map (,) [0..]) . V.toList) (V.freeze w) else return Nothing
      where
      loop = fmap null (readSTRef u) >>= bool loop' (return ())
      
      loop' = do
        xs <- readSTRef u
        writeSTRef u (tail xs)
        (uncurry add $ head xs) >>= bool (writeSTRef u xs) loop

      add c m = M.read v c >>= bool (add' c m) (return True)
      
      add' c m = do
        p_mb <- M.read w c
        when (isNothing p_mb) $ do
          M.write w c (Just m)
          let pairs = map (\e -> (eN e, 1 - (eI e `mod` 2)))
                    $ filter (\e -> eN e >= a && eN e < a + b) $ g V.! (2 * c + m)
          modifySTRef u (pairs ++)
        return (isNothing p_mb || p_mb == Just m)

eN :: E -> Int
eN = (`quot` 2) . eI

hit :: E -> E -> Bool
hit (E _ s1 e1) (E _ s2 e2) = not (e2 < s1 || s2 > e1)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)