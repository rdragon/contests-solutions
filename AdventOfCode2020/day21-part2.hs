import Data.Maybe
import Data.List
import qualified Data.Set as S

type Ingredient = String
type Allergen = String
type Food = (S.Set Ingredient, [Allergen])
type Hit = (Ingredient, Allergen)

main = do
    ps <- (map f . lines) <$> getContents
    putStrLn . intercalate "," . map fst . sortOn snd $ compute ps
    where f :: String -> Food
          f s = let (t, (_:u)) = span (/= '(') s in (S.fromList $ words t, tail . words . filter (/= ',') $ init u)

compute :: [Food] -> [Hit]
compute ps = case listToMaybe . catMaybes . map f . nub $ concatMap snd ps of
    Just x -> (x : (compute $ map (g x) ps))
    _ -> []
    where f :: Allergen -> Maybe Hit
          f a = case S.toList . foldl1' S.intersection . map fst $ filter ((a `elem`) . snd) ps of
              [i] -> Just (i, a)
              _ -> Nothing
          g :: Hit -> Food -> Food
          g (i, a) (is, as) = (S.delete i is, delete a as)