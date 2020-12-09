import Data.List
import Data.Maybe
import Data.Char
import Text.Read
  
main = do  
    contents <- getContents
    print (length . filter id . map isValidPassport $ getPassports contents)

getPassports :: String -> [String]
getPassports [] = [[]]
getPassports ('\n':'\n':s) = ([] : getPassports s)
getPassports (c:s) = let ss = getPassports s in ((c : head ss) : tail ss)

isValidPassport :: String -> Bool
isValidPassport s = (== 7) . length . filter isValidField . map getField $ words s

getField :: String -> (String, String)
getField s = let (t, u) = span (/= ':') s in (t, tail u)

isValidField :: (String, String) -> Bool
isValidField ("byr", s) = maybe False (\x -> x >= 1920 && x <= 2002) (readMaybe s :: Maybe Int)
isValidField ("iyr", s) = maybe False (\x -> x >= 2010 && x <= 2020) (readMaybe s :: Maybe Int)
isValidField ("eyr", s) = maybe False (\x -> x >= 2020 && x <= 2030) (readMaybe s :: Maybe Int)
isValidField ("hgt", s) = isHeight s
isValidField ("hcl", ('#':s)) = length s == 6 && all isHexDigit s
isValidField ("ecl", s) = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidField ("pid", s) = length s == 9 && all isDigit s
isValidField _ = False

isHeight :: String -> Bool
isHeight s
    | isNothing height = False
    | t == "cm" = h >= 150 && h <= 193
    | t == "in" = h >= 59 && h <= 76
    | otherwise = False
    where
        t = reverse . take 2 $ reverse s
        u = reverse . drop 2 $ reverse s
        height = (readMaybe u :: Maybe Int)
        Just h = height