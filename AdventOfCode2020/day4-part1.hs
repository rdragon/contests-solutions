import Data.List
  
main = do  
    contents <- getContents
    let ss = getPassports contents
    print (length . filter id . map isValid $ ss)

getPassports :: String -> [String]
getPassports [] = [[]]
getPassports ('\n':'\n':s) = ([] : getPassports s)
getPassports (c:s) = let ss = getPassports s in ((c : head ss) : tail ss)

isValid :: String -> Bool
isValid s = (== 7) . length . filter (/= "cid") . map (takeWhile (/= ':')) $ words s