-- 2016-06-05
import qualified Data.Sequence as S
import qualified Data.Foldable as Fold
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  _ <- getLine
  ls <- fmap B.lines B.getContents
  let v = (S.fromList . map readInt . B.words) (head ls)
  let v1 = foldl' f v (drop 2 ls)
  putStrLn (intercalate " " (fmap show . Fold.toList $ v1))
  
f :: S.Seq Int -> B.ByteString -> S.Seq Int
f v line = b S.>< a S.>< c
  where
    (a1, c) = S.splitAt r v
    (a, b) = S.splitAt (pred l) a1
    [l, r] = map readInt . B.words $ line

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt