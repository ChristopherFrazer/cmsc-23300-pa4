import qualified Data.Map.Strict as Map
import Data.Char (isAscii)
import Data.List (intercalate)
import System.IO

type WordCounts = Map.Map String Integer

processLine :: WordCounts -> String -> WordCounts
processLine acc line =
    case parseLineWords line of
        Nothing -> acc
        Just pairs -> foldl (\m (w, c) -> Map.insertWith (+) w c m) acc pairs

parseLineWords :: String -> Maybe [(String, Integer)]
parseLineWords line =
    case words line of
        [word, countStr] ->
            case reads countStr :: [(Integer, String)] of
                [(count, "")] ->
                    -- Only keep words composed entirely of ASCII characters
                    if all isAscii word
                        then let -- Remove apostrophes
                                noApostrophes = filter (/= '\'') word
                                subwords = splitOn '-' noApostrophes
                                -- Filter out empty strings from splitting
                                filtered = filter (not . null) subwords
                             in if null filtered
                                then Nothing
                                else Just [(w, count) | w <- filtered]
                        else Nothing
                _ -> Nothing
        _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim s = go s []
  where
    go [] acc = [reverse acc]
    go (c:cs) acc
        |c == delim = reverse acc : go cs []
        |otherwise  = go cs (c : acc)

main :: IO ()
main = do
    contents <- readFile "enwiki-2023-04-13.txt"
    let rawLines = lines contents
        -- Parse and pre-process each line
        merged = foldl processLine Map.empty rawLines
        
        totalCount = fromIntegral (sum (Map.elems merged)) :: Double
        -- Filter words with >= 50 occurrences and compute entropy
        results = [(word, entropy)
                  |(word, count) <- Map.toAscList merged
                  , count >= 50
                  , let p = fromIntegral count / totalCount
                        entropy = -logBase 2 p
                  ]
    writeFile "entropy.txt" (unlines [word ++ " " ++ show ent | (word, ent) <- results])
