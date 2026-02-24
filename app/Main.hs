import qualified Data.Map.Lazy as Map
import qualified Data.Array as A
import System.Environment (getArgs)

-- Load entropy balues into a Map
loadEntropy :: FilePath -> IO (Map.Map String Double)
loadEntropy path = do
    contents <- readFile path
    let parseLine l = case words l of
            [w, e] -> Just (w, read e :: Double)
            _ -> Nothing
    return $ Map.fromList [p | Just p <- map parseLine (lines contents)]

-- Find all prefixes of a given string that exist in the entropy dictionary
prefixes :: Map.Map String Double -> String -> [(String, Double)]
prefixes dict str = aux 1
  where
    aux len
        |len > length str = []
        |otherwise =
            let prefix = take len str
                (_, exact, gt) = Map.splitLookup prefix dict
                rest = aux (len + 1)
                -- Check for potential longer prefixes
                hasMore = case Map.lookupMin gt of
                    Just (k, _) -> prefix `isPrefix` k
                    Nothing -> False
                withThis = case exact of
                    Just e -> (prefix, e) : rest
                    Nothing -> rest
            in if exact == Nothing && not hasMore
                then [] -- No longer prefixes possible -> stop
                else withThis

-- Check if a string if a valid previx of another string
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- Performs the segmentation DP algorithm 
segment :: Map.Map String Double -> String -> (Double, [String])
segment dict msg = (entropyArr A.! 0, buildWords 0)
  where
    n = length msg

    -- Mutual recursion: assocs feeds entropyArr and wordArr,
    -- assocs reads from entropyArr.
    assocs :: [(Int, (Double, String))]
    assocs =
        [(k, best)
        |k <- [0 .. n - 1]
        , let suffix = drop k msg
              candidates = prefixes dict suffix
              choices = [(e + entropyArr A.! (k + length w), w)
                        | (w, e) <- candidates
                        ]
              best = case choices of
                  [] -> (100.0, [msg !! k]) --No prefix -> Use 1 character w/ entropy penalty
                  _  -> minimum choices
        ]

    entropyArr :: A.Array Int Double
    entropyArr = A.array (0, n) $
        (n, 0.0) : [(k, fst v) | (k, v) <- assocs]

    wordArr :: A.Array Int String
    wordArr = A.array (0, n - 1) $
        [(k, snd v) | (k, v) <- assocs]

    buildWords :: Int -> [String]
    buildWords k
        |k >= n = []
        |otherwise = let w = wordArr A.! k
                      in w : buildWords (k + length w)

-- Wraps text at input length
wrapText :: Int -> String -> String
wrapText _ [] = []
wrapText width s
    |length s <= width = s
    |otherwise = take width s ++ "\n" ++ wrapText width (drop width s)

main :: IO ()
main = do
    args <- getArgs
    dict <- loadEntropy "entropy.txt"
    case args of
        []    -> do
            input <- getContents
            let segmented = unlines [wrapText 60 (unwords (snd (segment dict (strip l))))
                                    |l <- lines input
                                    , not (null (strip l))
                                    ]
            putStr segmented
        wrds -> do
            let segmented = unlines [wrapText 60 (unwords (snd (segment dict w))) | w <- wrds]
            putStr segmented
  where
    --Remove excess leading/trailing spaces
    strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')