module Hist
    -- The type itself
    (Histogram, 
    -- Creation, Normalization, and Combination
    hist, histNormalize, histCombine,
    -- Serialization & Deserialization
    histToJSON, histFromJSON,
    getHistOfFile, getHistOfAllFiles,

    histGetWords) where

import Data.List (group, sort)
import Data.Char (toLower)
import Data.Map hiding (filter)
import System.IO
import System.Directory

type Histogram = Map String Float

hist :: String -> Histogram 
hist = histNormalize . fromList . count . group . histGetWords 

histNormalize :: Histogram -> Histogram
histNormalize hist = let factor = 1.0 / (sum $ elems hist) in
    Data.Map.map (* factor) hist

count :: [[String]] -> [(String, Float)]
count strs = let n = fromIntegral $ length strs in
    Prelude.map (\x -> (head x, (fromIntegral $ length x) / n)) strs

histGetWords :: String -> [String]
histGetWords = sort . words . sanitize

sanitize :: String -> String
sanitize = filterPunc . Prelude.map (toLower)

filterPunc :: String -> String 
filterPunc str = filter (`notElem` punc) str
    where punc = "`~!@#$%^&*()))_+-=[]{}\\|;:\"\',.<>/?\r\n"

histCombine :: Histogram -> Histogram -> Histogram
histCombine h1 h2 = unionWith (+) h1 h2

histToJSON :: Histogram -> String 
histToJSON hist = "{\n" 
    <> foldMap jsonifyPair (assocs hist)
    <> "}"

histFromJSON :: String -> Histogram
histFromJSON json = h
    where j' = filter (`notElem` "{}\"\t\n ") json
          j'' = filter (/="") $ splitByDelim ',' j'
          j3 = Prelude.map (splitByDelim ':') j''
          h = fromList $ Prelude.map (\x -> (head x, read $ head $ tail x ::Float)) j3

jsonifyPair :: (String, Float) -> String
jsonifyPair pair = (\(x, y) -> "\t" ++ show x ++ " : " ++ show y ++ ",\n") pair

splitByDelim :: Char -> String -> [String]
splitByDelim delim str = case break (==delim) str of
        (a, delim:b) -> a : splitByDelim delim b
        (a, "") -> [a]

getHistOfAllFiles :: FilePath -> IO Histogram
getHistOfAllFiles dir = do files <- getDirectoryContents dir 
                           setCurrentDirectory dir
                           hists <- mapM getHistOfFile (filter notRootOrHidden files)
                           setCurrentDirectory ".."
                           let combined = Prelude.foldr histCombine (fromList []) hists
                           return combined
                           where notRootOrHidden = (\x -> head x /= '.')

getHistOfFile :: FilePath -> IO Histogram
getHistOfFile filepath = do input <- readFile filepath
                            return (hist input)

