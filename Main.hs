module Main where

import Hist
import System.IO
import System.Directory
import Data.Map (lookup)

evalMessage :: String -> (TrainingData, TrainingData) -> Bool
evalMessage msg (hamData, spamData) = if pHam > pSpam then True else False
    where words = histGetWords msg
          pHam = (snd hamData) * (product $ Prelude.map (getHistVal (fst hamData)) words)
          pSpam = (snd spamData) * (product $ Prelude.map (getHistVal (fst spamData)) words)

type TrainingData = (Histogram, Float)

getHistVal :: Histogram -> String -> Float
getHistVal hist str = case Data.Map.lookup str hist of
                             Just v -> v
                             Nothing -> 0.001

train :: FilePath -> FilePath -> IO (TrainingData, TrainingData)
train hamDir spamDir = do
                       hamPriorProb <- getPriorProb hamDir spamDir
                       spamPriorProb <- getPriorProb spamDir hamDir
                       (hamHists, spamHists) <- getTrainingData hamDir spamDir
                       return ((hamHists, hamPriorProb), (spamHists, spamPriorProb))

-- Gets training data for ha and soam data, returning it as a tuple.
-- Ham data is the FIRST element in the tuple, spam is the SECOND
getTrainingData :: FilePath -> FilePath -> IO (Histogram, Histogram)
getTrainingData hamDir spamDir = do
                                 hamHist <- getHistOfAllFiles hamDir
                                 spamHist <- getHistOfAllFiles spamDir
                                 return (hamHist, spamHist)

getPriorProb :: FilePath -> FilePath -> IO Float
getPriorProb target other = do
                            targetC <- numFilesInDir target
                            otherC <- numFilesInDir other
                            return (fromIntegral targetC / (fromIntegral targetC + fromIntegral otherC))

numFilesInDir :: FilePath -> IO Int
numFilesInDir dir = do
                    files <- getDirectoryContents dir
                    return (length $ filter notRootOrHidden files)
                    where notRootOrHidden = (\x -> head x /= '.')
main :: IO ()
main = do
    putStrLn "Test"
