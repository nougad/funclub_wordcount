--------------------------------------------------------------------------------------------------------------------------
-- compile:
-- $ ghc -prof -auto-all -O2 -Wall Solution.hs # -prof -auto-all is for profiling
-- run:
-- $ time ./Solution +RTS -p < moby-dic.txt    # +RTS -p is for profiling
-- profiling:
-- $ cat Solution.prof                         # see bottom for results
--------------------------------------------------------------------------------------------------------------------------

module Main where

import Control.Applicative ((<$>))
import Data.Char           (toLower,isAlphaNum)

import qualified Data.Map as Map

-- replaces every non alphanumeric character with space and any uppercase character with lowercase
cleanupWord :: String -> String
cleanupWord = map (replaceNonAlphanum . toLower)
  where replaceNonAlphanum x
          | isAlphaNum x = x
          | otherwise = ' '

fillMap :: [String] -> Map.Map String Int
fillMap = foldl insertOrReplace Map.empty
  where insertOrReplace :: Map.Map String Int -> String -> Map.Map String Int
        insertOrReplace m k = case Map.lookup k m of
                                Just i  -> Map.insert k (i+1) m
                                Nothing -> Map.insert k 1 m

-- gets a list of words and filter the tenth best based on count
-- result is sorted based on count
filterTenBest :: Map.Map String Int -> [(String,Int)]
filterTenBest = Map.foldlWithKey filterTenBest' []
  where
    -- filterTenBestFirstRun iterates over the items and fills a list of top 10 words
    filterTenBest' :: [(String,Int)] -> String -> Int -> [(String,Int)]
    filterTenBest' tenBestSoFar word count
      | length tenBestSoFar <  10                                    = insert  tenBestSoFar (word,count)
      | length tenBestSoFar >= 10 && snd (head tenBestSoFar) < count = replace tenBestSoFar (word,count)
      | otherwise                                                    = tenBestSoFar

    -- replace gets a sorted list of counts and a item
    -- The item is inserted (in correct order) and the lowest item is removed
    -- the length of the list stays the same and list stays sorted
    replace :: [(String,Int)] -> (String,Int) -> [(String,Int)]
    replace [] a = [a]
    replace (y@(_wordY,countY):ys) a@(_wordA,countA)
      | countY < countA = insert ys a
      | otherwise       = y:ys

    -- insert gets a sorted list of counts and a item
    -- in inserts the item at the correct position (list stays sorted)
    insert :: [(String,Int)] -> (String,Int) -> [(String,Int)]
    insert []     a = [a]
    insert (y@(_wordY,countY):ys) a@(_wordA,countA)
      | countY <  countA = y:insert ys a
      | countY >= countA = a:y:ys


-- converts an (word,count) item into a String
showResult :: (String,Int) -> String
showResult (str,cnt) = str ++ ": " ++ show cnt

-- splits string at spaces
splitIntoWords :: String -> [String]
splitIntoWords = words

-- joins elements with newline
joinLinesIntoString :: [String] -> String
joinLinesIntoString = unlines

main :: IO ()
main = joinLinesIntoString <$> map showResult <$> reverse <$> filterTenBest <$> fillMap <$> splitIntoWords <$> cleanupWord <$> getContents >>= putStr


--------------------------------------------------------------------------------------------------------------------------
-- profiling result:
--
--        Wed Feb 27 00:16 2013 Time and Allocation Profiling Report  (Final)
--
--           Solution +RTS -p -RTS
--
--        total time  =        0.98 secs   (982 ticks @ 1000 us, 1 processor)
--        total alloc = 320,183,396 bytes  (excludes profiling overheads)
--
--COST CENTRE                    MODULE  %time %alloc
--
--fillMap.insertOrReplace        Main     49.1   18.5
--main                           Main     26.0   62.1
--cleanupWord                    Main     17.6   17.1
--cleanupWord.replaceNonAlphanum Main      6.8    2.4
--
--
--                                                                            individual     inherited
--COST CENTRE                       MODULE                  no.     entries  %time %alloc   %time %alloc
--
--MAIN                              MAIN                     46           0    0.0    0.0   100.0  100.0
-- main                             Main                     93           0   26.0   62.1   100.0  100.0
--  showResult                      Main                    106          10    0.0    0.0     0.0    0.0
--  cleanupWord                     Main                     99           0   17.6   17.1    24.4   19.5
--   cleanupWord.replaceNonAlphanum Main                    100     1243538    6.8    2.4     6.8    2.4
--  fillMap                         Main                     96           1    0.4    0.0    49.5   18.5
--   fillMap.insertOrReplace        Main                    101      218526   49.1   18.5    49.1   18.5
--  filterTenBest                   Main                     95           1    0.0    0.0     0.1    0.0
--   filterTenBest.filterTenBest'   Main                    102       17191    0.1    0.0     0.1    0.0
--    filterTenBest.replace         Main                    104          67    0.0    0.0     0.0    0.0
--     filterTenBest.insert         Main                    105         354    0.0    0.0     0.0    0.0
--    filterTenBest.insert          Main                    103          20    0.0    0.0     0.0    0.0
-- CAF                              Main                     91           0    0.0    0.0     0.0    0.0
--  cleanupWord                     Main                     98           1    0.0    0.0     0.0    0.0
--  splitIntoWords                  Main                     97           1    0.0    0.0     0.0    0.0
--  joinLinesIntoString             Main                     94           1    0.0    0.0     0.0    0.0
--  main                            Main                     92           1    0.0    0.0     0.0    0.0

