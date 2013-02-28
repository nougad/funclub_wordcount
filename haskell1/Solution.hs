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

-- replaces every non alphanumeric character with space and any uppercase character with lowercase
cleanupWord :: String -> String
cleanupWord = map (replaceNonAlphanum . toLower)
  where replaceNonAlphanum x
          | isAlphaNum x = x
          | otherwise = ' '


-- counts for every word the word frequency
doMap :: [String] -> [(String, Int)]
doMap x
  | null x        = []
  | length x == 1 = [(head x,1)]
  | length x == 2 = let fstx = head x
                        sndx = head $ tail x in
                    -- sort words alphabetically to speed up reduce
                    if fstx < sndx then [(fstx,1), (sndx,1)] else [(sndx,1), (fstx,1)]
  | otherwise = recursiveMap $ splitAt (length x `quot` 2) x -- every doMap gets the half list

    where
          recursiveMap :: ([String],[String]) -> [(String,Int)]
          recursiveMap (a,b) = reduce (doMap a) (doMap b)

          -- gets two list of (word,count) which are sorted alphabetically by word
          -- merges the two list into once
          reduce :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
          reduce [] b = b -- if one list is empty use the other one
          reduce a [] = a
          reduce (a@(aStr,aCnt):aRest) (b@(bStr,bCnt):bRest)
            | aStr < bStr  = (aStr,aCnt)      : reduce aRest (b:bRest) -- if one word is less than the other
            | aStr > bStr  = (bStr,bCnt)      : reduce (a:aRest) bRest --   continue with the rest
            | aStr == bStr = (aStr,aCnt+bCnt) : reduce aRest bRest     -- if words are equal - add counts


-- gets a list of words and filter the tenth best based on count
-- result is sorted based on count
filterTenBest :: [(String,Int)] -> [(String,Int)]
filterTenBest = foldl filterTenBest' []
  where
    -- filterTenBestFirstRun iterates over the items and fills a list of top 10 words
    filterTenBest' :: [(String,Int)] -> (String,Int) -> [(String,Int)]
    filterTenBest' tenBestSoFar y@(_word,count)
      | length tenBestSoFar <  10                                    = insert  tenBestSoFar y
      | length tenBestSoFar >= 10 && snd (head tenBestSoFar) < count = replace tenBestSoFar y
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
main = joinLinesIntoString <$> map showResult <$> reverse <$> filterTenBest <$> doMap <$> splitIntoWords <$> cleanupWord <$> getContents >>= putStr

--------------------------------------------------------------------------------------------------------------------------
-- profiling result:
--
--        Wed Feb 27 00:08 2013 Time and Allocation Profiling Report  (Final)
--
--           Solution +RTS -p -RTS
--
--        total time  =        1.15 secs   (1148 ticks @ 1000 us, 1 processor)
--        total alloc = 461,520,708 bytes  (excludes profiling overheads)
--
--COST CENTRE                    MODULE  %time %alloc
--
--doMap.reduce                   Main     45.4   17.1
--main                           Main     19.8   43.1
--doMap                          Main     15.2   26.3
--cleanupWord                    Main     14.1   11.9
--cleanupWord.replaceNonAlphanum Main      4.4    1.7
--
--
--                                                                              individual     inherited
--COST CENTRE                       MODULE                    no.     entries  %time %alloc   %time %alloc
--
--MAIN                              MAIN                       45           0    0.0    0.0   100.0  100.0
-- main                             Main                       91           0   19.8   43.1   100.0  100.0
--  showResult                      Main                      107          10    0.0    0.0     0.0    0.0
--  cleanupWord                     Main                       97           0   14.1   11.9    18.6   13.5
--   cleanupWord.replaceNonAlphanum Main                       98     1243538    4.4    1.7     4.4    1.7
--  doMap                           Main                       94      262143   15.2   26.3    61.5   43.4
--   doMap.fstx                     Main                      101       87454    0.0    0.0     0.0    0.0
--   doMap.sndx                     Main                      100       87454    0.5    0.0     0.5    0.0
--   doMap.recursiveMap             Main                       99      131071    0.3    0.0    45.7   17.1
--    doMap.reduce                  Main                      102     1875557   45.4   17.1    45.4   17.1
--  filterTenBest                   Main                       93           1    0.0    0.0     0.2    0.0
--   filterTenBest.filterTenBest'   Main                      103       17280    0.2    0.0     0.2    0.0
--    filterTenBest.replace         Main                      105          66    0.0    0.0     0.0    0.0
--     filterTenBest.insert         Main                      106         349    0.0    0.0     0.0    0.0
--    filterTenBest.insert          Main                      104          23    0.0    0.0     0.0    0.0
-- CAF                              Main                       89           0    0.0    0.0     0.0    0.0
--  cleanupWord                     Main                       96           1    0.0    0.0     0.0    0.0
--  splitIntoWords                  Main                       95           1    0.0    0.0     0.0    0.0
--  joinLinesIntoString             Main                       92           1    0.0    0.0     0.0    0.0
--  main                            Main                       90           1    0.0    0.0     0.0    0.0

