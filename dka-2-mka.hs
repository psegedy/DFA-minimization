-- File: dka-2-mka.hs
-- Author: Patrik Segedy <xseged00@vutbr.cz>
-- Description: DFA minimization

module Main where

import           Data.List
import           Data.List.Split
import Data.Function
-- import Data.Ord
import           System.Environment
import Debug.Trace

-- Data structure for transitions of finite automata
data Transition = Transition    { start  :: String
                                , dest   :: String
                                , symbol :: String
                                } deriving (Show, Eq)

-- Define deterministic finite automata
data DFA = DFA  { states      :: [String]
                , alphabet    :: [Char]
                , transitions :: [Transition]
                , sState      :: String
                , fStates     :: [String]
                } deriving (Show)

-- Make comma separated string from list of strings
commaSeparated :: [String] -> String
commaSeparated = intercalate ","

-- Create list of ints from list of strings
stoi :: [String] -> [Int]
stoi = map read

-- Sort list by descending order
sortDesc = sortBy (flip compare)

-- True if second element (transition symbol) of tuple is not empty
sndNotEmpty x
    | snd x == "" = False
    | otherwise   = True

-- second elem in tuple (transitions symbol) is longer than 1
-- split it to multiple tuples
multipleTransitions x = concat $ map (zip (fst x)) (chunksOf 1 (snd x))

-- Make DFA well defined - add sink state
wellDefined dfa
    | (concat $ map (missingSymbols dfa) (states dfa)) == "" = dfa   -- dfa is well defined
    | otherwise = dfa { states = (statesWithSink dfa)
                      , transitions = (transitions dfa) ++ (missingTransitions dfa)
                      } -- add sink state

-- get missing transitions in [Transition] format
missingTransitions dfa = map (\x -> Transition [(fst x)] (show $ newStateNum dfa) [(snd x)]) missing
    where
        -- [("1",""),("2","a"),("3","ab")]
        zipped = zip (statesWithSink dfa) $ map (missingSymbols dfa) (statesWithSink dfa)
        -- [('2','a'),('3','a'),('3','b')]
        missing = concat $ map (multipleTransitions) (filter (sndNotEmpty) zipped)

-- get next free state - e.g. use it as sink state
newStateNum dfa = succ $ head $ sortDesc $ stoi (states dfa)

-- add sink state to states
statesWithSink dfa = insert (show $ newStateNum dfa) (states dfa)

-- get missing symbols for state
-- DFA, state, missing symbols
missingSymbols :: DFA -> String -> String
missingSymbols dfa state = alphabet dfa \\ concat [x !! 1 | x <- (map (\t -> [start t, symbol t]) (transitions dfa)), x !! 0 == state]

-- Minimize finite automata using k-distinguishability

--(zip [1..] . sort . map sort $ s).
-- First split to two groups, final and non-final states
kDistingInit dfa = ((states dfa) \\ (fStates dfa)) : (fStates dfa) : []



-- NOTE: skompilovalo ale nebude to dobre...
-- NOTE2: dest samostatne pre kazdy symbol, nie spolu...
-- kDisting dfa partitions
-- kDisting dfa [] = []
-- kDisting dfa [[]] = []
-- -- kDisting dfa (x:xs) = eqPartition dfa x : (kDisting dfa xs)
-- kDisting dfa (x:xs)
--     | eqPartition dfa x = x : (kDisting dfa xs)
--     | otherwise = kDisting dfa xs

-- eqPartition dfa [[]] = False
-- eqPartition dfa [] = False
-- eqPartition dfa (x:xs)
--     | (map (getDsts dfa x \\ [x]) (chunksOf 1 (alphabet dfa))) == [] = True
--     | otherwise = eqPartition dfa xs


-- TODO: sort kDists before eqPartitions
-- map sort []

minStates dfa = zip [1..] . sort . map sort $ getKDist dfa (states dfa) (getKDist' dfa)
minStartStates dfa = show . head $ map (\x -> fst x) $ filter (\x -> snd x == True) $ map (\x -> (fst x, (sState dfa) `elem` snd x)) (minStates dfa)

minFinStates dfa [] = []
minFinStates dfa (y:ys) = (show . head $ map (\x -> fst x) $ filter (\x -> snd x == True) $ map (\x -> (fst x, y `elem` snd x)) (minStates dfa)) : minFinStates dfa ys

minimize dfa = DFA {
    states = map (show . fst) (minStates dfa),
    alphabet = alphabet dfa,
    transitions = transitions dfa,
    sState = minStartStates dfa,
    fStates = nub $ minFinStates dfa (fStates dfa)
}

-- https://stackoverflow.com/questions/2307893/sorting-lists-of-lists-in-haskell
sortByLength list = concat (groupBy ((==) `on` length) $ sortBy (compare `on` length) list)

getKDist dfa states [] = []
getKDist dfa states (x:xs)
    | length states > 0 = (intersect x states) : getKDist dfa (states \\ x) xs
    | otherwise = getKDist dfa x []

getKDist' dfa = sortByLength $ nub $ concat $ map (kDisting dfa (kDistingInit dfa) (kDistingInit dfa)) (chunksOf 1 (alphabet dfa))


kDisting dfa [] kDists sym = []
kDisting dfa (x:xs) kDists sym
    | eqPartitions dfa x sym (map sort kDists) = x : kDisting dfa xs kDists sym
    | otherwise = kDisting dfa (xs ++ splitGroup) (kDists ++ splitGroup) sym
    where 
        -- splitGroup = nub $ map (nub . getSrcs dfa sym x) (getDsts dfa x sym)
        commonGroup [] part = []
        commonGroup (y:ys) part
            | y `elem` part = (getSrcs dfa sym part y) : commonGroup ys part
            | otherwise = commonGroup ys part
        splitGroup = concat $ nub $ map (commonGroup (getDsts dfa x sym)) (reverse kDists)

-- commonGroup [] part = []
-- commonGroup (x:xs) part
--     | x `elem` part = x : commonGroup xs part
--     | otherwise = commonGroup xs part

-- checks that all states of partition lead to same k-distinguishable group
eqPartitions dfa part sym kDists = any (==True) (map (eqPartition (getDsts dfa part sym)) kDists)

-- checks if states belongs to partition 
-- states, eq-partition
eqPartition [] _ = False
eqPartition states@(x:xs) part
    | (all (==True) (eqPart states)) = True
    | otherwise = False
    where
        eqPart [] = []
        eqPart (x:xs) = (x `elem` part) : (eqPart xs)

-- get destination state from start state and symbol
getDst :: DFA -> String -> String -> [String]
getDst dfa sym src = [x !! 2 | x <- (map (\t -> [start t, symbol t, dest t]) (transitions dfa)), x !! 0 == src, x !! 1 == sym]

-- get all destinations for partition by same symbol
getDsts :: DFA -> [String] -> String -> [String]
getDsts dfa src sym = nub . concat $ map (getDst dfa sym) src

-- get sources to given dest and symbol
-- return only these which are in <states>
getSrcs :: DFA -> String -> [String] -> String -> [String]
getSrcs dfa sym states dst = intersect states [x !! 0 | x <- (map (\t -> [start t, symbol t, dest t]) (transitions dfa)), x !! 2 == dst, x !! 1 == sym]

-- alphabet list -> [String]
-- (chunksOf 1 (alphabet dfa))

-- Check that args are correct
validArgs args
    | not $ elem (length args) [1,2] = False        -- more than 2 args
    | not $ elem (args !! 0) ["-i", "-t"] = False   -- first arg must be '-i' or '-t'
    | elem "-i" args && elem "-t" args = False      -- ./dka-2-mka -i -t
    | otherwise = True

-- Read stdin or file
-- return content

getMyContents args
    | length args == 1 = getContents
    | otherwise        = readFile $ args !! 1

-- Get transitions from input and store them as [Transition]
getTransitions :: [String] -> [Transition]
getTransitions [] = []
getTransitions (x:xs) = Transition {
        start  = splitOn "," x !! 0,
        symbol = splitOn "," x !! 1,
        dest   = splitOn "," x !! 2
} : getTransitions xs

-- print trasnition structure
printTransition :: Transition -> String
printTransition trans = start trans ++ "," ++ symbol trans ++ "," ++ dest trans

-- parse given input as DFA
readDFA content = DFA {
    states = splitOn "," (content !! 0),
    alphabet = nub [x | x <- concat (drop 3 content), x `elem` ['a'..'z']],
    -- alphabet = intersect (concat (drop 3 content)) ['a'..'z'],
    -- getAlphabet trans = map (splitOn ",") ["4,b,6","7,c,9"]
    transitions = getTransitions (drop 3 content),
    sState = content !! 1,
    fStates = splitOn "," (content !! 2)
}

-- print finite automata
printFA fa = do
    putStrLn $ commaSeparated (states fa)
    putStrLn $ sState fa
    putStrLn $ commaSeparated (fStates fa)
    putStrLn . unlines $ map printTransition (transitions fa)


main = do
    args <- getArgs
    if not $ validArgs args
        then error "Invalid arguments"
        else do
            content <- getMyContents args
            let dfa = readDFA $ lines content
            print $ length content -- hack, avoid lazy evaluation - do it properly with deepseq if needed
            case (head args) of "-i" -> printFA dfa
                                "-t" -> printFA $ minimize dfa -- TODO: reduce
                                otherwise -> error "First argument should be '-i' or '-t'"
            print dfa -- debug print whole DFA structure



-- import Control.DeepSeq
-- import System.IO

-- main = do
--   contents <- getContents
--   contents `deepseq` putStr contents
