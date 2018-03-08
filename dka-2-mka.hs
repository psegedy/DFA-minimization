-- File: dka-2-mka.hs
-- Author: Patrik Segedy <xseged00@vutbr.cz>
-- Description: DFA minimization

module Main where

import System.Environment
import Data.List
import Data.List.Split

-- Data structure for transitions of finite automata
data Transition = Transition    { start :: String
                                , dest :: String
                                , symbol :: String
                                } deriving (Show, Eq)

-- Define deterministic finite automata
data DFA = DFA  { states :: [String]
                , alphabet :: [Char]
                , transitions :: [Transition]
                , sState :: String
                , fStates :: [String]
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
    | otherwise = dfa {states = insert (show $ newStateNum dfa) (states dfa), transitions = (transitions dfa) ++ (missingTransitions dfa) } -- add sink state

-- get missing transitions in [Transition] format
missingTransitions dfa = map (\x -> Transition [(fst x)] (show $ newStateNum dfa) [(snd x)]) missing
    where
        -- [("1",""),("2","a"),("3","ab")]
        zipped = zip (states dfa) $ map (missingSymbols dfa) (states dfa)
        -- [('2','a'),('3','a'),('3','b')]
        missing = concat $ map (multipleTransitions) (filter (sndNotEmpty) zipped)

-- get next free state - e.g. use it as sink state
newStateNum dfa = succ $ head $ sortDesc $ stoi (states dfa)

-- get missing symbols for state
-- DFA, state, missing symbols
missingSymbols :: DFA -> String -> String
missingSymbols dfa state = alphabet dfa \\ concat [x !! 1 | x <- (map (\t -> [start t, symbol t]) (transitions dfa)), x !! 0 == state]

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
                                "-t" -> printFA $ wellDefined dfa -- TODO: reduce
                                otherwise -> error "First argument should be '-i' or '-t'"
            print dfa -- debug print whole DFA structure



-- import Control.DeepSeq
-- import System.IO

-- main = do
--   contents <- getContents
--   contents `deepseq` putStr contents