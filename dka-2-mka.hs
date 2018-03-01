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
    alphabet = [x | x <- concat (drop 3 content), x `elem` ['a'..'z']],
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
                                "-t" -> error "Not implemented yet"
                                otherwise -> error "First argument should be '-i' or '-t'"
            print dfa -- debug print whole DFA structure



-- import Control.DeepSeq
-- import System.IO

-- main = do
--   contents <- getContents
--   contents `deepseq` putStr contents