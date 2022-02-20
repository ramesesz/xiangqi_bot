-- module (NICHT ÄNDERN!)
module XiangqiBot
    ( getMove
    , listMoves
    ) 
    where

import Data.Char
-- More modules may be imported

import Util

--- external signatures (NICHT ÄNDERN!)
getMove :: String -> String
getMove = _getMoveImpl_ -- YOUR IMPLEMENTATION HERE


listMoves :: String -> String
listMoves = _listMovesImpl_ -- YOUR IMPLEMENTATION HERE


-- YOUR IMPLEMENTATION FOLLOWS HERE
getPosition :: Int -> Int -> Int
getPosition  spalte zeile = spalte + (zeile * 9)

getCoordinates :: Int -> (Int, Int)
--get array indices from position
getCoordinates pos = (spalte, zeile)
    where
        spalte = getSpalte pos
        zeile = getZeile pos

getZeile :: Int -> Int
getZeile pos = mod pos 9

spalteToInt :: Char -> Int
spalteToInt x = case x of
    'a' -> 0
    'b' -> 1
    'c' -> 2
    'd' -> 3
    'e' -> 4
    'f' -> 5
    'g' -> 6
    'h' -> 7
    'i' -> 8

getSpalte :: Int -> Int
getSpalte pos = div pos 10


getPotentialMove :: String -> Char -> Int -> Int -> Bool -> String
getPotentialMove b p startPos zielPos checkIfChecks = do
  case  toLower (b !! (startPos -1)) of -- trace ("player: " ++ show p ++ show (b !! (startPos -1)) ++ show startPos ++ "->" ++ show zielPos ++ " b: " ++ b)
    'p' -> getPawnMoves b p startPos zielPos checkIfChecks
    'k' -> getKingMoves b p startPos zielPos checkIfChecks
    'r' -> getRookMoves b p startPos zielPos checkIfChecks
    'n' -> getKnightMoves b p startPos zielPos checkIfChecks
    'b' -> getBishopMoves b p startPos zielPos checkIfChecks
    'q' -> getQueenMoves b p startPos zielPos checkIfChecks
    '0' -> ""

--redPlayer = true
getAdvisorMoves :: String -> Bool -> [Int] -> [Int] -> String
getAdvisorMoves board player startPos zielPos =
    if startPos == zielPos
        then ""
        else 
            if canMove && doesNotResultInCheck
            then "," ++ [chr (startPos !! 1 + 97)] ++ [intToDigit (9 - startPos!!0)] ++ "-" ++ [chr (zielPos !! 1 + 97)] ++ [intToDigit (9-zielPos!!0)]
            else ""
        where
            xStart = startPos !! 1
            yStart = startPos !! 0
            xZiel = zielPos !! 1
            yZiel = zielPos !! 0
            canMove = isInPalast player && (abs (xZiel - xStart)) == 1 && (abs (yZiel - yStart)) == 1
    
getSoldierMoves :: String -> Bool -> [Int] -> [Int] -> String
getSoldierMoves board player startPos zielPos =
    if startPos == zielPos
        then ""
        else 
            if canMove && doesNotResultInCheck
            then "," ++ [chr (startPos !! 1 + 97)] ++ [intToDigit (9 - startPos!!0)] ++ "-" ++ [chr (zielPos !! 1 + 97)] ++ [intToDigit (9-zielPos!!0)]
            else ""
        where
            xStart = startPos !! 1
            yStart = startPos !! 0
            xZiel = zielPos !! 1
            yZiel = zielPos !! 0