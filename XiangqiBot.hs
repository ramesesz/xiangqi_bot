-- module (NICHT ÄNDERN!)
module XiangqiBot
    ( getMove
    , listMoves
    ) 
    where

import Data.Char
-- More modules may be imported

import Util
import GHC.RTS.Flags (ProfFlags(startHeapProfileAtStartup))

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

getGeneralMoves :: Bool -> [Int] -> [Int] -> String
getGeneralMoves player startPos zielPos =
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
        canMove = isInPalast player &&  abs (yStart - yZiel) + abs (xStart - xZiel) == 1



getAdvisorMoves :: Bool -> [Int] -> [Int] -> String
getAdvisorMoves player startPos zielPos =
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
    
getElephantMoves :: String -> Bool -> [Int] -> [Int] -> String
getElephantMoves board player startPos zielPos =
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
        canMove
            | player && (yStart < 5 || yZiel < 5) = False 
            | not player && (yStart > 4 || yZiel > 4) = False
            | abs (yStart - yZiel /= 2) || abs (xStart - xZiel /= 2) = False
            | (getFigur board ([abs (yStart + yZiel)/2, abs (xStart + xZiel)/2])) /= '1' = False 

getSoldierMoves :: Bool -> [Int] -> [Int] -> String
getSoldierMoves player startPos zielPos =
    if startPos == zielPos
        then ""
    else 
        if canMove && doesNotResultInCheck && moveVertHor
            then "," ++ [chr (startPos !! 1 + 97)] ++ [intToDigit (9 - startPos!!0)] ++ "-" ++ [chr (zielPos !! 1 + 97)] ++ [intToDigit (9-zielPos!!0)]
        else ""
    where
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        moveVertHor = (xStart - xZiel == 0) || (yStart - yZiel == 0)
        canMove = 
            if player then redSoldierValid startPos zielPos 
                else blackSoldierValid startPos zielPos
            
redSoldierValid :: [Int] -> [Int] -> Bool
redSoldierValid startPos zielPos = ownTerritoryValid || opponentTerritoryValid 
    where 
        --own territory only forward
        ownTerritoryValid = startPos!!0 > 4 && startPos!!0 - zielPos!!0 == 1
        --opponent territory only vertical and horizonal, but no backwards
        opponentTerritoryValid = startPos!!0 < 5 && (abs (startPos!!0 - zielPos!!0) + abs (startPos!!1 - zielPos!!1) == 1) && (startPos!!0 - zielPos!!0 /= -1)

blackSoldierValid :: [Int] -> [Int] -> Bool
blackSoldierValid startPos zielPos = ownTerritoryValid || opponentTerritoryValid
    where
        --own territory only forward
        ownTerritoryValid = startPos!!0 < 5 && startPos!!0 - zielPos!!0 == -1 
        --opponent territory only vertical and horizonal, but no backwards
        opponentTerritoryValid = startPos!!0 > 4 && (abs (startPos!!0 - zielPos!!0) + abs (startPos!!1 - zielPos!!1) == 1) && (startPos!!0 - zielPos!!0 /= 1)
