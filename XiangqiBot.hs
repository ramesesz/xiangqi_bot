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
isSamePos :: Int -> Int -> Bool
isSamePos piecePos tryPos = piecePos == tryPos

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