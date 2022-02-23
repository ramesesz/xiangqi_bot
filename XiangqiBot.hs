-- module (NICHT ÄNDERN!)
module XiangqiBot
    ( getMove
    , listMoves
    ) 
    where

import Data.Char
-- More modules may be imported

import Util
import Data.List

--- external signatures (NICHT ÄNDERN!)
getMove :: String -> String
getMove = _getMoveImpl_ -- YOUR IMPLEMENTATION HERE


listMoves :: String -> String
listMoves = _listMovesImpl_ -- YOUR IMPLEMENTATION HERE


-- YOUR IMPLEMENTATION FOLLOWS HERE
getBoard :: [Char] -> [Char]
getBoard x = concatMap boardConverter (head (splitOn ' ' x))

boardConverter :: Char -> [Char]
boardConverter x
    | x == '9' = "111111111"
    | x == '8' = "11111111"
    | x == '7' = "1111111"
    | x == '6' = "111111"
    | x == '5' = "11111"
    | x == '4' = "1111"
    | x == '3' = "111"
    | x == '2' = "11"
    | x == '/' = ""
    | otherwise = [x]

getFigur :: [Char] -> [Char] -> Char
getFigur board pos = board !! getIndex pos

getFigurByIndex :: [Char] -> Int -> Char
getFigurByIndex board index = board !! index

getFigurByPos :: [Char] -> [Int] -> Char
getFigurByPos board pos = getFigurByIndex board (calculateIndex pos)

getIndex :: [Char] -> Int
getIndex pos = getZeile (last pos) * 9 + getSpalte (head pos) 

calculateIndex :: [Int] -> Int
calculateIndex pos = head pos * 9 + pos !! 1

getPos :: [Char] -> [Int]
getPos x = [getZeile (last x), getSpalte (head x)]

calculatePos :: Int -> [Int]
calculatePos index = [index `div` 9, index `mod` 9]

getMoveChar :: [Int] -> [Char]
getMoveChar pos = [getX (last pos), getY (head pos)]

getMoveString :: [Int] -> [Char]
getMoveString pos = getMoveChar(take 2 pos) ++ "-" ++ getMoveChar(drop 2 pos)

getTranslatedMove :: [Char] -> [Int]
getTranslatedMove move = getPos (take 2 move) ++ getPos(drop 3 move)

getX :: Int -> Char 
getX x
    | x == 0 = 'a'
    | x == 1 = 'b'
    | x == 2 = 'c'
    | x == 3 = 'd'
    | x == 4 = 'e'
    | x == 5 = 'f'
    | x == 6 = 'g'
    | x == 7 = 'h'
    | x == 8 = 'i'
    | otherwise = '1'

getY :: Int -> Char
getY x = intToDigit (9-x)

getSpalte :: Char -> Int
getSpalte x
    | x == 'a' = 0
    | x == 'b' = 1
    | x == 'c' = 2
    | x == 'd' = 3
    | x == 'e' = 4
    | x == 'f' = 5
    | x == 'g' = 6
    | x == 'h' = 7
    | x == 'i' = 8
    | otherwise = -1

getZeile :: Char -> Int
getZeile x = 9 - digitToInt x

isInPalast :: [Int] -> Bool -> Bool
isInPalast move isRed
    | isRed = isInPalastRed pos
    | otherwise = isInPalastBlack pos
    where
        pos = getMoveChar move

isInPalastRed :: [Char] -> Bool
isInPalastRed pos |
    pos == "d0" || pos == "e0" || pos == "f0" || pos == "d1" || pos == "e1" || pos == "f1" || pos == "d2" || pos == "e2" || pos == "f2" = True
    | otherwise = False

isInPalastBlack :: [Char] -> Bool
isInPalastBlack pos |
    pos == "d7" || pos == "e7" || pos == "f7" || pos == "d8" || pos == "e8" || pos == "f8" || pos == "d9" || pos == "e9" || pos == "f9" = True
    | otherwise = False

getGeneralCoordinate :: [Char] -> Bool -> [Int]
getGeneralCoordinate board isRed
    | isRed = getGeneralCoordinateRed board
    | otherwise = getGeneralCoordinateBlack board

getGeneralCoordinateRed :: [Char] -> [Int]
getGeneralCoordinateRed board = [y,x]
    where
        index = elemIndices 'G' board
        x = mod (head index) 9
        y = div (head index) 9

getGeneralCoordinateBlack :: [Char] -> [Int]
getGeneralCoordinateBlack board = [y,x]
    where
        index = elemIndices 'g' board
        x = mod (head index) 9
        y = div (head index) 9

isTodesBlick :: [Char] -> Bool
isTodesBlick board = getVerticalBlock (getBoard board) blackGeneral redGeneral 0 == 1
    where
        redGeneral = getGeneralCoordinate (getBoard board) True
        blackGeneral = getGeneralCoordinate (getBoard board) False

getVerticalBlock :: [Char] -> [Int] -> [Int] -> Int -> Int
getVerticalBlock board curr end count
    | head curr > head end && count == 0 = getVerticalBlock board end curr count
    | last curr /= last end = -1
    | head curr >= head end = count
    | otherwise = getVerticalBlock board next end newCount
    where
        currIndex = calculateIndex curr
        currFigur = getFigurByIndex (getBoard board) currIndex
        next = [head curr + 1, last curr]
        newCount = if currFigur /= '1' then count + 1 else count

getHorizontalBlock :: [Char] -> [Int] -> [Int] -> Int -> Int
getHorizontalBlock board curr end count
    | last curr > last end && count == 0 = getHorizontalBlock board end curr count
    | head curr /= head end = -1
    | last curr >= last end = count
    | otherwise = getHorizontalBlock board next end newCount
    where
        currIndex = calculateIndex curr
        currFigur = getFigurByIndex (getBoard board) currIndex
        newCount = if currFigur /= '1' then count + 1 else count
        next = [head curr, last curr + 1]

getDiagonalBlock :: [Char] -> [Int] -> [Int] -> Int -> Int
getDiagonalBlock board curr end count
    | last curr > last end && head curr > head end && count == 0 = getDiagonalBlock board end curr count
    | abs (head end-head curr) /= abs (last end-last curr) = -1
    | last curr >= last end && head curr >= head end = count
    | otherwise = getDiagonalBlock board next end newCount
    where
        currIndex = calculateIndex curr
        currFigur = getFigurByIndex (getBoard board) currIndex
        newCount = if currFigur /= '1' then count + 1 else count
        next = [head curr + 1, last curr + 1]

-- Dari index 0, loop sampe index 89 buat dapetin moves (pake concatMap) (from)
-- Di recursemoves, recurse dari 0 sampe 89 buat index tujuan (to)

tryIndices :: [Int]
tryIndices = [0..89]

validMoves :: [Char] -> Bool -> [Char]
validMoves board isRed = concatMap (getValidMoves board isRed) tryIndices

getValidMoves :: [Char] -> Bool -> Int -> [Char]
getValidMoves board isRed index = recurseValidMoves board isRed index 0 89

recurseValidMoves :: [Char] -> Bool -> Int -> Int -> Int -> [Char]
recurseValidMoves board isRed from curr end
    | curr == end = ""
    | otherwise = checkMove board isRed moveFrom moveTo ++ recurseValidMoves board isRed from next end
    where
        next = curr + 1
        moveFrom = calculatePos from
        moveTo = calculatePos curr

checkMove :: [Char] -> Bool -> [Int] -> [Int] -> [Char]
checkMove board isRed from to
    | moveInBoard && isValid = checkFigur
    | otherwise = ""
    where
        xStart = from !! 1
        yStart = from !! 0
        xZiel = to !! 1
        yZiel = to !! 0
        moveInBoard = xStart >=0 && xStart <= 8 && yStart >=0 && yStart <= 9 && xZiel >=0 && xZiel <= 8 && yZiel >=0 && yZiel <= 9 
        isValid = startZielIsValid board isRed from to

startZielIsValid :: [Char] -> Bool -> [Int] -> [Int] -> Bool
startZielIsValid board isRed from to
    | from == to = False
    | moveBlank = False
    | opponentFigur = False 
    | killOwn = False
    | otherwise = True
    where
        figurFrom = getFigurByPos (getBoard board) from
        figurTo = getFigurByPos (getBoard board) to
        moveBlank = figurFrom == '1'
        opponentFigur = (isRed && isLower figurFrom) || (not isRed && isUpper figurFrom)
        killOwn = (isRed && isUpper figurTo) || (not isRed && isLower figurTo)

getGeneralMoves :: Bool -> [Int] -> [Int] -> String
getGeneralMoves player startPos zielPos
    | startPos == zielPos = "" --nanti dibikin checkmove
    | canMove = "," ++ getMoveChar startPos ++ "-" ++ getMoveChar zielPos
    | otherwise = ""
    where
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        canMove = isInPalast zielPos player &&  abs (yStart - yZiel) + abs (xStart - xZiel) == 1

getAdvisorMoves :: Bool -> [Int] -> [Int] -> String
getAdvisorMoves player startPos zielPos
    | startPos == zielPos = "" --nanti dibikin checkmove
    | canMove = "," ++ getMoveChar startPos ++ "-" ++ getMoveChar zielPos
    | otherwise = ""
    where
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        canMove = isInPalast zielPos player && (abs (xZiel - xStart)) == 1 && (abs (yZiel - yStart)) == 1

getElephantMoves :: String -> Bool -> [Int] -> [Int] -> String
getElephantMoves board player startPos zielPos
    | startPos == zielPos = "" --nanti dibikin checkmove
    | canMove = "," ++ getMoveChar startPos ++ "-" ++ getMoveChar zielPos
    | otherwise = ""
    where
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        canMove
            | player && (yStart < 5 || yZiel < 5) = False 
            | not player && (yStart > 4 || yZiel > 4) = False
            | abs (yStart - yZiel) /= 2 || abs (xStart - xZiel) /= 2 = False
            | getFigurByPos board [abs (div (yStart + yZiel) 2), abs (div (xStart + xZiel) 2)] /= '1' = False 
            | otherwise = True

getHorseMoves :: [Char] -> [Int] -> [Int] -> String
getHorseMoves board startPos zielPos
    | canMove = "," ++ getMoveChar startPos ++ "-" ++ getMoveChar zielPos
    | otherwise = ""
    where
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        canMove
            | yZiel - yStart == -2 && getFigurByPos board [yStart - 1, xStart] /= '1' = False
            | xZiel - xStart == 2 && getFigurByPos board [yStart, xStart + 1] /= '1' = False
            | yZiel - yStart == 2 && getFigurByPos board [yStart + 1, xStart] /= '1' = False
            | xZiel - xStart == -2 && getFigurByPos board [yStart, xStart - 1] /= '1' = False
            | abs (yStart - yZiel) == 1 && abs (xStart - xZiel) == 2 = True
            | abs (yStart - yZiel) == 2 && abs (xStart - xZiel) == 1 = True
            | otherwise = False

getRookMoves :: [Char] -> [Int] -> [Int] -> String
getRookMoves board startPos zielPos
    | canMove = "," ++ getMoveChar startPos ++ "-" ++ getMoveChar zielPos
    | otherwise = ""
    where
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        canMove
            | yStart - yZiel /= 0 && xStart - xZiel /= 0 = False --can only move hor or vert
            | yStart - yZiel == 0 && (getVerticalBlock board startPos zielPos 0) /= 1 = False
            | xStart - xZiel == 0 && (getHorizontalBlock board startPos zielPos 0) /= 1 = False
            | otherwise = True

getCannonMoves :: [Char] -> [Int] -> [Int] -> String
getCannonMoves board startPos zielPos
    | canMove = "," ++ getMoveChar startPos ++ "-" ++ getMoveChar zielPos
    | otherwise = "" 
    where 
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        canMove
            | yStart - yZiel /= 0 && xStart - xZiel /= 0 = False --can only move hor or vert
            | getFigurByPos board [yZiel, xZiel] == '1' && ((getVerticalBlock board startPos zielPos 0) /= 1 || (getHorizontalBlock board startPos zielPos 0) /= 1) = False --if move weg zum Ziel have to be clear
            | getFigurByPos board [yZiel, xZiel] /= '1' && ((getVerticalBlock board startPos zielPos 0) /= 2 || (getHorizontalBlock board startPos zielPos 0) /= 2) = False
            | otherwise = True

getSoldierMoves :: Bool -> [Int] -> [Int] -> String
getSoldierMoves player startPos zielPos
    | startPos == zielPos = "" --nanti dibikin checkmove
    | canMove = "," ++ getMoveChar startPos ++ "-" ++ getMoveChar zielPos
    | otherwise = ""
    where
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        canMove 
            | (xStart - xZiel /= 0) && (yStart - yZiel /= 0) = False --either vertical or horizontal
            | player = redSoldierValid startPos zielPos 
            | not player = blackSoldierValid startPos zielPos
            | otherwise = False
            
redSoldierValid :: [Int] -> [Int] -> Bool
redSoldierValid startPos zielPos
    | (yStart > 4 && yStart - yZiel /= 1) = False 
    | (yStart < 5 && (abs (yStart - yZiel) + abs (xStart - xZiel) /= 1)) = False
    | (yStart - yZiel == -1) = False
    | otherwise = True
        where
            xStart = startPos !! 1
            yStart = startPos !! 0
            xZiel = zielPos !! 1
            yZiel = zielPos !! 0

blackSoldierValid :: [Int] -> [Int] -> Bool
blackSoldierValid startPos zielPos
    | (yStart < 5 && yStart - yZiel /= -1) = False 
    | (yStart > 4 && (abs (yStart - yZiel) + abs (xStart - xZiel) /= 1)) = False
    | (yStart - yZiel == 1) = False
    | otherwise = True
        where
            xStart = startPos !! 1
            yStart = startPos !! 0
            xZiel = zielPos !! 1
            yZiel = zielPos !! 0
