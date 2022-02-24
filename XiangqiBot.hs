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
getMove b = final
    where
        moves = splitOn ',' (listMoves b)
        move = last moves
        final
            | last move == ']' = init move
            | head move == '[' = tail move 
            | otherwise = move

listMoves :: String -> String
listMoves b = "[" ++ tail (validMoves board isRed) ++ "]"
    where
        splitted = splitOn ' ' b
        board = getBoard (head splitted)
        color = last splitted
        isRed = color == "r"

-- YOUR IMPLEMENTATION FOLLOWS HERE
getBoard :: [Char] -> [Char]
getBoard x = concatMap boardConverter (head (splitOn ' ' x))

--requires getBoard string
modifyBoard :: [Char] -> [Int] -> [Int] -> [Char]
modifyBoard board start ziel = modifyBoard2 newBoard start ziel
    where
        newBoard = take (calculateIndex ziel) board ++ [getFigurByPos board start] ++ drop (calculateIndex ziel+1) board

modifyBoard2 :: [Char] -> [Int] -> [Int] -> [Char]
modifyBoard2 board start ziel = take (calculateIndex start) board ++ ['1'] ++ drop (calculateIndex start+1) board

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
isTodesBlick board = recurseVerticalBlock (getBoard board) blackGeneral redGeneral == 0
    where
        redGeneral = getGeneralCoordinate (getBoard board) True
        blackGeneral = getGeneralCoordinate (getBoard board) False

getVerticalBlock :: [Char] -> [Int] -> [Int] -> Int 
getVerticalBlock board start end
    | head start > head end = recurseVerticalBlock (getBoard board) [head end+1, last end] [head start-1, last start]
    | otherwise = recurseVerticalBlock (getBoard board) [head start+1, last start] [head end-1, last end]

recurseVerticalBlock :: [Char] -> [Int] -> [Int] -> Int
recurseVerticalBlock board curr end
    | last curr /= last end = -1
    | head curr > head end = 0
    | otherwise = if currFigur /= '1' then recurseVerticalBlock board next end + 1 else recurseVerticalBlock board next end
    where
        currIndex = calculateIndex curr
        currFigur = getFigurByIndex (getBoard board) currIndex
        next = [head curr + 1, last curr]

getHorizontalBlock :: [Char] -> [Int] -> [Int] -> Int 
getHorizontalBlock board start end
    | last start > last end = recurseHorizontalBlock (getBoard board) [head end, last end+1] [head start, last start-1]
    | otherwise = recurseHorizontalBlock (getBoard board) [head start, last start+1] [head end, last end-1]

recurseHorizontalBlock :: [Char] -> [Int] -> [Int] -> Int
recurseHorizontalBlock board curr end
    | head curr /= head end = -1
    | last curr > last end = 0
    | otherwise = if currFigur/='1' then recurseHorizontalBlock board next end + 1 else recurseHorizontalBlock board next end
    where
        currIndex = calculateIndex curr
        currFigur = getFigurByIndex (getBoard board) currIndex
        next = [head curr, last curr + 1]

isCheck :: [Char] -> Bool -> Bool
isCheck board isRed = not (null (recurseCheck (getBoard board) isRed 0 89))

-- kalau checkMove gak kosong, berarti ga check
-- kalau semuanya kosong berarti check

recurseCheck :: [Char] -> Bool -> Int -> Int -> [Char]
recurseCheck board isRed curr end
    | curr == end = currCheck
    | otherwise = currCheck ++ recurseCheck board isRed next end
    where 
        currCheck = checkMove board (not isRed) moveFrom moveTo
        next = curr + 1
        moveFrom = calculatePos curr
        moveTo = getGeneralCoordinate (getBoard board) isRed

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
    | curr == end = checkMove board isRed moveFrom moveTo
    | otherwise = checkMove board isRed moveFrom moveTo ++ recurseValidMoves board isRed from next end
    where
        next = curr + 1
        moveFrom = calculatePos from
        moveTo = calculatePos curr

checkMove :: [Char] -> Bool -> [Int] -> [Int] -> [Char]
checkMove board isRed from to
    | moveInBoard && isValid = getFigurMove
    | otherwise = ""
    where
        xStart = from !! 1
        yStart = from !! 0
        xZiel = to !! 1
        yZiel = to !! 0
        moveInBoard = xStart >=0 && xStart <= 8 && yStart >=0 && yStart <= 9 && xZiel >=0 && xZiel <= 8 && yZiel >=0 && yZiel <= 9 
        isValid = startZielIsValid board isRed from to
        getFigurMove = checkFigur board isRed from to

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

checkFigur :: [Char] -> Bool -> [Int] -> [Int] -> [Char]
checkFigur board isRed from to
    | figur == 'G' || figur == 'g' = getGeneralMoves isRed from to
    | figur == 'A' || figur == 'a' = getAdvisorMoves isRed from to
    | figur == 'E' || figur == 'e' = getElephantMoves board isRed from to
    | figur == 'H' || figur == 'h' = getHorseMoves board from to
    | figur == 'R' || figur == 'r' = getRookMoves board from to
    | figur == 'C' || figur == 'c' = getCannonMoves board from to
    | figur == 'S' || figur == 's' = getSoldierMoves isRed from to
    | otherwise = ""
    where
        figur = getFigurByPos (getBoard board) from

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
        canMove = isInPalast zielPos player &&  abs (yStart - yZiel) + abs (xStart - xZiel) == 0

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
            | xStart - xZiel == 0 && (getVerticalBlock board startPos zielPos) /= 0 = False
            | yStart - yZiel == 0 && (getHorizontalBlock board startPos zielPos) /= 0 = False
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
            | getFigurByPos board [yZiel, xZiel] == '1' && ((getVerticalBlock board startPos zielPos) /= 0 && (getHorizontalBlock board startPos zielPos) /= 0) = False --if move weg zum Ziel have to be clear
            | getFigurByPos board [yZiel, xZiel] /= '1' && ((getVerticalBlock board startPos zielPos) /= 1 && (getHorizontalBlock board startPos zielPos) /= 1) = False
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


main :: IO ()
main = do
    let start = "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR b"
    let blah = "9/9/9/9/9/9/S8/9/9/R8"
    let check = "2R6/3R3g1/R8/s1s3s2/6h1s/9/S1S5S/c1H6/4A4/4GAE2"
    print (getBoard start)
    print (validMoves (getBoard start) False)
    print (getMoveChar [9, 1])
    -- print (getFigurByIndex (getBoard start) 89)
    print (length (getBoard start))
    print (getFigurByIndex (getBoard start) ((length (getBoard start))-1))
    print (getVerticalBlock (getBoard start) (getPos "a6") (getPos "a9"))
    print (getHorizontalBlock (getBoard start) (getPos "b3") (getPos "a3"))
    print (validMoves (getBoard blah) True)
    print (getVerticalBlock (getBoard blah) (getPos "a0") (getPos "a1"))
    print (isCheck (getBoard check) True)
    print (listMoves start)
    print (getMove start)