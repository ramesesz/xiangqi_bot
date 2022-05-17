--module (NICHT ÄNDERN!)
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
getMove b = last (splitOn ',' moves)
    where
        splitted = splitOn ' ' b
        board = getBoard (head splitted)
        color = last splitted
        isRed = color == "r"
        moves = tail (filterValidMoves board isRed)

listMoves :: String -> String
listMoves b = "[" ++ tail (filterValidMoves board isRed) ++ "]"
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

replacer :: Int -> Char -> [Char] -> [Char]
replacer index char board = take index board ++ char : drop (index + 1) board

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
        x
            | length index > 0 = mod (head index) 9
            | otherwise = -1
        y 
            | length index > 0 = div (head index) 9
            | otherwise = -1

getGeneralCoordinateBlack :: [Char] -> [Int]
getGeneralCoordinateBlack board = [y,x]
    where
        index = elemIndices 'g' board
        x
            | length index > 0 = mod (head index) 9
            | otherwise = -1
        y 
            | length index > 0 = div (head index) 9
            | otherwise = -1

isTodesBlick :: [Char] -> Bool
isTodesBlick board
    | blackGeneral == [-1,-1] || redGeneral == [-1,-1] = True
    | otherwise = getVerticalBlock board blackGeneral redGeneral == 0
    where
        redGeneral = getGeneralCoordinate board True
        blackGeneral = getGeneralCoordinate board False

getVerticalBlock :: [Char] -> [Int] -> [Int] -> Int 
getVerticalBlock board start end
    | head start > head end = recurseVerticalBlock board [head end+1, last end] [head start-1, last start]
    | otherwise = recurseVerticalBlock board [head start+1, last start] [head end-1, last end]

recurseVerticalBlock :: [Char] -> [Int] -> [Int] -> Int
recurseVerticalBlock board curr end
    | last curr /= last end = -1
    | head curr > head end = 0
    | otherwise = if currFigur /= '1' then recurseVerticalBlock board next end + 1 else recurseVerticalBlock board next end
    where
        currIndex = calculateIndex curr
        currFigur = getFigurByIndex board currIndex
        next = [head curr + 1, last curr]

getHorizontalBlock :: [Char] -> [Int] -> [Int] -> Int 
getHorizontalBlock board start end
    | last start > last end = recurseHorizontalBlock board [head end, last end+1] [head start, last start-1]
    | otherwise = recurseHorizontalBlock board [head start, last start+1] [head end, last end-1]

recurseHorizontalBlock :: [Char] -> [Int] -> [Int] -> Int
recurseHorizontalBlock board curr end
    | head curr /= head end = -1
    | last curr > last end = 0
    | otherwise = if currFigur/='1' then recurseHorizontalBlock board next end + 1 else recurseHorizontalBlock board next end
    where
        currIndex = calculateIndex curr
        currFigur = getFigurByIndex board currIndex
        next = [head curr, last curr + 1]

isCheck :: [Char] -> Bool -> Bool
isCheck board isRed = not (null (recurseCheck board isRed 0 (length board)))

-- kalau checkMove gak kosong, berarti ga check
-- kalau semuanya kosong berarti check

recurseCheck :: [Char] -> Bool -> Int -> Int -> [Char]
recurseCheck board isRed curr end
    | curr == end = currCheck
    | not (null currCheck) = currCheck
    | otherwise = currCheck ++ recurseCheck board isRed next end
    where 
        currCheck
            | moveTo == [-1,-1] = getMoveChar moveFrom ++ "-" ++ getMoveChar moveTo
            | otherwise = checkMove board (not isRed) moveFrom moveTo
        next = curr + 1
        moveFrom = calculatePos curr
        moveTo = getGeneralCoordinate board isRed

-- Dari index 0, loop sampe index 89 buat dapetin moves (pake concatMap) (from)
-- Di recursemoves, recurse dari 0 sampe 89 buat index tujuan (to)

filterValidMoves :: [Char] -> Bool -> [Char]
filterValidMoves board isRed = final
    where
        moves = splitOn ',' (validMoves board isRed)
        fixed = tail moves
        final = concatMap (filterMove board isRed) fixed

filterMove :: [Char] -> Bool -> [Char] -> [Char]
filterMove board isRed move
    | clear = "," ++ move
    | otherwise = ""
    where
        newMove = getTranslatedMove move
        moveFrom = take 2 newMove
        moveTo = drop 2 newMove
        newBoard = modifyBoard board moveFrom moveTo
        clear = clearBlickUndCheck newBoard isRed
        
tryIndices :: [Int]
tryIndices = [0..89]

validMoves :: [Char] -> Bool -> [Char]
validMoves board isRed = concatMap (getValidMoves board isRed) tryIndices

getValidMoves :: [Char] -> Bool -> Int -> [Char]
getValidMoves board isRed index = recurseValidMoves board isRed index 0 (length board)

recurseValidMoves :: [Char] -> Bool -> Int -> Int -> Int -> [Char]
recurseValidMoves board isRed from curr end
    | curr == end = moveStr
    | otherwise = moveStr ++ recurseValidMoves board isRed from next end
    where
        next = curr + 1
        moveFrom = calculatePos from
        moveTo = calculatePos curr
        moveStr = checkMove board isRed moveFrom moveTo

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
        figurFrom = getFigurByPos board from
        figurTo = getFigurByPos board to
        moveBlank = figurFrom == '1'
        opponentFigur = (isRed && isLower figurFrom) || (not isRed && isUpper figurFrom)
        killOwn = (isRed && isUpper figurTo) || (not isRed && isLower figurTo)

clearBlickUndCheck :: [Char] -> Bool -> Bool
clearBlickUndCheck board isRed
    | isCheck board isRed = False
    | isTodesBlick board = False
    | otherwise = True

checkFigur :: [Char] -> Bool -> [Int] -> [Int] -> [Char]
checkFigur board isRed from to
    | figur == 'G' || figur == 'g' = getGeneralMoves board isRed from to
    | figur == 'A' || figur == 'a' = getAdvisorMoves board isRed from to
    | figur == 'E' || figur == 'e' = getElephantMoves board isRed from to
    | figur == 'H' || figur == 'h' = getHorseMoves board isRed from to
    | figur == 'R' || figur == 'r' = getRookMoves board isRed from to
    | figur == 'C' || figur == 'c' = getCannonMoves board isRed from to
    | figur == 'S' || figur == 's' = getSoldierMoves board isRed from to
    | otherwise = ""
    where
        figur = getFigurByPos board from
        -- convertedBoard = getBoard board
        -- newBoard = modifyBoard convertedBoard from to

getGeneralMoves :: [Char] -> Bool -> [Int] -> [Int] -> String
getGeneralMoves board player startPos zielPos
    | startPos == zielPos = "" --nanti dibikin checkmove
    | canMove = "," ++ getMoveChar startPos ++ "-" ++ getMoveChar zielPos
    | otherwise = ""
    where
        xStart = startPos !! 1
        yStart = startPos !! 0
        xZiel = zielPos !! 1
        yZiel = zielPos !! 0
        canMove = isInPalast zielPos player &&  abs (yStart - yZiel) + abs (xStart - xZiel) == 1

getAdvisorMoves :: [Char] -> Bool -> [Int] -> [Int] -> String
getAdvisorMoves board player startPos zielPos
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

getHorseMoves :: [Char] -> Bool -> [Int] -> [Int] -> String
getHorseMoves board player startPos zielPos
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

getRookMoves :: [Char] -> Bool -> [Int] -> [Int] -> String
getRookMoves board player startPos zielPos
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

getCannonMoves :: [Char] -> Bool -> [Int] -> [Int] -> String
getCannonMoves board player startPos zielPos
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

getSoldierMoves :: [Char] -> Bool -> [Int] -> [Int] -> String
getSoldierMoves board player startPos zielPos
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


-- main :: IO ()
-- main = do
--     print (listMoves "rheagaehr/9/1c5c1/s3s1s1s/2s6/S8/2S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rhe2a1hr/4a4/1c2eg1c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/3GA4/RHE2AEHR b")
--     print (listMoves "rhea1a1h1/9/1c2Sgr2/s1s4cs/6s2/1C7/S1S3SCS/R4A3/3G5/1HE2AEHR b")
--     print (listMoves "rhea1a1h1/9/1c2g1r2/s1s4cs/6s2/1C7/S1S3SCS/R8/3GA4/1HE2AEHR b")
--     print (listMoves "rheagaehr/9/1c7/s1sSs3s/9/c5s2/4S1S1S/1C5C1/9/RHEAGAEHR b")
--     print (listMoves "rCeaga1hr/9/9/scs5s/2e3S2/c7S/9/4E2C1/9/RH1AGAEHR b")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/S8/2S1S1S1S/1C5C1/9/RHEAGAEHR b")
--     print (listMoves "rheaga1hr/9/1c7/s1sSs3s/6e2/c1E5S/4SsS2/1C5C1/9/RH1AGAEHR r")
--     print (listMoves "rCeaga1hr/9/9/scs5s/2e3S2/c7S/9/4E2C1/9/RH1AGAEHR b")
--     print (listMoves "r1e1ga1h1/4a4/4H1r2/7cs/s8/S1s6/5hSCS/R8/4G4/2EA1AEHR r")
--     print (listMoves "rheagae1r/9/1c4hc1/s1s1s1s1s/9/4S4/SCS3S1S/7C1/9/RHEAGAEHR b")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rhe2a1hr/4a4/1c2eg1c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/3G5/RHEA1AEHR r")
--     print (listMoves "rhea1a1hr/4g4/1c2e2c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/4G4/RHEA1AEHR r")
--     print (listMoves "rheaga1hr/9/1c2S4/s1s5s/6e2/c1E5S/4s1S2/1C5C1/9/RH1AGAEHR b")
--     print (listMoves "rCeaga1hr/9/1c7/s1s5s/2e6/c7S/6S2/4E2C1/9/RH1AGAEHR r")
--     print (listMoves "rheagae1r/4h4/1c5c1/s1s1s3s/4S1s2/9/SCS3S1S/7C1/9/RHEAGAEHR r")
--     print (listMoves "1Ceaga1hr/9/r8/scs5s/2e3S2/R7S/9/4E2C1/9/1H1AGAEHR b")
--     print (listMoves "r1e1ga1h1/4a4/4c1r2/2H4cs/s8/S1s6/5hSCS/R8/9/2EAGAEHR r")
--     print (listMoves "rheagaehr/9/1c1c5/s1s1s1s1s/9/9/S1S1S1S1S/3C3C1/9/RHEAGAEHR r")
--     print (listMoves "rheaga1hr/9/1c7/s1sSs3s/6e2/c1E2s3/4S1S1S/1C5C1/9/RH1AGAEHR r")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rheagaehr/9/1c5c1/s3s1s1s/2s6/S8/2S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rheaga1hr/9/1c2e4/s1s5s/9/c1E5S/4s1S2/1C5C1/9/RH1AGAEHR r")
--     print (listMoves "rhe1ga1h1/4a4/4c1r2/7cs/s1s6/9/S1S3SCS/R8/9/1HEAGAEHR r")
--     print (listMoves "rheaga1hr/9/1c2e4/s1sSs3s/9/c1E2s3/4S1S1S/1C5C1/9/RH1AGAEHR b")
--     print (listMoves "rheaga1hr/9/1c7/s1s1S3s/6e2/c1E5S/4SsS2/1C5C1/9/RH1AGAEHR b")
--     print (listMoves "rhea1a1hr/4g4/1c2e2c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/4G4/RHEA1AEHR r")
--     print (listMoves "rhe2a1hr/9/1c1aeg1c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/3GA4/RHE2AEHR r")
--     print (listMoves "rheaga1hr/9/1c7/s1sSs3s/6e2/c1E2s2S/4S1S2/1C5C1/9/RH1AGAEHR b")
--     print (listMoves "rhea1a1h1/4g4/1c3r3/7cs/s1s1C4/9/S1S3SCS/R8/4A4/1HE1GAEHR r")
--     print (listMoves "rheaga1hr/9/1c7/s1sSs3s/6e2/c1E2s3/4S1S1S/1C5C1/9/RH1AGAEHR r")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rhea1a1hr/5g3/1c2e2c1/s1s1s3s/6s2/1C2S4/S1S3S1S/3G3C1/9/RHEA1AEHR b")
--     print (listMoves "rhea1a1h1/4g4/1c3r3/s6cs/2s3s2/1C7/S1S3SCS/R8/4A4/1HE1GAEHR r")
--     print (listMoves "rheagaehr/9/1c1c5/s1s1s1s1s/9/9/S1S1S1S1S/3C3C1/9/RHEAGAEHR r")
--     print (listMoves "rCeaga1hr/9/1c2e4/s1s5s/9/c1E5S/6S2/4s2C1/9/RH1AGAEHR r")
--     print (listMoves "rheagaehr/9/1c7/s1s1s3s/2S3s2/c8/4S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/S8/2S1S1S1S/1C5C1/9/RHEAGAEHR b")
--     print (listMoves "rhea1a1h1/4g4/1c2r4/7cs/s1s1C4/9/S1S3SCS/R8/4A4/1HE1GAEHR b")
--     print (listMoves "rhe2a1hr/9/1c1aeg3/s1s1s2cs/6s2/1C2S4/S1S3SCS/R8/3GA4/1HE2AEHR b")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "r1e1ga1h1/4a4/4H1r2/7cs/s8/S1s6/6SCS/R2h5/9/2EAGAEHR r")
--     print (listMoves "rhea1a1h1/9/1c2g1r2/s1s4cs/6s2/1C7/S1S3SCS/R4A3/3G5/1HE2AEHR r")
--     print (listMoves "rheagaehr/9/2c4c1/s1s1s1s1s/9/9/S1S1S1S1S/3C3C1/9/RHEAGAEHR r")
--     print (listMoves "rhea1a1h1/9/1c2Sgr2/s1s4cs/6s2/1C7/S1S3SCS/R4A3/3G5/1HE2AEHR b")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/S8/2S1S1S1S/1C5C1/9/RHEAGAEHR b")
--     print (listMoves "r1e1ga1h1/4a4/4H1r2/7cs/s8/S1s6/6SCS/R2h5/9/2EAGAEHR r")
--     print (listMoves "rheaga1hr/9/1c2e4/s1s5s/9/c1E5S/4s1S2/1C5C1/9/RH1AGAEHR r")
--     print (listMoves "rhea1a1h1/4g4/4cr3/7cs/s1sC5/9/S1S3SCS/R8/9/1HEAGAEHR b")
--     print (listMoves "rheagaeCr/9/2c4c1/s1s1s1s1s/9/9/S1S1S1S1S/3C5/9/RHEAGAEHR b")
--     print (listMoves "rheagae1r/4h4/1c5c1/s1s1s3s/4S1s2/9/SCS3S1S/7C1/9/RHEAGAEHR r")
--     print (listMoves "rheaga1hr/9/1c2e2c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/9/RHEAGAEHR r")
--     print (listMoves "rhe2a1hr/9/1c1aeg1c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/3GA4/RHE2AEHR r")
--     print (listMoves "1Ceaga1hr/9/r8/scs5s/2e3S2/c7S/9/4E2C1/9/RH1AGAEHR r")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rhea1a1hr/9/1c2eg1c1/s1s1s3s/6s2/1C2S4/S1S3S1S/3G3C1/9/RHEA1AEHR r")
--     print (listMoves "rheagae1r/4h4/1c5c1/s3s3s/2s1S1s2/9/SCS3S1S/2H4C1/9/R1EAGAEHR r")
--     print (listMoves "rheagaehr/9/1c7/s1s1s1s1s/9/S1S4c1/4S1S1S/1C5C1/9/RHEAGAEHR b")
--     print (listMoves "rheaga1hr/9/1c2e4/s1sSs3s/9/c4s3/4S1S1S/1C2E2C1/9/RH1AGAEHR r")
--     print (listMoves "rheagaeCr/9/2c6/s1s1s1s1s/9/2S4c1/S3S1S1S/3C5/9/RHEAGAEHR b")
--     print (listMoves "rheagaeCr/9/2c6/s1s1s1s1s/9/2S4c1/S3S1S1S/3C5/9/RHEAGAEHR b")
--     print (listMoves "rheagaehr/9/1c5c1/s3s1s1s/2s6/S8/2S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rheagaeh1/9/1c7/4r4/s1s3s1s/9/9/4s4/9/3G3c1 b")
--     print (listMoves "rhea1a1hr/9/1c2eg1c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/3G5/RHEA1AEHR b")
--     print (listMoves "rhea1a1hr/4g4/1c2e2c1/s1s1s3s/6s2/1C2S4/S1S3S1S/7C1/3G5/RHEA1AEHR b")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r")
--     print (listMoves "rheaga1h1/4C4/4c1r2/7cs/s1s6/9/S1S3SCS/R8/9/1HEAGAEHR b")
--     print (listMoves "rhea1a1h1/4g4/1c2r4/s6cs/1Cs3s2/9/S1S3SCS/R8/4A4/1HE1GAEHR r")
--     print (listMoves "r1e1ga1h1/4a4/2h1c1r2/7cs/s1s6/2S6/S5SCS/R1H6/9/2EAGAEHR b")
--     print (listMoves "rhe1ga1h1/4a4/4c1r2/7cs/s1s6/9/S1S3SCS/R1H6/9/2EAGAEHR b")
--     print (listMoves "r1e1ga1h1/4a4/4c1r2/7cs/sh7/1Hs6/S5SCS/R8/9/2EAGAEHR r")
--     print (listMoves "rhea1a1h1/4g4/1c2r4/7cs/s1s1C4/9/S1S3SCS/R8/4A4/1HE1GAEHR b")
--     print (listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r")
    