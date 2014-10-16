empty = '-'
a = ["---", "--A", "--A", "---"]
b = ["--A", "--A", "---"]
c = ["---", "-AA", "---"]

swap pos1 pos2 board = secondBoard
  where 
    firstLetter = letterAtPosition (fst pos1) (snd pos1) board
    secondLetter = letterAtPosition (fst pos2) (snd pos2) board
    firstBoard = replaceLetter pos2 firstLetter board 
    secondBoard = replaceLetter pos1 secondLetter firstBoard 

replaceLetter :: (Int, Int) -> Char -> [[Char]] -> [[Char]]
replaceLetter at withLetter board 
  | (snd at) == 0         = (replaceNth (fst at) withLetter (head board)):(tail board)
  | otherwise             = (head board):(replaceLetter ((fst at), ((snd at) - 1)) withLetter (tail board))

replaceNth :: Int -> Char -> [Char] -> [Char]
replaceNth n newVal (x:xs)
     | n == 0    = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

isVertical letter board
  | (letterAtPosition x (y + 1) board) == letter        = True
  | (letterAtPosition x (y - 1) board) == letter        = True
  | (letterAtPosition (x + 1) y board) == letter        = False
  | (letterAtPosition (x - 1) y board) == letter        = False
  | otherwise                                           = error "isVertical: can't find vertical or horizontal"
  where 
    firstPos = firstLetterPos letter board
    x        = fst firstPos
    y        = snd firstPos

isHorizontal letter board = not (isVertical letter board)

firstLetterPos letter board = firstLetterPos' letter board (0, 0)

firstLetterPos' letter board pos
  | isOutOfBounds (fst pos) (snd pos) board                     = error "firstLetterPos': searched in a place that is out of bounds"
  | (letterAtPosition (fst pos) (snd pos) board) == letter      = pos
  | otherwise                                                   = firstLetterPos' letter board (nextPos pos board)

nextPos pos board
  | x >= length (head board) -1     = (0, y + 1)
  | otherwise                       = (x + 1, y)
  where 
    x = fst pos
    y = snd pos

isOutOfBounds x y board = ((length board) <= y) || ((length (board!!0)) <= x)

--canMove :: (Int, Int) -> Char -> [[Char]] -> Bool
--canMove dir letter board = letterAtPosition (fst checkPos) (snd checkPos) board == empty
--  where 
--    firstPos = firstLetterPos letter board
--    x = (fst firstPos)
--    y = (snd firstPos)
--    dirX = (fst dir)
--    dirY = (snd dir)
--    checkPos = ((x + dirX), (y + dirY))

-- firstPos should be the first position of the letter in the board and so it must be the 
-- top of the Car if vertical or the left of the car if horizontal
-- position range will always be (firstPos, furtherRange)
positionRange :: (Int, Int) -> Bool -> [[Char]] -> ((Int, Int), (Int, Int))
positionRange firstPos isVert board 
  | isVert            = (firstPos, (goDown letter firstPos board))
  | otherwise         = (firstPos, (goRight letter firstPos board))
  where letter = letterAtPosition (fst firstPos) (snd firstPos) board

goDown letter pos board 
  | (isOutOfBounds (fst pos) (snd pos) board) || (letter /= letterHere)     = ((fst pos), (snd pos) - 1)
  | otherwise                                                               = goDown letter ((fst pos), (snd pos) + 1) board
  where letterHere = letterAtPosition (fst pos) (snd pos) board

goRight letter pos board 
  | (isOutOfBounds (fst pos) (snd pos) board) || (letter /= letterHere)     = ((fst pos) - 1, (snd pos))
  | otherwise                                                               = goRight letter ((fst pos) + 1, (snd pos)) board
  where letterHere = letterAtPosition (fst pos) (snd pos) board

letterAtPosition x y board 
  | isOutOfBounds x y board         = error "trying to get letter at out of bounds position"
  | otherwise                       = board !! y !! x

canMove :: (Int, Int) -> Char -> [[Char]] -> Bool
canMove (x, y) letter board = (not (isOutOfBounds (fst checkPos) (snd checkPos) board)) && letterAtPosition (fst checkPos) (snd checkPos) board == empty
  where 
    firstPos = firstLetterPos letter board
    isVert = isVertical letter board
    range = positionRange firstPos isVert board
    relevantRange = if (x + y) < 0 then (fst range) else (snd range) -- this logic is incorrect
    checkPos = addPos relevantRange (x, y)

addPos pos1 pos2 = (((fst pos1) + (fst pos2)), ((snd pos1) + (snd pos2)))

move (x, y) letter board
  | canMove (x, y) letter board                = swap swapPos1 swapPos2 board
  | otherwise                                  = []
  where
    firstPos = firstLetterPos letter board
    isVert = isVertical letter board
    range = positionRange firstPos isVert board
    relevantRange = if y == 0 then (fst range) else (snd range)
    swapPos1 = if (x + y) > 0 then firstPos else addPos firstPos (x, y)
    swapPos2 = if (x + y) < 0 then relevantRange else addPos relevantRange (x, y)
    checkPos = addPos relevantRange (x, y)

generateStatesForLetter letter board
  | isVertical letter board           = [(move (0, -1) letter board) ,  (move (0, 1) letter board)]
  | otherwise                         = [(move (-1, 0) letter board) , (move (1, 0) letter board)]
