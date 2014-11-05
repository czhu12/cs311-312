empty = '_'
testboard1 = parseBoard "___B___"
testboard2 = parseBoard "BBB_____________WWW"


removeEmptyLists :: (Eq a) => [[a]] -> [[a]]
removeEmptyLists listOfLists 
  | null listOfLists                      = []
  | (head listOfLists) == []              = removeEmptyLists (tail listOfLists)
  | otherwise                             = (head listOfLists) : removeEmptyLists (tail listOfLists)

swap :: (Int, Int) -> (Int, Int) -> [[Char]] -> [[Char]]
swap pos1 pos2 board = secondBoard
  where 
    firstLetter = letterAtPosition pos1 board
    secondLetter = letterAtPosition pos2 board
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

isOutOfBounds :: (Int, Int) -> [[Char]] -> Bool
isOutOfBounds pos board
  | x < 0 = True
  | y < 0 = True
  | otherwise = ((length board) <= y) || ((length (board!!y)) <= x)
  where 
    x = fst pos
    y = snd pos
      

letterAtPosition :: (Int, Int) -> [[Char]] -> Char
letterAtPosition pos board 
  | isOutOfBounds (x, y) board         = error "trying to get letter at out of bounds position"
  | otherwise                       = board !! y !! x
  where 
    x = fst pos
    y = snd pos

moveLeft :: (Int, Int) -> [String] -> (Int, Int)
moveLeft pos board = (x - 1, y)
  where 
    x = fst pos
    y = snd pos
  
moveRight :: (Int, Int) -> [String] -> (Int, Int)
moveRight pos board = (x + 1, y)
  where 
    x = fst pos
    y = snd pos

moveDownLeft :: (Int, Int) -> [String] -> (Int, Int)
moveDownLeft pos board
	| isUpperHalf pos board	    = (x - 1, y + 1)
	| otherwise										= (x, y + 1)
	where 
		x = fst pos
		y = snd pos

moveDownRight :: (Int, Int) -> [String] -> (Int, Int)
moveDownRight pos board
	| isUpperHalf pos board 			= (x, y + 1)
	| otherwise										= (x + 1, y + 1)
	where 
		x = fst pos
		y = snd pos

moveUpLeft :: (Int, Int) -> [String] -> (Int, Int)
moveUpLeft pos board
	| isBottomHalf pos board			= (x, y - 1)
	| otherwise										= (x - 1, y - 1)
	where 
		x = fst pos
		y = snd pos

moveUpRight :: (Int, Int) -> [String] -> (Int, Int)
moveUpRight pos board
	| isBottomHalf pos board 		  = (x + 1, y - 1)
	| otherwise										= (x, y - 1)
	where 
		x = fst pos
		y = snd pos

isBottomHalf :: (Int, Int) -> [String] -> Bool
isBottomHalf pos board = not (isUpperHalf pos board)

isUpperHalf :: (Int, Int) -> [String] -> Bool
isUpperHalf pos board = (length board) `div` 2 >= (snd pos)

--"AABBBAA"
parseBoard :: String -> [String]
parseBoard rawBoard = parseBoard' rawBoard degree 0
  where degree = degreeOfRawBoard rawBoard

parseBoard' :: String -> Int -> Int -> [String]
parseBoard' rawBoard degree row
  | elementCount == degree && row /= 0      = [rawBoard]
  | otherwise                               = (take elementCount rawBoard) : 
                                              parseBoard' (drop elementCount rawBoard) degree (row + 1)
  where elementCount = getNumElemInRow degree row

getNumElemInRow degree row 
  | row > midpoint            = degree + row - 2 * (row - midpoint)
  | otherwise                 = degree + row
  where midpoint = getMidPointOfDegree degree

getMidPointOfDegree degree
  | degree == 1           = 1
  | otherwise             = degree - 1

unparseBoard :: [String] -> String
unparseBoard board 
  | null board            = ""
  | otherwise             = head board ++ unparseBoard (tail board)

degreeOfRawBoard :: String -> Int
degreeOfRawBoard rawBoard = degreeOfRawBoard' rawBoard 0

degreeOfRawBoard' rawBoard curr
  | currLength == length rawBoard             = curr
  | otherwise                           = degreeOfRawBoard' rawBoard (curr + 1)
  where currLength = 3 * (curr ^ 2) - 3 * curr + 1

generateStatesForPos pos board = (generateStatesForPos' pos board)
generateStatesForPos' pos board
  | isOutOfBounds pos board         = []
  | otherwise                       = [(tryMove pos letter moveUpLeft board),    
                                       (tryMove pos letter moveUpRight board), 
                                       (tryMove pos letter moveDownLeft board),
                                       (tryMove pos letter moveDownRight board),
                                       (tryMove pos letter moveLeft board),
                                       (tryMove pos letter moveRight board)]
  where 
    letter = letterAtPosition pos board
    
tryMove pos letter moveFunc board 
  | isOutOfBounds movedToPos board                     = []      -- Try failed.
  | movedToLetter == empty                              = swap pos movedToPos board
  -- Here we need to check if we move that direction again, whether or not the letter is opposite.
  | movedToLetter == letter                             = if canHopOver then swap (moveFunc movedToPos board) pos board else []
  | otherwise                                           = []
  where 
    movedToPos = moveFunc pos board
    movedToLetter = (letterAtPosition movedToPos board)
    canHopOver = canHop letter movedToPos moveFunc board
 
canHop letter movedToPos moveFunc board
  | isOutOfBounds hopToPos board       = False
  | otherwise                               = hopToLetter /= empty && hopToLetter /= letter 
  where 
    hopToPos = moveFunc movedToPos board
    hopToLetter = letterAtPosition hopToPos board


-- Time to write the score functionality --

-- Score takes a letter and a board and an n which represents how many letters this game starts out with
-- It will judge the score of the board relative to the letter.
-- If the letter won, score will return 10, if the letter lost, the score will return -10, if neither, it will return 0.
score letter board n = score' letter board n (0, 0)
score' board letter n pos
  | countLetters letter board < n / 2           = -10
  | generateStatesForLetter letter board == []  = -10
  | otherwise                                   = 0

generateStatesForLetter letter board = generateStatesForLetter' letter board (0, 0)
generateStatesForLetter' letter board pos 
  | isOutOfBounds pos board                 = []
  | letterAtPosition pos board == letter    = (generateStatesForPos pos board) ++ (generateStatesForLetter' letter board next)
  | otherwise                               = generateStatesForLetter' letter board next
  where next = nextPos next board

countLetters letter board = countLetters' letter board (0, 0)
countLetters' letter board pos
  | isOutOfBounds pos board                             = 0
  | letterAtPosition pos board == letter          = 1 + countLetters' letter board next
  | otherwise                                     = countLetters' letter board next
  where next = nextPos pos board

nextPos :: (Int, Int) -> [[Char]] -> (Int, Int)
nextPos pos board
  | x >= length (board !! y) -1     = (0, y + 1)
  | otherwise                       = (x + 1, y)
  where 
    x = fst pos
    y = snd pos
