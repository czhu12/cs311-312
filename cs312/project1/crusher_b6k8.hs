empty = '_'
testboard1 = parseBoard "___B___"
testboard2 = parseBoard "B__B___"
testboard3 = parseBoard "B__B__W"
testboard4 = parseBoard "B______"
testboard5 = parseBoard "BBBBBBW"
testboard6 = parseBoard "BBB_____________WWW"
testboard7 = parseBoard "BBBWBBB"
testboard8 = parseBoard "BBB_BB_______WW_WWW"

main = print (crusher_b6k8 [unparseBoard testboard8] 'B' 4 3)

crusher_b6k8 :: [String] -> Char -> Int -> Int -> [String]
crusher_b6k8 unparsedStates player depth degree = minimax unparsedStates player depth degree

-- =================================================== Minimax ========================================

-- This function needs to take the board and parse it to the internal representation of a board. 
-- Then after the minimax algorithm runs, it needs to unparse it back to the external representation.
minimax :: [String] -> Char -> Int -> Int -> [String]
minimax unparsedStates player depth degree = unparsedSolution
  where 
    parsedStates = (map parseBoard unparsedStates)
    toTakeCount = (length unparsedStates) + 1
    --parsedSolution = takeLastN (minimax' parsedStates player depth degree player) toTakeCount
    parsedSolution =  (minimax' parsedStates player depth degree player) 
    unparsedSolution = map unparseBoard parsedSolution

-- Performs the actual minimax algorithm
minimax' :: [[String]] -> Char -> Int -> Int -> Char -> [[String]]
minimax' states letter depth degree player
  | depth == 0                                  = states
  | boardScore == -10                           = states --lost
  | boardScore == 10                            = states --won
  | player == letter                            = getMaximum letter minimaxPaths degree
  | player /= letter                            = getMinimum letter minimaxPaths degree
  | otherwise                                   = error "How did I get here?" 
  where 
    board = (head states)
    pastStates = (tail states)
    boardScore = (score letter degree pastStates board)
    childrenStates = generateStatesForLetter letter board
    minimaxPaths = removeEmptyLists (minimaxOnAll childrenStates states (oppositeLetter letter) (depth - 1) degree player)
    -- here we wanna call minimax on each of the children, flipping the letter and appending the current board

minimaxOnAll :: [[String]] -> [[String]] -> Char -> Int -> Int -> Char -> [[[String]]]
minimaxOnAll childrenStates states letter depth degree player
  | null childrenStates           = []
  | otherwise                     = minimaxResults : minimaxOnAll (tail childrenStates) states letter depth degree player
  where 
    currentChildState = head childrenStates
    minimaxPath = currentChildState : states
    minimaxResults = minimax' minimaxPath letter depth degree player
 
-- Gets the max state in a list of states.
getMaximum :: Char -> [[[String]]] -> Int -> [[String]]
getMaximum letter statePaths n = (getMaxOrMin' letter statePaths n (-100000) [[[]]] True)

-- Gets the min state in a list of states.
getMinimum :: Char -> [[[String]]] -> Int -> [[String]]
getMinimum letter statePaths n = (getMaxOrMin' letter statePaths n 100000 [[[]]] False)

getMaxOrMin' :: Char -> [[[String]]] -> Int -> Int -> [[String]] -> Bool -> [[String]]
getMaxOrMin' letter statePaths n maxScoreSoFar maxStatePathSoFar isMax
  | null statePaths                               = maxStatePathSoFar
  | isMax && boardScore > maxScoreSoFar           = getMaxOrMin' letter (tail statePaths) n boardScore currentStatePath isMax
  | (not isMax) && boardScore < maxScoreSoFar     = getMaxOrMin' letter (tail statePaths) n boardScore currentStatePath isMax
  | otherwise                                     = getMaxOrMin' letter (tail statePaths) n maxScoreSoFar maxStatePathSoFar isMax
  where 
    currentStatePath = (head statePaths)
    lastBoard = (head currentStatePath)
    pastStates = (tail currentStatePath)
    boardScore = score letter n pastStates lastBoard

-- =================================================== Move Generation ========================================

generateStatesForLetter :: Char -> [String] -> [[String]]
generateStatesForLetter letter board = generateStatesForLetter' letter board (0, 0)
generateStatesForLetter' letter board pos 
  | isOutOfBounds pos board                 = []
  | letterAtPosition pos board == letter    = (generateStatesForPos pos board) ++ (generateStatesForLetter' letter board next)
  | otherwise                               = generateStatesForLetter' letter board next
  where next = nextPos pos board

-- Generates all states for a position.
generateStatesForPos pos board =  removeEmptyLists (generateStatesForPos' pos board)
generateStatesForPos' pos board
  | isOutOfBounds pos board         = []
  | otherwise                       = [(tryMove pos moveUpLeft board),    
                                       (tryMove pos moveUpRight board), 
                                       (tryMove pos moveDownLeft board),
                                       (tryMove pos moveDownRight board),
                                       (tryMove pos moveLeft board),
                                       (tryMove pos moveRight board)]
    
-- This will try to move the piece at a position in a direction, if it succeeds, it will return the new board, else it will return []
tryMove pos moveFunc board 
  | isOutOfBounds movedToPos board                     = []      -- Try failed.
  | movedToLetter == empty                              = swap pos movedToPos board
  -- Here we need to check if we move that direction again, whether or not the letter is opposite.
  | movedToLetter == letter                             = if canHopOver then swap hopToPos pos killedLetterBoard else []
  | otherwise                                           = []
  where 
    letter = letterAtPosition pos board
    movedToPos = moveFunc pos board
    movedToLetter = (letterAtPosition movedToPos board)
    canHopOver = canHop letter movedToPos moveFunc board
    hopToPos = (moveFunc movedToPos board)
    killedLetterBoard = (replaceLetter hopToPos empty board)
 
-- This checks if a piece is able to hop over another piece in a given direction
canHop letter movedToPos moveFunc board
  | isOutOfBounds hopToPos board       = False
  | otherwise                               = hopToLetter /= empty && hopToLetter /= letter 
  where 
    hopToPos = moveFunc movedToPos board
    hopToLetter = letterAtPosition hopToPos board

removeEmptyLists :: (Eq a) => [[a]] -> [[a]]
removeEmptyLists listOfLists 
  | null listOfLists                      = []
  | (head listOfLists) == []              = removeEmptyLists (tail listOfLists)
  | otherwise                             = (head listOfLists) : removeEmptyLists (tail listOfLists)

-- Swaps two pieces on the board
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

isMiddle :: (Int, Int) -> [String] -> Bool
isMiddle pos board = (snd pos) == quot (length board) 2

-- ==================================================== Move Direction Functions ======================================
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
  | isMiddle pos board          = (x - 1, y + 1)
	| isBottomHalf pos board	    = (x - 1, y + 1)
	| otherwise										= (x, y + 1)
	where 
		x = fst pos
		y = snd pos

moveDownRight :: (Int, Int) -> [String] -> (Int, Int)
moveDownRight pos board
  | isMiddle pos board          = (x, y + 1)
	| isBottomHalf pos board 			= (x, y + 1)
	| otherwise										= (x + 1, y + 1)
	where 
		x = fst pos
		y = snd pos

moveUpLeft :: (Int, Int) -> [String] -> (Int, Int)
moveUpLeft pos board
  | isMiddle pos board          = (x - 1, y - 1)
	| isBottomHalf pos board			= (x, y - 1)
	| otherwise										= (x - 1, y - 1)
	where 
		x = fst pos
		y = snd pos

moveUpRight :: (Int, Int) -> [String] -> (Int, Int)
moveUpRight pos board
  | isMiddle pos board          = (x, y - 1)
	| isBottomHalf pos board 		  = (x + 1, y - 1)
	| otherwise										= (x, y - 1)
	where 
		x = fst pos
		y = snd pos

-- Checks if a position is bottom half of a board
isBottomHalf :: (Int, Int) -> [String] -> Bool
isBottomHalf pos board = not (isUpperHalf pos board)

-- Checks if a position is upper half of a board
isUpperHalf :: (Int, Int) -> [String] -> Bool
isUpperHalf pos board = (length board) `div` 2 >= (snd pos)

-- Parse the board to internal representation
parseBoard :: String -> [String]
parseBoard rawBoard = parseBoard' rawBoard degree 0
  where degree = degreeOfRawBoard rawBoard

parseBoard' :: String -> Int -> Int -> [String]
parseBoard' rawBoard degree row
  | elementCount == degree && row /= 0      = [rawBoard]
  | otherwise                               = (take elementCount rawBoard) : 
                                              parseBoard' (drop elementCount rawBoard) degree (row + 1)
  where elementCount = getNumElemInRow degree row

-- Figures out how many element be in row of a board of with some degree 
getNumElemInRow :: Int -> Int -> Int
getNumElemInRow degree row 
  | row > midpoint            = degree + row - 2 * (row - midpoint)
  | otherwise                 = degree + row
  where midpoint = getMidPointOfDegree degree

getMidPointOfDegree :: Int -> Int
getMidPointOfDegree degree
  | degree == 1           = 1
  | otherwise             = degree - 1

-- Unparses board to external representation
unparseBoard :: [String] -> String
unparseBoard board 
  | null board            = ""
  | otherwise             = head board ++ unparseBoard (tail board)

-- Figures out the degree of a raw board
degreeOfRawBoard :: String -> Int
degreeOfRawBoard rawBoard = degreeOfRawBoard' rawBoard 0

degreeOfRawBoard' :: String -> Int -> Int
degreeOfRawBoard' rawBoard curr
  | currLength == length rawBoard             = curr
  | otherwise                           = degreeOfRawBoard' rawBoard (curr + 1)
  where currLength = 3 * (curr ^ 2) - 3 * curr + 1


-- =================================================== Score ========================================
-- Time to write the score functionality --

-- Score takes a letter and a board and an n which represents how many letters this game starts out with
-- It will judge the score of the board relative to the letter.
-- If the letter won, score will return 10, if the letter lost, the score will return -10, if neither, it will return 
-- the difference between the letter being scored and the other letter.
-- Has to score both letters to figure out score.
score :: Char -> Int -> [[String]] -> [String] -> Int
score letter n pastStates board 
  | (scoreLetter letter n pastStates board) == -10                       = -10
  | (scoreLetter (oppositeLetter letter) n pastStates board) == -10      = 10
  | otherwise                                                            = (countLetters letter board) - (countLetters (oppositeLetter letter) board)

-- Scores only a single letter, can only tell if it lost.
scoreLetter :: Char -> Int -> [[String]] -> [String] -> Int
scoreLetter letter n pastStates board = scoreLetter' letter n pastStates (0, 0) board
scoreLetter' letter n pastStates pos board
  | countLetters letter board <= n - 1          = -10
  | nonRedundantStates == []                    = -10
  | otherwise                                   = (countLetters letter board) - (countLetters (oppositeLetter letter) board)
  where 
    nextPossibleStates = generateStatesForLetter letter board
    nonRedundantStates = nonRedundant nextPossibleStates pastStates

-- Gets all the non redundant states by going through the states and 
-- discarding the states that are also in the pastStates
nonRedundant :: [[String]] -> [[String]] -> [[String]]
nonRedundant states pastStates 
  | null states                     = []
  | elem (head states) pastStates   = nonRedundant (tail states) pastStates
  | otherwise                       = (head states) : (nonRedundant (tail states) pastStates)

-- Counts how many times the letter appears on the board.
countLetters :: Char -> [String] -> Int
countLetters letter board = countLetters' letter board (0, 0)

countLetters' :: Char -> [String] -> (Int, Int) -> Int
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

takeLastN :: [[String]] -> Int -> [[String]]
takeLastN list n
  | null list         = []
  | n == 0            = []
  | otherwise         = (head list) : takeLastN (tail list) (n - 1)

oppositeLetter letter = if letter == 'B' then 'W' else 'B'
