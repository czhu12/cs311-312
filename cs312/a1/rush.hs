-- Constants
empty = '-'

-- Tests
nosolution = ["---A", 
              "---A", 
              "XX-A", 
              "---A"]

goal = ["----", 
        "----", 
        "--XX", 
        "----"]
nogoal = ["----", 
          "----", 
          "-XX-", 
          "----"]
nogoal2 = ["----", 
           "XX--", 
           "----", 
           "----"]
nogoal3 = ["----", 
           "XX-A", 
           "---A", 
           "----"]
nogoal4 = ["-----", 
           "XX-BA", 
           "---BA", 
           "----B"]

nogoal5 = ["----B-", 
           "----B-", 
           "--XXB-", 
           "---C--",
           "---CDD",
           "------"]

solve :: [[Char]] -> [[[Char]]]
solve board = reverse (statesearch [board] [])

printSolution :: [[[Char]]] -> IO()
printSolution [] = putStrLn "No solution could be found."
printSolution solution = putStrLn (formatSolutions (solution))

formatSolutions :: [[[Char]]] -> [Char]
formatSolutions solutions
  | null solutions          = ""
  | otherwise               = (formatSolutionState state) ++ (formatSolutions (tail solutions))
  where
    state = (head solutions)

formatSolutionState :: [[Char]] -> [Char]
formatSolutionState state 
  | null state            = "\n\n"
  | otherwise             = row ++ "\n" ++ (formatSolutionState (tail state))
  where row = (head state)


statesearch :: [[[Char]]] -> [[[Char]]] -> [[[Char]]]
statesearch unexplored path
  | null unexplored                     = []
  | goalFound (head unexplored)    = (head unexplored):path
  | elem (head unexplored) path         = statesearch (tail unexplored) path
  | (not (null newstates))              = newstates
  | otherwise                           =
      statesearch (tail unexplored) path
    where newstates = statesearch 
                        (generateNewStates (head unexplored))
                        ((head unexplored):path)

generateNewStates :: [[Char]] -> [[[Char]]]
generateNewStates b = removeEmptyLists (generateNewStates' b [] (0, 0))

-- if the end of the target car's x position is equal to the x position of the end of the board that means we win.

generateNewStates' :: [[Char]] -> [Char] -> (Int, Int) -> [[[Char]]]
generateNewStates' board seenCars pos
  | isOutOfBounds (fst pos) (snd pos) board = []
  | currentLetter == empty                  = generateNewStates' board seenCars (nextPos pos board)
  | not (elem currentLetter seenCars)       = (generateStatesForLetter currentLetter board) ++ (generateNewStates' board (currentLetter:seenCars) (nextPos pos board))
  | otherwise                               = generateNewStates' board seenCars (nextPos pos board)
  where 
    currentLetter = (letterAtPosition (fst pos) (snd pos) board)

generateStatesForLetter :: Char -> [[Char]] -> [[[Char]]]
generateStatesForLetter letter board
  | isVertical letter board           = [(move (0, -1) letter board) ,  (move (0, 1) letter board)]
  | otherwise                         = [(move (-1, 0) letter board) , (move (1, 0) letter board)]

-- How should we approach this? If isVertical, then we should find the top and bottom bounds of the object.
-- if is not vertical then we should find the left and right bounds. 
-- To move right: we swap the value of furthest left with 1 + furthest right
-- To move left: we swap the value of furthest right with 1 + furthest left
-- To move up: we swap the value of furthest down with 1 + furthest up
-- To move down: we swap the value of furthest up with 1 + furthest down

swap :: (Int, Int) -> (Int, Int) -> [[Char]] -> [[Char]]
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

isVertical :: Char -> [[Char]] -> Bool
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

isHorizontal :: Char -> [[Char]] -> Bool
isHorizontal letter board = not (isVertical letter board)

firstLetterPos :: Char -> [[Char]] -> (Int, Int)
firstLetterPos letter board = firstLetterPos' letter board (0, 0)

firstLetterPos' :: Char -> [[Char]] -> (Int, Int) -> (Int, Int)
firstLetterPos' letter board pos
  | isOutOfBounds (fst pos) (snd pos) board                     = error "firstLetterPos': searched in a place that is out of bounds"
  | (letterAtPosition (fst pos) (snd pos) board) == letter      = pos
  | otherwise                                                   = firstLetterPos' letter board (nextPos pos board)

nextPos :: (Int, Int) -> [[Char]] -> (Int, Int)
nextPos pos board
  | x >= length (head board) -1     = (0, y + 1)
  | otherwise                       = (x + 1, y)
  where 
    x = fst pos
    y = snd pos

isOutOfBounds :: Int -> Int -> [[Char]] -> Bool
isOutOfBounds x y board
  | x < 0 = True
  | y < 0 = True
  | otherwise = ((length board) <= y) || ((length (board!!0)) <= x)

-- firstPos should be the first position of the letter in the board and so it must be the 
-- top of the Car if vertical or the left of the car if horizontal
-- position range will always be (firstPos, furtherRange)
positionRange :: (Int, Int) -> Bool -> [[Char]] -> ((Int, Int), (Int, Int))
positionRange firstPos isVert board 
  | isVert            = (firstPos, (goDown letter firstPos board))
  | otherwise         = (firstPos, (goRight letter firstPos board))
  where letter = letterAtPosition (fst firstPos) (snd firstPos) board

goDown :: Char -> (Int, Int) -> [[Char]] -> (Int, Int)
goDown letter pos board 
  | (isOutOfBounds (fst pos) (snd pos) board) || (letter /= letterHere)     = ((fst pos), (snd pos) - 1)
  | otherwise                                                               = goDown letter ((fst pos), (snd pos) + 1) board
  where letterHere = letterAtPosition (fst pos) (snd pos) board

goRight :: Char -> (Int, Int) -> [[Char]] -> (Int, Int)
goRight letter pos board 
  | (isOutOfBounds (fst pos) (snd pos) board) || (letter /= letterHere)     = ((fst pos) - 1, (snd pos))
  | otherwise                                                               = goRight letter ((fst pos) + 1, (snd pos)) board
  where letterHere = letterAtPosition (fst pos) (snd pos) board

letterAtPosition :: Int -> Int -> [[Char]] -> Char
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

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos pos1 pos2 = (((fst pos1) + (fst pos2)), ((snd pos1) + (snd pos2)))

move :: (Int, Int) -> Char -> [[Char]] -> [[Char]]
move (x, y) letter board
  | canMove (x, y) letter board                = swap swapPos1 swapPos2 board
  | otherwise                                  = []
  where
    firstPos = firstLetterPos letter board
    isVert = isVertical letter board
    range = positionRange firstPos isVert board
    relevantRange = (snd range)
    swapPos1 = if (x + y) > 0 then firstPos else addPos firstPos (x, y)
    swapPos2 = if (x + y) > 0 then addPos relevantRange (x, y) else relevantRange 

removeEmptyLists :: (Eq a) => [[a]] -> [[a]]
removeEmptyLists listOfLists 
  | null listOfLists                      = []
  | (head listOfLists) == []              = removeEmptyLists (tail listOfLists)
  | otherwise                             = (head listOfLists) : removeEmptyLists (tail listOfLists)

goalFound :: [[Char]] -> Bool
goalFound state = fst endOfTarget == fst (boundsOfBoard state)
  where 
    rangeOfTarget = positionRange (firstLetterPos 'X' state) False state
    endOfTarget = snd rangeOfTarget

boundsOfBoard :: [[Char]] -> (Int, Int)
boundsOfBoard board = (length (board!!0) - 1, (length board) - 1)
