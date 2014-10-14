EMPTY = "-"
solve board = statesearch [board] (generateGoal board) []
 
statesearch unexplored path
  | null unexplored                     = []
  | goalFound (head unexplored)    = (head unexplored):path
  | elem (head unexplored) path         = statesearch (tail unexplored)
  | (not (null newstates))              = newstates
  | otherwise                           =
      statesearch (tail unexplored) path
    where newstates = statesearch 
                        (generateNewStates (head unexplored))
                        ((head unexplored):path)

-- given a state that is like...
-- ["--A",
--  "--A",
--  "---"]
--  and generate new states
--

generateNewStates board = generateNewStates' board [] (0 0)

generateNewStates' board seenCars pos
  | isOutOfBounds (fst pos) (snd pos) board = []
  | currentLetter == EMPTY                  = generateNewStates board seenCars (nextPos pos board)
  | not (elem currentLetter seenCars)       = (generateStatesForLetter currentLetter board) 
    ++ generateNewStates board (currentLetter:seenCars) (nextPos pos board)
  | otherwise generateNewStates board (currentLetter:seenCars) (nextPos pos board)
  where 
    currentLetter = (letterAtPosition (fst pos) (snd pos) board)

generateStatesForLetter letter board
  | isVertical letter board           = (moveLeftTry letter board firstPos) ++ (moveRightTry letter board firstPos)
  | otherwise                         = (moveUpTry letter board firstPos) ++ (moveDownTry letter board firstPos)
  where 
    firstPos = firstLetterPos letter board

-- How should we approach this? If isVertical, then we should find the top and bottom bounds of the object.
-- if is not vertical then we should find the left and right bounds. 
-- To move right: we swap the value of furthest left with 1 + furthest right
-- To move left: we swap the value of furthest right with 1 + furthest left
-- To move up: we swap the value of furthest down with 1 + furthest up
-- To move down: we swap the value of furthest up with 1 + furthest down

-- Swap Function:
-- Basically regenerate board with new values
swap board pos1 pos2 = secondBoard
  where 
    firstLetter = letterAtPosition (fst pos1) (snd pos1) board
    secondLetter = letterAtPosition (fst pos2) (snd pos2) board
    firstBoard = replaceLetter pos1 firstLetter board 
    secondBoard = replaceLetter pos1 secondLetter firstBoard 
-- firstPos means that the rest of the car must be further down or further right
-- To move left, we should check if the place immediately before the firstPos is empty, if so
-- we find the end of the car and set that to a '-'

moveLeftTry letter board firstPos
  | and -- and (not out of bounds) (is free)
      (not (isOutOfBounds ((fst firstPos) - 1) (snd firstPos) board))
      ((letterAtPosition ((fst firstPos) -1) (snd firstPos) board) == EMPTY)
              = moveLeft letter board firstPos
  | otherwise = []


moveLeft letter board firstPos
  | and (replaceLetter startPosition withLetter board)
        (replaceLetter endPosition EMPTY board)
  where 
    withLetter = letterAtPosition (fst firstPos) (snd firstPos) board
    startPosition = (((fst firstPos) - 1) (snd firstPos))
    endPosition = farRightPosition firstPos board

-- Ok so you left off here.... to move left you need to replace the position immediately left of the firstPos with the letter at firstPos, then you need to find the farest right of the letters of firstPos and replace that with '-'

moveRightTry letter board firstPos
  | 

moveUpTry letter board firstPos
  |

moveDownTry letter board firstPos
  |


isVertical letter board
  | (letterAtPosition x y + 1 board) == letter        = True
  | (letterAtPosition x y - 1 board) == letter        = True
  | (letterAtPosition x + 1 y board) == letter        = False
  | (letterAtPosition x - 1 y board) == letter        = False
  | otherwise                                   = error "isVertical: can't find vertical or horizontal"
  where 
    firstPos = firstLetterPos letter board
    x        = fst firstPos
    y        = snd firstPos

isHorizontal letter board = not (isVertical letter board)

firstLetterPos letter board = firstLetterPos' letter board (0, 0)

firstLetterPos' letter board pos
  | isOutOfBounds (fst pos) (snd pos) board                     = error "firstLetterPos': searched in a place that is out of bounds"
  | (letterAtPosition (fst pos) (snd pos) board) == letter      = pos
  | otherwise                                                   = firstLetterPos' letter board (nextPos pos)

nextPos pos board
  | x >= length (head board)        = (0, y + 1)
  | otherwise                       = (x + 1, y)
  where 
    x = fst pos
    y = snd pos

letterAtPosition x y board = board !! y !! x

isOutOfBounds x y board = or ((length board) >= y) ((length (board!!0)) >= x)

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

replaceLetter at withLetter board 
  | (snd at) == 0         = replaceNth (fst at) withLetter (head board):(tail board)
  | otherwise             = (head board):(replaceLetter at withLetter (tail board))

