swap board pos1 pos2 = secondBoard
  where 
    firstLetter = letterAtPosition (fst pos1) (snd pos1) board
    secondLetter = letterAtPosition (fst pos2) (snd pos2) board
    firstBoard = replaceLetter pos1 firstLetter board 
    secondBoard = replaceLetter pos1 secondLetter firstBoard 

letterAtPosition x y board = board !! y !! x

replaceLetter at withLetter board 
  | (snd at) == 0         = replaceNth (fst at) withLetter (head board):(tail board)
  | otherwise             = (head board):(replaceLetter at withLetter (tail board))

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

isVertical letter board
  | (letterAtPosition x (y + 1) board) == letter        = True
  | (letterAtPosition x (y - 1) board) == letter        = True
  | (letterAtPosition (x + 1) y board) == letter        = False
  | (letterAtPosition (x - 1) y board) == letter        = False
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
  | otherwise                                                   = firstLetterPos' letter board (nextPos pos board)

nextPos pos board
  | x >= length (head board)        = (0, y + 1)
  | otherwise                       = (x + 1, y)
  where 
    x = fst pos
    y = snd pos

isOutOfBounds x y board = ((length board) >= y) || ((length (board!!0)) >= x)
