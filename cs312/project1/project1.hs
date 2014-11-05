empty = '_'

slice from to xs = take (to - from + 1) (drop from xs)

minimax board depth maximizingPlayer 
  | depth == 0 || isTerminal board        = score board
  | maximizingPlayer                      = (genAllChildren board)
  | otherwise                             =

generateNewStates b = removeEmptyLists (generateNewStates' b [] (0, 0))

generateNewStates' board pos
  | isOutOfBounds (fst pos) (snd pos) board = []
  | currentLetter == empty                  = generateNewStates' board seenCars (nextPos pos board)
  | not (elem currentLetter seenCars)       = (generateStatesForLetter currentLetter board) ++ (generateNewStates' board (currentLetter:seenCars) (nextPos pos board))
  | otherwise                               = generateNewStates' board seenCars (nextPos pos board)
  where 
    currentLetter = (letterAtPosition (fst pos) (snd pos) board)

generateNewStatesForPos pos board
  | isOutOfBounds pos board           = []
  | otherwise [canMove letter ( 1,  1) board, 
               canMove letter (-1, -1) board,    
               canMove letter ( 1, -1) board, 
               canMove letter (-1,  1) board]
  where letter = letterAtPosition pos board
    
--function minimax(node, depth, maximizingPlayer)
--  if depth = 0 or node is a terminal node
--    return the heuristic value of node
--  if maximizingPlayer
--    bestValue := -∞
--    for each child of node
--      val := minimax(child, depth - 1, FALSE)
--      bestValue := max(bestValue, val)
--    return bestValue
--  else
--    bestValue := +∞
--    for each child of node
--      val := minimax(child, depth - 1, TRUE)
--      bestValue := min(bestValue, val)
--    return bestValue
