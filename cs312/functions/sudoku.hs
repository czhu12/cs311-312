data Sudoku = Sudoku [[Maybe Int]]

example :: Sudoku
example =
Sudoku
[ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
, [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
, [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
, [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
, [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
, [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
, [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
, [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
, [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
]


isValidBoard
generateNewBoards

generateNewBoards board x y
isValidBoard board = 


solve:: [[Maybe Int]] -> [[Maybe Int]]
solve board x y
  | not (isValidBoard board)      = False
  | otherwise (inspect listOfBoards x y)
  where listOfBoards = generateNewBoards board x y

inspect listOfBoards x y
  let board = head listOfBoards
  let sol = solve board x y
  if sol then sol else inspect (rest listOfBoards x y)
