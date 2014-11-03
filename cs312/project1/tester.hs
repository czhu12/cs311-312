moveDownLeft :: (Int, Int) -> (Int, Int)
moveDownLeft pos 
	| isBottomHalf(pos)						= (x - 1, y + 1)
	| otherwise										= (x, y + 1)
	where 
		x = fst pos
		y = snd pos

moveDownRight :: (Int, Int) -> (Int, Int)
moveDownRight pos 
	| isBottomHalf(pos)						= (x, y + 1)
	| otherwise										= (x + 1, y + 1)
	where 
		x = fst pos
		y = snd pos

moveUpLeft :: (Int, Int) -> (Int, Int)
moveUpLeft pos 
	| isBottomHalf(pos)						= (x, y - 1)
	| otherwise										= (x - 1, y - 1)
	where 
		x = fst pos
		y = snd pos

moveUpRight :: (Int, Int) -> (Int, Int)
moveUpRight pos 
	| isBottomHalf(pos)						= (x + 1, y - 1)
	| otherwise										= (x, y - 1)
	where 
		x = fst pos
		y = snd pos
