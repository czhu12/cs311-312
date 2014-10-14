mmax :: Int -> Int -> Int
mmax a b 
  | a > b     = a
  | otherwise = b

factorial :: Int -> Int
factorial a 
  | a == 0          = 1
  | otherwise       = a * factorial (a - 1)

fibonacci :: Int -> Int
fibonacci a
  | a == 0 || a == 1  = 1
  | otherwise         = fibonacci(a - 1) + fibonacci(a - 2)

tupleAdd :: (Int, Int) -> Int
tupleAdd a = fst a + snd a

lucky :: (Integral a) => a -> String
lucky a
  | a <= 7      = "Go fuck yoruself"
  | otherwise   = "Fuck yourself anyways"

--myappend list1 list2
--  | null list1 = list2
--  | otherwise myappend(tail(list1), (head list1): list2)
