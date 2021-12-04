


f :: String -> [[Char]]
f "" = []
f (x:xs) = [x] : f xs

mostCommonBit :: [Int] -> Int -> [Int]
mostCommonBit [] _ = []
mostCommonBit (x:xs) len = if x < (div len 2) then 0:mostCommonBit xs len
                           else 1:mostCommonBit xs len

leastCommonBit :: [Int] -> Int -> [Int]
leastCommonBit [] _ = []
leastCommonBit (x:xs) len = if x < (div len 2) then 1:leastCommonBit xs len
                           else 0:leastCommonBit xs len


addlist _ [] = []
addlist [] _ = []
addlist (x:xs) (y:ys) = (x+y):addlist xs ys

binToDec :: [Int] -> Int -> Int
binToDec [] _ = 0
binToDec (x:xs) index = (x * 2 ^ index) + binToDec xs (index + 1)

main = do
   st <- readFile "input"
   let pst = (map (map (\x -> read x:: Int) . f) $ lines st)
   let gammarate = binToDec (reverse (mostCommonBit (foldr addlist (head pst) pst) (length pst))) 0
   let epsilonrate = binToDec (reverse (leastCommonBit (foldr addlist (head pst) pst) (length pst))) 0
   return (gammarate * epsilonrate) 
