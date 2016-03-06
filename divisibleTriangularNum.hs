import Data.List (nub)

intSqrt :: Int -> Int
intSqrt n = head [x | x <- [1..n], x * x >= n]

divisors :: Int -> [(Int, Int)]
divisors n = [(a, div n a) | a <- [1..(intSqrt n)], mod n a == 0]

removeTuples :: [(a, a)] -> [a]
removeTuples xs = (map fst xs) ++ (map snd xs)

triangularNums = [sum [1..n] | n <- [1..]]

main = print $ head ([n | n <- triangularNums, length (nub (removeTuples (divisors n))) >= 500])
