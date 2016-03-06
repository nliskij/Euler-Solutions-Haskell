import Data.List (sortBy)

collatzInc :: Int -> Int -> Int
collatzInc n steps
    | n == 1 = steps
    | odd n = (collatzInc (3*n + 1) (steps + 1))
    | even n = (collatzInc (quot n 2) (steps + 1))

collatz :: Int -> Int
collatz n = ((flip collatzInc) 0) n

indexOfMax :: (Enum b, Num b, Ord a) => [a] -> b
indexOfMax xs = snd (head (sortBy (\as bs-> (flip compare) (fst as) (fst bs))
    (zip xs [0..])))


main = print $ (indexOfMax (map collatz [1..1000000])) + 1
