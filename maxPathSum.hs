data Tree a = Node a (Tree a) (Tree a) | Empty
        deriving (Show, Eq)

leftNode :: Tree a -> Tree a
leftNode Empty = Empty
leftNode (Node _ n _) = n

rightNode :: Tree a -> Tree a
rightNode Empty = Empty
rightNode (Node _ _ n) = n

matrixToTreeInc :: [[a]] -> Int -> Int -> Tree a
matrixToTreeInc xs i j
    | length xs == i + 1   = Node ((xs !! i) !! j) Empty Empty
    | otherwise            = Node ((xs !! i) !! j)
        (matrixToTreeInc xs (i + 1) j)
        (matrixToTreeInc xs (i + 1) (j + 1))

matrixToTree :: [[a]] -> Tree a
matrixToTree xs = matrixToTreeInc xs 0 0

pathsInc :: Tree a -> [[a]]-> [[a]]
pathsInc Empty ps = ps
pathsInc (Node x l r) ps = (pathsInc l (map (x:) ps)) ++
                           (pathsInc r (map (x:) ps))

paths :: Tree a -> [[a]]
paths Empty = []
paths (Node x l r) = pathsInc (Node x l r) [[]]

triangle = [
    [75],
    [95,64],
    [17,47,82],
    [18,35,87,10],
    [20,04,82,47,65],
    [19,01,23,75,03,34],
    [88,02,77,73,07,63,67],
    [99,65,04,28,06,16,70,92],
    [41,41,26,56,83,40,80,70,33],
    [41,48,72,33,47,32,37,16,94,29],
    [53,71,44,65,25,43,91,52,97,51,14],
    [70,11,33,28,77,73,17,78,39,68,17,57],
    [91,71,52,38,17,14,91,43,58,50,27,29,48],
    [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
    [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

smallTriangle = [
    [1],
    [2, 3],
    [4, 5, 6]]

main = print $ maximum (map sum (paths (matrixToTree triangle)))
