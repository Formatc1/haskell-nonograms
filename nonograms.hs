import System.Environment

splitBy :: Char -> [Char] -> [[Char]]
splitBy delimiter = foldr f [[]]
    where f c (x:xs)
            | c == delimiter = []:(x:xs)
            | otherwise = (c:x):xs

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

fillSimpleBoxes :: (Int -> Int) -> Int -> ([Int], [Int]) -> [Int]
fillSimpleBoxes _ _ ([], board) = board
fillSimpleBoxes f color (numbers@(num:nums), board@(field:fields)) = if field < 0 
    then field : (fillSimpleBoxes f color (numbers, fields)) 
    else if (itFits num board) 
        then if null nums 
            then (replicate num color) ++ (drop num board)
            else (replicate num color) ++ [0] ++ (fillSimpleBoxes f (f color) (nums, (drop (num + 1) board)))
        else 0 : fillSimpleBoxes f color (numbers, fields)
fillSimpleBoxes _ _ _ = []

itFits :: Int -> [Int] -> Bool
itFits 0 [] = True
itFits _ [] = False
itFits 0 (field:fields) = if field > 0
    then False
    else True
itFits num (field:fields) = if field < 0
    then False
    else itFits (num - 1) fields

glueOnPair :: ([Int], [Int]) -> [Int]
glueOnPair (numbers, row) = glue 0 (head numbers) row

glue :: Int -> Int -> [Int] -> [Int]
glue _ 0 row = row
glue 0 number (field:row)
    | field < 0 = (-1) : glue 0 number row
    | field == 0 = 0 : glue 0 (number-1) row 
    | field > 0 = 1 : glue 1 (number-1) row
glue 1 number list@(field:row)
    | field < 0 = list
    | field == 0 = 1 : glue 1 (number-1) row
    | field > 0 = 1 :glue 1 (number-1) row

--leftFillSimpleBoxes :: Int -> Int -> [Int] -> [Int]
--leftFillSimpleBoxes _ size [] = replicate size 0
--leftFillSimpleBoxes color size (number:next:xs) = (replicate number color) ++ [0] ++ (leftFillSimpleBoxes (color + 1) (size - number - 1) (next:xs))
--leftFillSimpleBoxes color size [number] = (replicate number color) ++ (replicate (size - number) 0)

--rightFillSimpleBoxes :: Int -> Int -> [Int] -> [Int]
--rightFillSimpleBoxes _ size [] = replicate size 0
--rightFillSimpleBoxes color size (number:next:xs) = (rightFillSimpleBoxes (color - 1) (size - number - 1) (next:xs)) ++ [0] ++ (replicate number color)  
--rightFillSimpleBoxes color size [number] = (replicate (size - number) 0 ) ++ (replicate number color)

--checkSimpleBoxes :: (Int, Int) -> Int
--checkSimpleBoxes x = if (fst x) == (snd x) && (fst x) > 0 then 1 else 0

intersectionElem :: (Int, Int) -> Int
intersectionElem x = if (fst x) > 0 && (snd x) > 0 && (fst x) == (snd x) 
    then 1
    else if (fst x) < 0 || (snd x) < 0
        then -1
        else 0

intersectionList :: Int -> Int -> ([Int], [Int]) -> [Int]
intersectionList _ _ ([], []) = []
intersectionList stateX stateY ((x:xs), (y:ys))
    | x == 1 || y == 1 = 1 : intersectionList stateX stateY (xs, ys)
    | x < 0 || y < 0 = (-1) : intersectionList stateX stateY (xs, ys)
    | x > 1 && y > 1 && x == y = 1 : intersectionList x y (xs, ys)
    | stateX == stateY && x == 0 && y == 0 = (-1) : intersectionList stateX stateY (xs, ys)
    | x == 0 && y == 0 = 0 : intersectionList stateX stateY (xs, ys)
    | x > 1 && y > 1 = 0 : intersectionList x y (xs, ys)
    | x > 1 = 0 : intersectionList x stateY (xs, ys)
    | y > 1 = 0 : intersectionList stateX y (xs, ys)
    | otherwise = 0 : intersectionList stateX stateY (xs, ys)

--simpleBoxes :: Int -> [Int] -> [Int]
--simpleBoxes size numbers = map checkSimpleBoxes (zip x (rightFillSimpleBoxes (maximum x) size (reverse numbers))) where x = (leftFillSimpleBoxes 2 size numbers)

unionElem :: (Int, Int) -> Int
unionElem x = if (fst x) > 0 || (snd x) > 0 
    then 1
    else if (fst x) < 0 || (snd x) < 0
        then -1
        else 0

addSolutionList :: ((Int, Int) -> Int) -> ([Int], [Int]) -> [Int]
addSolutionList f x = map f (zip (fst x) (snd x))

addSolution :: ((Int, Int) -> Int) -> [[Int]] -> [[Int]] -> [[Int]]
addSolution f x y = map (addSolutionList f) (zip x y)

showBoard :: [[Int]] -> [Char]
showBoard board = (foldr (\a b -> a ++ "\n" ++ b) [] (map (map (\a -> if a > 0
    then '█'
    else ' ')) board))

count :: [Int] -> Int
count row = length (filter (> 0) row)

checkRow :: ([Int], [Int]) -> Bool
checkRow row = (fst row) == (removeZeroFromHead (foldr checkRowFoldFun [] (snd row)))

checkAndCompleteRow :: ([Int], [Int]) -> [Int]
checkAndCompleteRow row = if checkRow row
    then map (\x -> if x > 0
        then 1
        else -1) (snd row)
    else snd row

removeZeroFromHead :: [Int] -> [Int]
removeZeroFromHead [] = []
removeZeroFromHead (0:list) = list
removeZeroFromHead list = list

checkRowFoldFun :: Int -> [Int] -> [Int]
checkRowFoldFun x []
    | x > 0 = [1]
    | otherwise = [0]
checkRowFoldFun x (row@(actual:rest))
    | x > 0 = (actual+1):rest
    | actual == 0 = row
    | otherwise = 0:row

reversePair :: ([a], [a]) -> ([a], [a])
reversePair (a, b) = (reverse a, reverse b)

countProgress :: [[Int]] -> Int
countProgress board = sum (map count board)

stepSolution :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] 
stepSolution horizontal vertical board = do
    board1 <- return board
    board2 <- return (transpose board)
    --board3 <- return (addSolution intersectionElem (map (fillSimpleBoxes ((+) 1) 2) (zip horizontal board1)) (map (\x -> reverse (fillSimpleBoxes (flip (-) 1) ((length (fst x)) + 1) (reversePair x))) (zip horizontal board1)))
    --board4 <- return (addSolution intersectionElem (map (fillSimpleBoxes ((+) 1) 2) (zip vertical board2)) (map (\x -> reverse (fillSimpleBoxes (flip (-) 1) ((length (fst x)) + 1) (reversePair x))) (zip vertical board2)))
    board3 <- return (map (intersectionList 0 0) (zip (map (fillSimpleBoxes ((+) 1) 2) (zip horizontal board1)) (map (\x -> reverse (fillSimpleBoxes (flip (-) 1) ((length (fst x)) + 1) (reversePair x))) (zip horizontal board1))))
    board4 <- return (map (intersectionList 0 0) (zip (map (fillSimpleBoxes ((+) 1) 2) (zip vertical board2)) (map (\x -> reverse (fillSimpleBoxes (flip (-) 1) ((length (fst x)) + 1) (reversePair x))) (zip vertical board2))))


    board3 <- return (addSolution unionElem board3 (transpose board4))
    board4 <- return (transpose board3)

    board3 <- return (map (checkAndCompleteRow) (zip horizontal board3))
    board4 <- return (map (checkAndCompleteRow) (zip vertical board4))

    board3 <- return (addSolution unionElem board3 (transpose board4))

    board3 <- return (map glueOnPair (zip horizontal board3))
    board3 <- return (map (\x -> reverse (glueOnPair (reversePair x))) (zip horizontal board3))

    board4 <- return (transpose board3)
    board4 <- return (map glueOnPair (zip vertical board4))
    board4 <- return (map (\x -> reverse (glueOnPair (reversePair x))) (zip vertical board4))
    board3 <- return (transpose board4)

    if (countProgress board3 > countProgress board1)
        then stepSolution horizontal vertical board3
        else if sum (map sum horizontal) == countProgress board3
            then board3
            else findCorrect vertical (bruteForceBoard horizontal board3)
            --else board3

genBruteForceRow :: Int -> [Int] -> [Int] -> [[Int]]
genBruteForceRow 0 [] row = [row]
genBruteForceRow emptyCells [] row = [row ++ replicate emptyCells (-1)]
genBruteForceRow 0 (number:numbers) row 
    | row == take (length row) [-1,-1..] = genBruteForceRow 0 numbers (row ++ replicate number 1)
    | otherwise = genBruteForceRow 0 numbers (row ++ ((-1):replicate number 1))
genBruteForceRow emptyCells listNumbers@(number:numbers) row
    | row == take (length row) [-1,-1..] = (genBruteForceRow emptyCells numbers (row ++ replicate number 1)) ++ (genBruteForceRow (emptyCells - 1) listNumbers (row ++ [-1]))
    | otherwise = (genBruteForceRow emptyCells numbers (row ++ ((-1):replicate number 1))) ++ (genBruteForceRow (emptyCells-1) listNumbers (row ++ [-1]))

--[0,0,1,1,1,1,1,1,1,1,0,0,-1,0,0]
bruteForce :: ([Int], [Int]) -> [[Int]]
bruteForce ([], row) = [replicate (length row) (-1)]
bruteForce (listNumbers, row) = filter (checkIfCover row) (genBruteForceRow ((length row) - ((sum listNumbers) + (length listNumbers) - 1)) listNumbers [])

-- checkIfCover computedPartial fromBruteForce
checkIfCover :: [Int] -> [Int] -> Bool
checkIfCover [] [] = True
checkIfCover [] _ = False
checkIfCover _ [] = False
checkIfCover (x:xs) (y:ys) 
    | x >= 0 && y > 0 = checkIfCover xs ys
    | x <= 0 && y < 0 = checkIfCover xs ys
    | otherwise = False

bruteForceBoard :: [[Int]] -> [[Int]] -> [[[Int]]]
bruteForceBoard horizontal board = sequence (map bruteForce (zip horizontal board))

findCorrect :: [[Int]] -> [[[Int]]] -> [[Int]]
findCorrect vertical boards = transpose (map snd (head (filter (\x -> foldl (&&) True (map checkRow x)) (map ((zip vertical).transpose) boards))))

main = do
    -- read board parameters from file
    files <- getArgs
    x <- (>>=) (readFile (head files)) (return.splitBy '\n')
    --x <- (>>=) (readFile "boards/22x15.txt") (return.splitBy '\n')
    x <- return (map (splitBy ' ') x)
    x <- return (map (map (read::String->Int)) x)
    size <- return (head x)
    width <- return (head size)
    height <- return (head (tail size))
    horizontal <- return (drop width (drop 1 x))
    vertical <- return (take width (drop 1 x))

    board <- return (replicate height (replicate width 0))

    board <- return (stepSolution horizontal vertical board)

    --writeFile (head (tail files)) (showBoard board)
    writeFile "output.txt" (showBoard board)
    print board


--main = do
----    --writeFile "output.txt" (showBoard (genBruteForceRow 3 [5,1] []))
--    print (findCorrect [[5, 3], [7, 4], [2, 2, 3], [15], [2, 2, 2], [15], [2, 2, 2], [3, 7], [4, 5], [3, 3]] (bruteForceBoard [[1, 1], [7], [9], [2, 1, 1, 3], [2, 1, 1, 2], [2, 1, 1, 2], [4, 1], [7], [6], [2, 1, 1, 3], [2, 1, 1, 3], [4, 1, 3], [8], [6], [1, 1]] [[-1,-1,-1,1,-1,1,-1,-1,-1,-1],[0,1,1,1,1,1,1,0,0,0],[0,1,1,1,1,1,1,1,1,0],[1,1,-1,1,-1,1,-1,1,1,1],[1,1,0,1,-1,1,-1,0,1,0],[1,1,0,1,-1,1,-1,0,1,0],[1,1,1,1,-1,1,-1,-1,-1,-1],[0,1,0,1,1,1,1,1,0,0],[-1,-1,-1,1,1,1,1,1,1,-1],[1,1,-1,1,-1,1,-1,1,1,1],[1,1,-1,1,-1,1,-1,1,1,1],[1,1,1,1,-1,1,-1,1,1,1],[0,1,1,1,1,1,1,1,0,0],[-1,-1,1,1,1,1,1,1,-1,-1],[-1,-1,-1,1,-1,1,-1,-1,-1,-1]]))
--    --print (countProgress [[-1,-1,-1,1,-1,1,-1,-1,-1,-1],[0,1,1,1,1,1,1,0,0,0],[0,1,1,1,1,1,1,1,1,0],[1,1,-1,1,-1,1,-1,1,1,1],[1,1,0,1,-1,1,-1,0,1,0],[1,1,0,1,-1,1,-1,0,1,0],[1,1,1,1,-1,1,-1,-1,-1,-1],[0,1,0,1,1,1,1,1,0,0],[-1,-1,-1,1,1,1,1,1,1,-1],[1,1,-1,1,-1,1,-1,1,1,1],[1,1,-1,1,-1,1,-1,1,1,1],[1,1,1,1,-1,1,-1,1,1,1],[0,1,1,1,1,1,1,1,0,0],[-1,-1,1,1,1,1,1,1,-1,-1],[-1,-1,-1,1,-1,1,-1,-1,-1,-1]])
--    --print (sum (map sum [[5, 3], [7, 4], [2, 2, 3], [15], [2, 2, 2], [15], [2, 2, 2], [3, 7], [4, 5], [3, 3]]))
--    --print (checkIfCorrect [[1], [2]] [[[1,1], [2,2]], [[3,3], [4,4]]])
