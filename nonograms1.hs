import System.Environment

data Field = Blank | Cross | Undefined deriving (Eq)

instance Show Field where
    show Blank = " "
    show Cross = "X"
    show Undefined = "?"

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
    where f c (x:xs)
            | c == delimiter = []:(x:xs)
            | otherwise = (c:x):xs

initBoard :: Int -> Int -> [[Field]]
initBoard width height = replicate height (replicate width Undefined)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

countRow :: Field -> [Field] -> Int
countRow _ [] = 0
countRow elem (x:xs) 
    | elem == x = 1 + countRow elem xs
    | otherwise = countRow elem xs

countProgress :: [[Field]] -> Int
countProgress [] = 0
countProgress board = sum (map (countRow Cross) board)

ifFits :: Int -> [Field] -> Bool
ifFits 0 (Cross:_) = False
ifFits 0 _ = True
ifFits x [] = False 
ifFits x (Blank:_) = False
ifFits x (_:row) = ifFits (x - 1) row

genPossibleRow :: Int -> [Int] -> [[Field]]
genPossibleRow n [] = [replicate n Blank]
genPossibleRow n (k:ks) 
    | n < k = []
    | otherwise = [Blank : row | row <- genPossibleRow (n-1) (k:ks)] ++ if null ks 
        then [replicate k Cross ++ replicate (n-k) Blank]
        else [replicate k Cross ++ Blank : row | row <- genPossibleRow (n-k-1) ks]

checkIfCover :: [Field] -> [Field] -> Bool
checkIfCover [] [] = True
checkIfCover (Blank:x) (Blank:y) = checkIfCover x y
checkIfCover (Cross:x) (Cross:y) = checkIfCover x y
checkIfCover (Undefined:x) (_:y) = checkIfCover x y
checkIfCover (_:_) (_:_) = False

reduceWrongRows :: [Field] -> [[Field]] -> [[Field]]
reduceWrongRows = filter.checkIfCover

intersectionRows :: [Field] -> [Field] -> [Field]
intersectionRows [] [] = []
intersectionRows (Blank:x) (Blank:y) = Blank : intersectionRows x y
intersectionRows (Cross:x) (Cross:y) = Cross : intersectionRows x y
intersectionRows (_:x) (_:y) = Undefined : intersectionRows x y

intersectionPossibilities :: [[Field]] -> [Field]
intersectionPossibilities [] = []
intersectionPossibilities [row] = row
intersectionPossibilities (x:y:xs) = intersectionPossibilities ((intersectionRows x y):xs)

solutionStep :: [[Int]] -> [[Field]] -> [[Field]]
solutionStep [] [] = []
solutionStep (numbers:restNumbers) (row:board) = (intersectionPossibilities (reduceWrongRows row (genPossibleRow (length row) numbers))) : solutionStep restNumbers board

solve :: [[Int]] -> [[Int]] -> [[Field]] -> [[Field]]
solve horizontal vertical board = if countProgress newBoard > countProgress board 
    then solve horizontal vertical newBoard
    else board
    where newBoard = transpose (solutionStep vertical (transpose (solutionStep horizontal board)))

showBoard :: [[Field]] -> [Char]
showBoard board = (foldr (\a b -> a ++ "\n" ++ b) [] (map (map (\a -> if a == Cross
    then '█'
    else ' ')) board))

main = do
    files <- getArgs
    x <- ((readFile (head files)) >>= return . (splitBy '\n'))
    --x <- ((readFile "boards/30x20.txt") >>= return . (splitBy '\n'))
    x <- return $ map ((map (read::String->Int)) . splitBy ' ') x
    size <- return (head x)
    width <- return (head size)
    height <- return (head (tail size))
    horizontal <- return (drop width (drop 1 x))
    vertical <- return (take width (drop 1 x))
    board <- return (solve horizontal vertical (initBoard width height))
    writeFile "output.txt" (showBoard board)
    print board

--main = print (intersectionPossibilities (reduceWrongRows [Undefined,Undefined,Undefined,Undefined,Undefined,Undefined] (genPossibleRow 6 [4])))