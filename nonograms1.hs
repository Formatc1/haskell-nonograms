import System.Environment

data Field = Blank | Cross | Undefined deriving (Eq)

instance Show Field where
    show Blank = " "
    show Cross = "X"
    show Undefined = "?"

initBoard :: Int -> Int -> [[Field]]
initBoard width height = replicate height (replicate width Undefined)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

countRow :: Field -> [Field] -> Int
countRow _ [] = 0
countRow element (x:xs) 
    | element == x = 1 + countRow element xs
    | otherwise = countRow element xs

countProgress :: [[Field]] -> Int
countProgress [] = 0
countProgress board = sum (map (countRow Cross) board)

ifFits :: Int -> [Field] -> Bool
ifFits 0 (Cross:_) = False
ifFits 0 _ = True
ifFits _ [] = False 
ifFits _ (Blank:_) = False
ifFits x (_:row) = ifFits (x - 1) row

findUndefined :: [Field] -> Bool
findUndefined [] = False
findUndefined (Undefined:_) = True
findUndefined (_:x) = findUndefined x

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
solutionStep (numbers:restNumbers) (row:board) = if findUndefined row 
    then (intersectionPossibilities (reduceWrongRows row (genPossibleRow (length row) numbers))) : solutionStep restNumbers board
    else row : solutionStep restNumbers board

solve :: [[Int]] -> [[Int]] -> [[Field]] -> [[Field]]
solve horizontal vertical board = if countProgress newBoard > countProgress board 
    then solve horizontal vertical newBoard
    else board
    where newBoard = transpose (solutionStep vertical (transpose (solutionStep horizontal board)))

showBoard :: [[Field]] -> [Char]
showBoard board = (foldr (\a b -> a ++ "\n" ++ b) [] (map (map (\a -> if a == Cross
    then '█'
    else ' ')) board))

main :: IO ()
main = do
    files <- getArgs
    x <- ((readFile (head files)) >>= return . lines)
    --x <- ((readFile "boards/22x15.txt") >>= return . lines)
    y <- return $ map ((map (read::String->Int)) . words) x
    size <- return (head y)
    width <- return (head size)
    height <- return (head (tail size))
    horizontal <- return (drop width (drop 1 y))
    vertical <- return (take width (drop 1 y))
    board <- return (solve horizontal vertical (initBoard width height))
    writeFile "output.txt" (showBoard board)
    print board

--main = print (intersectionPossibilities (reduceWrongRows [Undefined,Undefined,Undefined,Undefined,Undefined,Undefined] (genPossibleRow 6 [4])))