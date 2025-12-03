type Word' = String
type Line  = String
type Doc   = String


data Tree = Node Word' [Int] Tree Tree
          | Leaf
          deriving Show

--a)Lines ja existe na biblioteca padrão

--b)
numLines :: [Line] -> [(Int, Line)]
numLines ls = zip [1..] ls

--c)
allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords [] = []
allNumWords ((n, ln):xs) =
    let ws = words ln
        wsLong = filter (\w -> length w > 3) ws
    in  [(n, w) | w <- wsLong] ++ allNumWords xs
    

--d)
insOrd :: Ord a => a -> [a] -> [a]
insOrd x [] = [x]
insOrd x (y:ys)
    | x < y     = x : y : ys
    | x == y    = y : ys
    | otherwise = y : insOrd x ys

--e)
ins :: Word' -> Int -> Tree -> Tree
ins w n Leaf = Node w [n] Leaf Leaf
ins w n (Node w' ns left right)
    | w < w'    = Node w' ns (ins w n left) right
    | w > w'    = Node w' ns left (ins w n right)
    | otherwise = Node w' (insOrd n ns) left right

--f)
mIndexTree :: [(Int, Word')] -> Tree
mIndexTree [] = Leaf
mIndexTree ((n, w):xs) = ins w n (mIndexTree xs)

--h)
makeIndexTree :: Doc -> Tree
makeIndexTree doc =
    let ls     = lines doc
        num    = numLines ls
        pairs  = allNumWords num
    in  mIndexTree pairs

--i)
printTree :: Tree -> IO ()
printTree Leaf = return ()
printTree (Node w ns left right) = do
    printTree left
    putStrLn (w ++ " : " ++ show ns)
    printTree right

main :: IO ()
main = do
    putStrLn "Digite o nome do arquivo:"
    file <- getLine
    doc <- readFile file
    let tree = makeIndexTree doc
    printTree tree