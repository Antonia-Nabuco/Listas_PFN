import Data.Char (toLower)
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
   in  [(n, map toLower w) | w <- wsLong] ++ allNumWords xs
   {--[(n, w) | w <- wsLong] ++ allNumWords xs--}


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


printTreeCrescente :: Tree -> IO ()
printTreeCrescente tree = putStrLn (drawC tree)
printTreeVisual :: Tree -> IO ()
printTreeVisual tree = printTreeCrescente tree

drawC :: Tree -> String
drawC Leaf = ""
drawC (Node w ns left right) =
    w ++ " : " ++ show ns ++ "\n"
    ++ drawSubC left  "├── " "│   "
    ++ drawSubC right "└── " "    "

drawSubC :: Tree -> String -> String -> String
drawSubC Leaf _ _ = ""
drawSubC (Node w ns left right) prefix childIndent =
    prefix ++ w ++ " : " ++ show ns ++ "\n"
    ++ drawSubC left  (childIndent ++ "├── ") (childIndent ++ "│   ")
    ++ drawSubC right (childIndent ++ "└── ") (childIndent ++ "    ")

------
{--printTreeDecrescente :: Tree -> IO ()
printTreeDecrescente tree = putStrLn (drawD tree)

drawD :: Tree -> String
drawD Leaf = ""
drawD (Node w ns left right) =
    w ++ " : " ++ show ns ++ "\n"
    ++ drawSubD right "├── " "│   "
    ++ drawSubD left  "└── " "    "

drawSubD :: Tree -> String -> String -> String
drawSubD Leaf _ _ = ""
drawSubD (Node w ns left right) prefix childIndent =
    prefix ++ w ++ " : " ++ show ns ++ "\n"
    ++ drawSubD right (childIndent ++ "├── ") (childIndent ++ "│   ")
    ++ drawSubD left  (childIndent ++ "└── ") (childIndent ++ "    ") --}


main :: IO ()
main = do
    putStrLn "Digite o nome do arquivo:"
    file <- getLine
    doc <- readFile file
    let tree = makeIndexTree doc
    printTreeVisual tree