type Word' = String
type Line  = String
type Doc   = String

--arvore binária de busca com 2 casos. Folha ou nó com palavra, lista de linhas, subárvore esquerda e subárvore direita
data Tree = Node Word' [Int] Tree Tree
          | Leaf
          deriving Show

--a)Lines ja existe na biblioteca padrão

--b)
numLines :: [Line] -> [(Int, Line)]
numLines ls = zip [1..] ls

--Junta os numeros naturais com cada linha

--c)
allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords [] = []
allNumWords ((n, ln):xs) =
    let ws = words ln
        wsLong = filter (\w -> length w > 3) ws
    in  [(n, w) | w <- wsLong] ++ allNumWords xs

--Word Ln Divide a linha em palavras.
--filter remove as palavras com 3 ou menos caracteres

--d)
insOrd :: Ord a => a -> [a] -> [a]
insOrd x [] = [x]
insOrd x (y:ys)
    | x < y     = x : y : ys
    | x == y    = y : ys
    | otherwise = y : insOrd x ys

--Serve para inserir o número da linha na lista, mantendo ordenado e sem duplicar.

--e)
ins :: Word' -> Int -> Tree -> Tree
ins w n Leaf = Node w [n] Leaf Leaf
ins w n (Node w' ns left right)
    | w < w'    = Node w' ns (ins w n left) right
    | w > w'    = Node w' ns left (ins w n right)
    | otherwise = Node w' (insOrd n ns) left right

--Se a árvore está vazia, cria um novo nó com: palavra w lista somente com o número da linha [n]
-- se o nó já existir Compare palavras: Se w < w', insere à esquerda Se w > w', insere à direita
--Se for a mesma palavra, n cria um novo nó apenas o novo numro da linha

--f)
mIndexTree :: [(Int, Word')] -> Tree
mIndexTree [] = Leaf
mIndexTree ((n, w):xs) = ins w n (mIndexTree xs)
--Constrói a árvore assim: Cria árvore para o resto da lista (mIndexTree xs) Insere o par atual (n,w) ne

--h)
makeIndexTree :: Doc -> Tree
makeIndexTree doc =
    let ls     = lines doc
        num    = numLines ls
        pairs  = allNumWords num
    in  mIndexTree pairs
--lines doc → divide documento em linhas numLines → [(linha, conteudo)] allNumWords → (linha, palavra) filtrando palavras pequenas
--mIndexTree → insere tudo na árvore

printTree :: Tree -> IO ()
printTree Leaf = return ()
printTree (Node w ns left right) = do
    printTree left
    putStrLn (w ++ " : " ++ show ns)
    printTree right
--É uma busca em ordem (in-order traversal):
--imprime a esquerda (palavras menores)
--imprime a palavra atual
--imprime a direita (palavras maiores)
--Resultado: ordem alfabética.

main :: IO ()
main = do
    putStrLn "Digite o nome do arquivo:"
    file <- getLine
    doc <- readFile file
    let tree = makeIndexTree doc
    printTree tree

------------
-- FUNÇÃO printTree MODIFICADA PARA ORDEM DECRESCENTE
printTree :: Tree -> IO ()
printTree Leaf = return ()
printTree (Node w ns left right) = do
    printTree right -- Visita primeiro a sub-árvore direita (maiores palavras)
    putStrLn (w ++ " : " ++ show ns) -- Imprime o nó atual
    printTree left  -- Visita a sub-árvore esquerda (menores palavras)
-- Resultado: ordem alfabética decrescente.

main :: IO ()
main = do
    putStrLn "Digite o nome do arquivo:"
    file <- getLine
    doc <- readFile file
    let tree = makeIndexTree doc
    printTree tree