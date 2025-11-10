import Data.List (sortBy, groupBy, sort, group)

type Doc = String
type Line = String
type Word' = String

-- Programa principal
main :: IO ()
main = do
  putStr "Digite o nome do arquivo: "
  arq <- getLine
  txt <- readFile arq
  putStrLn "\nÍNDICE DE PALAVRAS:\n"
  putStr (makeindexFormatado (makeindex txt))

-- Função principal de índice
makeindex :: Doc -> [([Int], Word')]
makeindex txt =
  shorten
    (almalgamate
      (sortLs
        (allNumWords
          (numLines
            (lines txt)))))

-- Formata a saída: "Palavra - [linhas]"
makeindexFormatado :: [([Int], Word')] -> String
makeindexFormatado xs =
  unlines (map (\(linhas, palavra) -> palavra ++ " - " ++ show linhas) xs)

-- a) Separar documento em linhas: já faz parte da lib padrão "lines"

-- b) Numerar as linhas
numLines :: [Line] -> [(Int, Line)]
numLines xs = zip [1..] xs

-- c) Associar número da linha a cada palavra com mais de 3 letras
allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords [] = []
allNumWords ((n, linha):xs) =
  zip (repeat n) (filter (\w -> length w > 3) (words linha)) ++ allNumWords xs

-- d) Ordenar alfabeticamente as palavras
sortLs :: [(Int, Word')] -> [(Int, Word')]
sortLs = sortBy (\(_, a) (_, b) -> compare a b)

-- e) Agrupar as ocorrências por palavra
almalgamate :: [(Int, Word')] -> [([Int], Word')]
almalgamate xs =
  map (\grp -> (map fst grp, snd (head grp))) $
    groupBy (\(_, a) (_, b) -> a == b) xs

-- f) Remover repetições de números de linha
shorten :: [([Int], Word')] -> [([Int], Word')]
shorten = map (\(ns, w) -> (removerRepetidos ns, w))

removerRepetidos :: Ord a => [a] -> [a]
removerRepetidos = map head . group . sort