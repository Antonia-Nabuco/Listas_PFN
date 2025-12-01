main = do putStr "Nome do arquivo:"
          arq <- getLine
          txt <- readFile arq
          putStr(txt ++ "\n")
          let tamanho = length txt
          putStr ("O arquivo" ++ arv ++ "possui" ++ sho tamanho ++ "caracteres\n")
          
          numLines xs = zip [1..] (lines xs)


imprimirLinhas :: [(int, String)] -> IO ()
imprimirLinhas [] = return ()
imprimirLinhas((linhas, palavra):xs) = do putStr(show n) ++ ". " ++ l ++ "\n"
                                          imprimir xs 