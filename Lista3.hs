--1
segundos [] = []
segundos ((x,y):xs) = y : segundos xs --x,y é o primeiro par da lista ai eu pego o segundo que é o y

--2
menorMaior  [x] = (x,x)
menorMaior (x:xs) = (novomin,novomax) -- haskell n sabe oq é novomin e novomax entao preciso definir oq é dps
    where
        (minPri,maxPri) = menorMaior xs --pra poder dizer oq é esse novomin e novomax
        novomin = if minPri < x then minPri  else x
        novomax = if maxPri > x then maxPri  else x 

--3
separar n []= ([],[])
separar n (x:xs) = (novomenores,novomaiores)
        where
            (menores,maiores) = separar n xs
            novomenores = if x < n then x : menores else menores
            novomaiores = if x > n then x : maiores else maiores

--4
enumerar xs = enumerarN 1 xs
  where
    enumerarN _ [] = []
    enumerarN n (x:xs) = (n, x) : enumerarN (n+1) xs

--5
menores [] = []
menores ((x,y):xs)
  | x < y     = (x,y) : menores xs
  | otherwise = menores xs