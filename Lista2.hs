--1
pertence x [] = False
pertence x (y:ys)
    | x == y    = True
    | otherwise = pertence x ys

--2
intersecao [] ys = []
intersecao (x:xs) ys = if pertence x ys then x: intersecao xs ys else intersecao xs ys

--3
inversao [] = []
inversao (y:ys) = inversao ys ++ [y]

--4
nUltimos _ [] = []            -- se a lista é vazia, retorna []
nUltimos 0 _  = []            -- se n = 0, retorna []
nUltimos n ys = reverse (take n (reverse ys))

--5
soma [] ys = []
soma xs [] = []
soma (x:xs) (y:ys) = x + y : soma xs ys

--6
potencias 0 = [1]
potencias n = potencias (n-1) ++ [2^n]

--7

--8
menor [] = error "Essa função não eh definida"
menor [x] = x
menor (x:xs) = let m = menor xs in 
                 if x<m then x else menor

--9