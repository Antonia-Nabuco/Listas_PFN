--1
pertence x [] = False -- se x e lista vazia f
pertence x (y:ys)
    | x == y    = True
    | otherwise = pertence x ys
-- compara x com a cabeca V,senao o pertence recebe x,ys pra continuar comparando
--2
intersecao [] ys = [] --se a lista percorre e n encontra nd igual volta ela vazia
intersecao (x:xs) ys = if pertence x ys then x: intersecao xs ys else intersecao xs ys
--chama funcao pra ver se sao iguais se for acrecente x, senao devolve o restante da lista
--3
inversao [] = [] 
inversao (y:ys) = inversao ys ++ [y] --funcao inversao(lista\calda) e fica concatenando com a cabeca

--4
nUltimos _ [] = []            -- se a lista é vazia, retorna []
nUltimos 0 _  = []            -- se n = 0, retorna []
nUltimos n ys = reverse (take n (reverse ys))

--5
soma [] ys = [] -- se vazio e calda retorna vazio
soma xs [] = [] 
soma (x:xs) (y:ys) = x + y : soma xs ys -- soma as cabecas e coloque na frente da lista

--6
potencias 0 = [1] --toda potencia em 0 é 1
potencias n = potencias (n-1) ++ [2^n] -- n é o elevado da potencia, fica concatenando o novo n

--7
intercalacao [] ys = ys -- Se lista1 = vazia a intercalação é a lista2 inteira
intercalacao xs [] = xs -- Se lista2 vazia, intercalacao é a lista1 inteira
intercalacao (x:xs) (y:ys) = if x <= y then x: intercalacao xs (y:ys) else y: intercalacao (x:xs) ys
-- menor da cabeca entra 1 na lista ai fica comparando oq sobrou ate zerar

--8
menor [x] = x --menor da listaigual menor 
menor (x:xs) =
    let m = menor xs
    in if x < m then x else m
--percorre menor xs e fica comparando ate a lista esgotar e ser x=x

--9 
removerElem x [] = [] --se lista vazia e x retorne vazia
removerElem x (y:ys) = if x == y then ys else y:removerElem x ys 
-- x = cabeca entao retorna calda senao retorna na lista a cabeca e dnv seu numero x e calda

--10
ordenar []=[] 
ordenar xs =(menor xs):(ordenar(removerElem(menor xs)xs)) 
-- encontra o menor elemento da lista e coloca na frente e remove o menor repetido na chamada

--11
ins n [] = [n] -- n e vazio retorna n
ins n (x:xs) = if pertence n (x:xs) == True then ordenar (x:xs) else ordenar (n:(x:xs))
--verificar se n pertence a lista se sim ordena senao ordene n:[] 

--12
enesimo 1 (x:xs) = x -- valor minimo é 1 
enesimo n (x:xs) = enesimo (n-1) xs

--13
repetir 1 e = [e] -- n=1
repetir n e = e:repetir (n-1) e

--14
numString 0 = []
numString n = numString (div n 10) ++ [int2Char (rem n 10)] --pega o número sem o último dígito, converte e adiciona o último dígito convertido em Char
int2Char :: Int -> Char
int2Char d = toEnum (d + 48)
--toEnum numero inteiro vira caracter com tabela de ASCII

--15
stringNum ::String -> Int
stringNum [] = 0
stringNum (x:xs) = char2Int x * (10 ^ length xs) + stringNum xs --multiplica pelo peso decimal +
char2Int :: Char -> Int
char2Int d = fromEnum d - 48
--fromEnum caracter em seu numero corespondente

--16
bin2int [] = 0
bin2int (x:xs) = (char2Int x) * 2 ^ (length xs) + bin2int xs --multiplica pelo peso binário

--17
int2bin 0 = []
int2bin n = int2bin (div n 2) ++ [int2Char(rem n 2)]

--18
minusculas [] = []
minusculas (x:xs) = if fromEnum x < 97 then toEnum (fromEnum x +32):minusculas xs else x:minusculas xs
