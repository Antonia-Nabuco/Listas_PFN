--1
ehTriangulo a b c | (a < b + c) || (b < c + a) || (c < b + a) = True
                  | otherwise = False

--2                 
tipoTriangulo a b c |(a == b && b == c) = ("equilatero")
                    |(a == b) && a /= c || (a == c) && a /= b || (b == c) && b /= a = ("isoceles")
                    |otherwise = ("escaleno")

--3
naoTriangulo a b c = if (ehTriangulo a b c) then "nao eh um triangulo"
                     else tipoTriangulo a b c  

--4
somaPares 0 = 0
somaPares n = if rem n 2 == 0 then n + somaPares (n-2) else somaPares (n-1)

--5
somaPot2m m 0 = m
somaPot2m m n = (m * 2^n) + somaPot2m m (n - 1)

--6 
isPrimo n
        | n <= 1 = False
        | n == 2 = True
        | n rem 2 == 0 = False
        | otherwise = not(temDivosor n 3)
        
    where
      temDivisor num d
         |d * d > num = False
         |num rem d == 0 = True
         |otherwise = temDivisor num (d + 2)
           
     
--7                                
SeriePI n = somaTermos 0 1 1
  where
   limite = 4 / fromIntegral n
   somaTermos soma sinal demom
    |4 / demom < limite = soma
    |otherwise = somaTermos (soma + sinal * 4/ demom) (-sinal) (demom + 2)