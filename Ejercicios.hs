--division
division :: Int->Int->Int
division n 0 = 0
division 0 m = 0
division n m = if n < m then 0 else 1 + division(n-m) m

--producto
producto :: Int->Int->Int
producto n 0 = 0
producto 0 m = 0
producto n 1 = n
producto 1 m = m 
producto n m = n + producto n m-1

--potencia
potencia :: Int->Int->Int
potencia n 0 = 1
potencia 0 m = 0
potencia n 1 = n
potencia n m = n * potencia n m-1

--suma digitos
suma_digitos :: Int->Int->Int
suma_digitos n < 10 = n
suma_digitos n%10 + suma_digitos(n//10)

--invertir lista
invertir_lista ::[Int]->[Int]
invertir_lista [] = []
invertir_lista (x:xs) = (invertirLista xs) ++ [x]

--sumar pares de una lista
suma_pares :: [Int] -> 
suma_pares [] = 0
suma_pares (x:xs)
|mod x 2 == 0 = x + sumaPares(xs)
|otherwise = sumaPares(xs)

--cantidad impares de una lista
cant_impar :: [int] -> Int
cant_impar [] = 0
cant_impar(x:xs)
|mod x 2 == 1 = 1 + cant_impar(xs)
|otherwise = cant_impar(xs)

--determinar si una lista contiene a otra lista
pertenece :: Int->[Int]-> Bool
pertenece x [] = False
pertenece x (y:ys)
|x==y = True
|otherwise = pertenece x ys



