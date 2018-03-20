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

--invertir entero
invertir :: Integer->Integer->Integer
invertir x y = if x== 0 then y else invertir(div x 10) ((y*10)+ (mod x 10))

--palindromo
palindromo :: Integer->String
palindromo x = if x == (invertir x 0) then "es palindomo" else "no es palindomo"

--invertir lista
invertir_lista ::[Int]->[Int]
invertir_lista [] = []
invertir_lista (x:xs) = (invertirLista xs) ++ [x]

--sumar pares lista RECURSIVO
sumaParesRecursiva::[Int]->Int
sumaParesRecursiva[] = 0
sumaParesRecursiva (x:xs)
 | mod x 2 == 1 = 0 + sumaParesRecursiva(xs)
 | otherwise = x + sumaParesRecursiva(xs)

--sumar pares lista LISTA DE COMPRESION
sumaParesCom::[Int]->Int
sumaParesCom(x:xi) = sum (filter even (x:xi))

--cantidad impares de una lista RECURSIVO
cantImpar :: [int] -> Int
cantImpar [] = 0
cantImpar(x:xs)
 | mod x 2 == 1 = 1 + cantImpar(xs)
 | otherwise = cantImpar(xs)

--cantidad impares de una lista LISTA DE COMPRENSION
listaImparFunciones::[Int]->Int
listaImparFunciones(x:xi) = length (filter odd (x:xi))


--lista dentro de otra lista RECURSIVO
contieneLista::[Int]->[Int]->Bool
contieneLista [] [] = True
contieneLista [] (y:ys) = True
contieneLista (x:xs) [] = True
contieneLista (x:xs) (y:ys) = lista y (x:xs) && contieneLista (x:xs) ys


--mayor de una lista
mayorLista::[Int]->Int
mayorLista[] = 0
mayorLista(x:[]) = x
mayorLista(x:xs)
 | x>mayorLista(xs) = x
 | otherwise = mayorLista(xs)
