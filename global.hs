
{-- ENUNCIADO
Defina una función llamada parImpar- en versiones con patrones y con guardas - 
que recibe una lista de enteros y devuelve una tupla con dos listas, la primera con los elementos pares y 
la segunda con los impares (en caso que no contenga alguno de ellos devuelve la lista vacía). 
Puede definir funciones auxiliares y usar mod. Por ejemplo: parImpar [10,12,13,14,4,7] => ([10,12,14,4], [13,7])} 
--}

--Con patrones

parImpar :: [Int] -> ([Int], [Int])
parImpar []     = ([], [])
parImpar (x:xs) =
  if x `mod` 2 == 0
  then let (pares, impares) = parImpar xs in (x : pares, impares)
  else let (pares, impares) = parImpar xs in (pares, x : impares)

--Con guardas

parImpar :: [Int] -> ([Int], [Int])
parImpar [] = ([], [])
parImpar (x:xs)
  | x `mod` 2 == 0 = (x : pares, impares)
  | otherwise      = (pares, x : impares)
  where (pares, impares) = parImpar xs

{-- ENUNCIADO
Efectuar las definiciones type necesarias y luego la función recuperar- en versiones con patrones y con guardas - que
devuelve el valor de la matriz (vector de vectores), que se ubica en la posición de fila y columna (se numeran desde 0),
que se indique en la llamada a la función (puede usar el operador!!) Por ejemplo sea m = [[1,2,3], [4,5,6], [7,8,9]]
entonces: recuperar m 2 1 => 8
--}

--Con patrones

recuperar :: [[Int]] -> Int -> Int -> Int
recuperar matriz fila columna = (matriz!!fila)!!columna

--Con guardas

recuperar :: [[Int]] -> Int -> Int -> Int
recuperar matriz fila columna
  | fila < 0 || fila >= length matriz || columna < 0 || columna >= length (head matriz) = error "Índices fuera de rango"
  | otherwise = (matriz !! fila) !! columna
