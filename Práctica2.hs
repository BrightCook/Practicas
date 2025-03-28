{-
UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO
Facultad de Ciencias
Estructuras Discretas
2025-2

Práctica 2

Profesor:
  Ulises Rodríguez Domínguez

Ayudantes de teoría:
  Irvin Javier Cruz González

Ayudante de laboratorio:
  Martínez Dámasto Raúl Eduardo

Nombre: Castañeda Bermeo Jorge Erik
-}

-- ===== LISTAS CON RECURSIÓN =====

-- 1. Longitud de una lista
longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- 2. Suma de elementos numéricos
sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-- 3. Agregar elemento (principio/final)
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento xs x True = x : xs
agregaElemento xs x False = xs ++ [x]

-- 4. Máximo de una lista
maximoLista :: (Ord a, Num a) => [a] -> a
maximoLista [] = error "Lista vacía"
maximoLista [x] = x
maximoLista (x:xs) = max x (maximoLista xs)

-- 5. Recuperar elemento por índice
recuperarElemento :: [a] -> Int -> a
recuperarElemento xs n
    | n < 0     = error "Índice no válido"
    | otherwise = case xs of
        []      -> error "Índice no válido"
        (y:ys)  -> if n == 0 then y else recuperarElemento ys (n - 1)

-- ===== LISTAS POR COMPRENSIÓN =====

-- 1. Divisores de un número
divisoresDeN :: Int -> [Int]
divisoresDeN n = [x | x <- [1..n], n `mod` x == 0]

-- 2. Lista a conjunto (sin duplicados)
conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista xs = [x | (x, i) <- zip xs [0..], x `notElem` take i xs]

-- 3. Filtrar números pares
soloPares :: [Int] -> [Int]
soloPares xs = [x | x <- xs, even x]

-- ===== EJECUCIÓN DEL CÓDIGO =====
main :: IO ()
main = do
    putStrLn "\n--- Casos ---"
    
    -- Pruebas longitud
    print $ longitud [5,4,6]      -- Debe mostrar 3
    
    -- Pruebas sumaLista
    print $ sumaLista [1,2,3,4]     -- Debe mostrar 10
    
    -- Pruebas agregaElemento
    print $ agregaElemento [1,2,3] 4 False  -- [1,2,3,4]
    
    -- Pruebas maximoLista
    print $ maximoLista [10,9,17,7] -- Debe mostrar 17
    
    -- Pruebas recuperarElemento
    print $ recuperarElemento [2,4,6] 1  -- Debe mostrar 4
    
    -- Pruebas divisoresDeN
    print $ divisoresDeN 10       -- Debe mostrar [1,2,5,10]
    
    -- Pruebas conjuntoLista
    print $ conjuntoLista [1,2,2,1,3]  -- Debe mostrar [1,2,3]
    
    -- Pruebas soloPares
    print $ soloPares [1,2,3,4]    -- Debe mostrar [2,4]
