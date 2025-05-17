{-
UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO
Facultad de Ciencias
Estructuras Discretas
2025-2

Práctica 3

Profesor:
  Ulises Rodríguez Domínguez

Ayudantes de teoría:
  Irvin Javier Cruz González

Ayudante de laboratorio:
  Martínez Dámasto Raúl Eduardo

Nombre: Castañeda Bermeo Jorge Erik
-}

data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz _ izq der) = 1 + longitud izq + longitud der

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izq der) = ancho izq + ancho der

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz x izq der) InOrder = recorrido izq InOrder ++ [x] ++ recorrido der InOrder
recorrido (Raiz x izq der) PreOrder = [x] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz x izq der) PosOrder = recorrido izq PosOrder ++ recorrido der PosOrder ++ [x]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz x izq der) = [x] : combinar (niveles izq) (niveles der)
  where
    combinar [] ys = ys
    combinar xs [] = xs
    combinar (x:xs) (y:ys) = (x ++ y) : combinar xs ys

-------------------- EJERCICIO 6 --------------------
minimo :: Ord a => Arbol a -> a 
minimo ArbolVacio = error "Arbol vacio"
minimo (Raiz x ArbolVacio _) = x
minimo (Raiz _ izq _) = minimo izq

-------------------- EJERCICIO 7 --------------------
maximo :: Ord a => Arbol a -> a 
maximo ArbolVacio = error "Arbol vacio"
maximo (Raiz x ArbolVacio ArbolVacio) = x
maximo (Raiz x izq der) =
  let
    maxIzq = case izq of
               ArbolVacio -> x
               _ -> maximo izq
    maxDer = case der of
               ArbolVacio -> x
               _ -> maximo der
  in
    if x >= maxIzq && x >= maxDer then x
    else if maxDer >= maxIzq then maximo der
    else maximo izq

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz x izq der) n
  | n < x = Raiz x (eliminar izq n) der
  | n > x = Raiz x izq (eliminar der n)
  | otherwise = eliminarRaiz (Raiz x izq der)
  where
    eliminarRaiz (Raiz _ ArbolVacio ArbolVacio) = ArbolVacio
    eliminarRaiz (Raiz _ izq ArbolVacio) = izq
    eliminarRaiz (Raiz _ ArbolVacio der) = der
    eliminarRaiz (Raiz _ izq der) = 
      let m = maximo izq 
      in Raiz m (eliminar izq m) der

-------------------- MAIN (para compilación) --------------------
main :: IO ()
main = do
  let ejemplo = Raiz 5 (Raiz 3 ArbolVacio ArbolVacio) (Raiz 8 (Raiz 7 ArbolVacio ArbolVacio) ArbolVacio)
  putStrLn "Ejemplo de árbol:"
  print ejemplo
  putStrLn "longitud:" >> print (longitud ejemplo)
  putStrLn "profundidad:" >> print (profundidad ejemplo)
  putStrLn "ancho:" >> print (ancho ejemplo)
  putStrLn "recorrido InOrder:" >> print (recorrido ejemplo InOrder)
  putStrLn "recorrido PreOrder:" >> print (recorrido ejemplo PreOrder)
  putStrLn "recorrido PosOrder:" >> print (recorrido ejemplo PosOrder)
  putStrLn "niveles:" >> print (niveles ejemplo)
  putStrLn "minimo:" >> print (minimo ejemplo)
  putStrLn "maximo:" >> print (maximo ejemplo)
  putStrLn "eliminar 3:" >> print (eliminar ejemplo 3)


