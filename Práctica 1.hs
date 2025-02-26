
-- Universidad Nacional Autónoma de México
-- Facultad de Ciencias
-- Estructuras Discretas 2025-2
-- Introducción a Haskell
-- Profesor: Ulises Rodríguez Domínguez
-- Ayudantes de teoría: Irvin Javier Cruz González
-- Ayudante de laboratorio: Martínez Dámaso Raúl Eduardo
-- Alumno: Castañeda Bermeo Jorge Erik

module Main where

import Prelude hiding (sqrt)
import GHC.Float (sqrt)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

valorAbsoluto :: Int -> Int
valorAbsoluto x = if x < 0 then -x else x

pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt (b * b + h * h)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = 
    let d = sqrt (b * b - 4 * a * c)
    in ((-b + d) / (2 * a), (-b - d) / (2 * a))

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = 
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

esBisiesto :: Int -> Bool
esBisiesto año = (año `mod` 4 == 0 && año `mod` 100 /= 0) || (año `mod` 400 == 0)

comparador :: Int -> Int -> Int
comparador x y = if x == y then 0 else if x < y then -1 else 1

maximo :: Int -> Int -> Int -> Int
maximo x y z = if x >= y && x >= z then x else if y >= x && y >= z then y else z

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = x > y && y > z && z > w

main :: IO ()
main = do
    print (distanciaPuntos (2.0, 1.0) (5.0, 5.0))
    print (valorAbsoluto (-1))
    print (pendiente (3.0, 2.0) (7.0, 8.0))
    print (hipotenusa 9.0 12.0)
    print (raices 1.0 (-5.0) 6)
    print (areaTriangulo 13 15 14)
    print (esBisiesto 2025)
    print (comparador 5 5)
    print (maximo 8 9 9)
    print (esDescendente 10 9 8 7)
