module Auxiliares (sinRepetdos, listaOrdenada, primeros, limpiarRepetidos) where

sinRepetidos :: (Eq a) => [a] -> Bool
sinRepetidos (x:xs) = not (elem x xs) && sinRepetidos xs
sinRepetidos _ = True

listaOrdenada :: (Ord a) => [a] -> Bool
listaOrdenada (x:y:xs) = x <= y && listaOrdenada (y:xs)
listaOrdenada _ = True

primeros :: [(a,b)] -> [a]
primeros (x:xs) = fst x : primeros xs
primeros _ = []

limpiarRepetidos :: (Eq a) => [a] -> [a]
limpiarRepetidos [] = []
limpiarRepetidos (x:xs)
  | elem x xs = limpiarRepetidos xs
  | otherwise = x:limpiarRepetidos xs
