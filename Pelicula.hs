module Pelicula (Pelicula, nuevaP, nombreP, generosP, actoresP, es3DP,
 agruparPelisPorGeneroP, generarSagaDePeliculasP) where

import Tipos

data Pelicula = P Nombre [Genero] [Actor] Bool deriving (Show, Eq)

nuevaP:: Nombre -> [Genero] -> [Actor] -> Bool -> Pelicula
nuevaP n gs as b = P n gs as b

nombreP :: Pelicula -> Nombre
nombreP (P n _ _ _) = n

generosP :: Pelicula -> [Genero]
generosP (P _ gs _ _) = gs

actoresP :: Pelicula -> [Actor]
actoresP (P _ _ as _) = as

es3DP :: Pelicula -> Bool
es3DP (P _ _ _ b) = b

agruparPelisPorGeneroP :: [Pelicula] -> [(Genero, [Pelicula])]
agruparPelisPorGeneroP ps = pelisPorGeneros (generosDePelis ps)
  where pelisPorGeneros [] = []
        pelisPorGeneros (g:gs) = (g, pelisDelGenero ps g):pelisPorGeneros gs

pelisDelGenero :: [Pelicula] -> Genero -> [Pelicula]
pelisDelGenero  [] g = []
pelisDelGenero (p:ps) g
  | elem g (generosP p) = p:pelisDelGenero ps g
  | otherwise = pelisDelGenero ps g

generosDePelis :: [Pelicula] -> [Genero]
generosDePelis ps = limpiarRepetidos (obtenerGeneros ps)
  where obtenerGeneros [] = []
        obtenerGeneros (p:ps) = generosP p ++ obtenerGeneros ps

generarSagaDePeliculasP :: [Actor] -> [Genero] -> [Nombre] -> [Pelicula]
generarSagaDePeliculasP as gs (n:xs) = 
nuevaP n gs as False:generarSagaDePeliculasP as gs xs
generarSagaDePeliculasP _ _ _ = []

limpiarRepetidos :: (Eq a) => [a] -> [a]
limpiarRepetidos [] = []
limpiarRepetidos (x:xs)
  | elem x xs = limpiarRepetidos xs
  | otherwise = x:limpiarRepetidos xs
