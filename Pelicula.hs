module Pelicula (Pelicula, nuevaP, nombreP, generosP, actoresP, es3DP), agruparPelisPorGeneroP, generarSagaDePeliculasP) where

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
pelisDelGenero ((P n gs as b):ps) g
  | elem g gs = (P n gs as b):pelisDelGenero ps g
  | otherwise = pelisDelGenero ps g

generosDePelis :: [Pelicula] -> [Genero]
generosDePelis ps = obtenerGeneros ps []
  where obtenerGeneros [] x = x
        obtenerGeneros ((P _ [] _ _):ps) xs = obtenerGeneros ps xs
        obtenerGeneros ((P n (g:gs) as b):ps) xs
          | elem g xs  = obtenerGeneros  ((P n gs as b):ps) xs
          | otherwise = obtenerGeneros ((P n gs as b):ps) (g:xs)

generarSagaDePeliculasP :: [Actor] -> [Genero] -> [Nombre] -> [Pelicula]
generarSagaDePeliculasP as gs (n:xs) = [P n gs as False] ++ generarSagaDePeliculasP as gs xs
generarSagaDePeliculasP _ _ _ = []
