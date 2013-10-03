module Pelicula (Pelicula, nuevaP, nombreP, generosP, actoresP, es3DP, agruparPelisPorGeneroP, generarSagaDePeliculasP) where

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
