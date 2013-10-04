module Cine (nuevoC, nombreC, peliculasC, salasC, espectadoresC, salaC, ticketsVendidosC,  abrirSalaC, agregarPeliculaC, cerrarSalaC, cerrarSalasC, cerrarSalasDeLaCadenaC, peliculaC, venderTicketC, ingresarASalaC, pasarA3DUnaPeliculaC ) where

import Tipos
import Pelicula
import Ticket

data Cine = C Nombre | 
			SalaSinPelicula Cine Sala | 
			SalaConPelicula Cine Sala Pelicula Int | 
			TicketVendido Cine Ticket deriving (Show)

nuevoc :: String -> Cine
nuevoc n = C n

nombrec :: Cine -> String
nombrec C n = n
nombrec SalaSinPelicula c _ = nombrec c
nombrec SalaConPelicula c _ _ _ = nombrec c
nombrec TicketVendido c _ = nombrec c

peliculasc :: Cine -> [Pelicula]
peliculasc C _ = []
peliculasc SalaSinPelicula c _ = peliculasc c
peliculasc SalaConPelicula c _ p _ = p:peliculasc c
peliculasc TicketVendido c _ = peliculasc c

salasc :: Cine -> [Sala]
salasc C _ = []
salasc SalaSinPelicula c s = s:salasc c
salasc SalaConPelicula c s _ _ = s:salasc c
salasc TicketVendido c _ = salasc c
