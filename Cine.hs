module Cine (nuevoC, nombreC, peliculasC, salasC, espectadoresC, salaC, ticketsVendidosC,  abrirSalaC, agregarPeliculaC, cerrarSalaC, cerrarSalasC, cerrarSalasDeLaCadenaC, peliculaC, venderTicketC, ingresarASalaC, pasarA3DUnaPeliculaC ) where

import Tipos
import Pelicula
import Ticket

data Cine = C Nombre | 
			SalaSinPelicula Cine Sala | 
			SalaConPelicula Cine Sala Pelicula Int | 
			TicketVendido Cine Ticket deriving (Show)

nuevoC :: String -> Cine
nuevoC n = C n

nombreC :: Cine -> String
nombreC C n = n
nombreC SalaSinPelicula c _ = nombreC c
nombreC SalaConPelicula c _ _ _ = nombreC c
nombreC TicketVendido c _ = nombreC c

peliculasC :: Cine -> [Pelicula]
peliculasC C _ = []
peliculasC SalaSinPelicula c _ = peliculasC c
peliculasC SalaConPelicula c _ p _ = p:peliculasC c
peliculasC TicketVendido c _ = peliculasC c

salasC :: Cine -> [Sala]
salasC C _ = []
salasC SalaSinPelicula c s = s:salasC c
salasC SalaConPelicula c s _ _ = s:salasC c
salasC TicketVendido c _ = salasC c

espectadoresC :: Cine -> Sala -> Int
espectadoresC (SalaSinPelicula c r) s
				|r==s = 0
				|otherwise espectadoresC c s
espectadoresC (SalaConPelicula c r _ i) s 
				|r==s = i
				|otherwise espectadoresC c s
espectadoresC (TicketVendido c _) s = espectadoresC c s

salaC :: Cine -> Pelicula -> Sala
salaC (SalaSinPelicula c _) p = salaC c p
salaC (SalaConPelicula c s q _) p 
				|q==p = s
				|otherwise salaC c p
salaC (TicketVendido c _) p = salaC c p

ticketsVendidosC :: Cine -> [Ticket]
ticketsVendidosC C _ = []
ticketsVendidosC (SalaSinPelicula c _) = ticketsVendidosC c 
ticketsVendidosC (SalaConPelicula c _ _ _) = ticketsVendidosC c
ticketsVendidosC (TicketVendido c t)
				|usadoT t = ticketsVendidosC c
				|otherwise = t:(ticketsVendidosC c)

abrirSalaC :: Cine -> Sala -> Cine
abrirSalaC c s = SalaSinPelicula c s



