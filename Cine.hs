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
nombreC (C n) = n
nombreC (SalaSinPelicula c _) = nombreC c
nombreC (SalaConPelicula c _ _ _) = nombreC c
nombreC (TicketVendido c _) = nombreC c

peliculasC :: Cine -> [Pelicula]
peliculasC (C _) = []
peliculasC (SalaSinPelicula c _) = peliculasC c
peliculasC (SalaConPelicula c _ p _) = p:peliculasC c
peliculasC (TicketVendido c _) = peliculasC c

salasC :: Cine -> [Sala]
salasC (C _) = []
salasC (SalaSinPelicula c s) = s:salasC c
salasC (SalaConPelicula c s _ _) = s:salasC c
salasC (TicketVendido c _) = salasC c

espectadoresC :: Cine -> Sala -> Int
espectadoresC (SalaSinPelicula c r) s
	|r==s = 0
	|otherwise = espectadoresC c s
espectadoresC (SalaConPelicula c r _ i) s 
	|r==s = i
	|otherwise = espectadoresC c s
espectadoresC (TicketVendido c _) s = espectadoresC c s

salaC :: Cine -> Pelicula -> Sala
salaC (SalaSinPelicula c _) p = salaC c p
salaC (SalaConPelicula c s q _) p 
	|q==p = s
	|otherwise = salaC c p
salaC (TicketVendido c _) p = salaC c p

ticketsVendidosC :: Cine -> [Ticket]
ticketsVendidosC (C _) = []
ticketsVendidosC (SalaSinPelicula c _) = ticketsVendidosC c 
ticketsVendidosC (SalaConPelicula c _ _ _) = ticketsVendidosC c
ticketsVendidosC (TicketVendido c t)
	|usadoT t = ticketsVendidosC c
	|otherwise = t:(ticketsVendidosC c)

abrirSalaC :: Cine -> Sala -> Cine
abrirSalaC c s = SalaSinPelicula c s

agregarPeliculaC :: Cine -> Pelicula -> Sala -> Cine
agregarPeliculaC (SalaSinPelicula c sa) p s 
	|s == sa = SalaConPelicula c s p 0
	|otherwise = SalaSinPelicula (agregarPeliculaC c p s) sa
agregarPeliculaC (SalaConPelicula c sa pe e) p s = SalaConPelicula (agregarPeliculaC c p s) sa pe e
agregarPeliculaC (TicketVendido c t) p s = TicketVendido (agregarPeliculaC c p s) t

cerrarSalaC :: Cine -> Sala -> Cine
cerrarSalaC (C n) s = C n
cerrarSalaC (SalaSinPelicula c sc) s
	| s == sc = c
	| otherwise = SalaSinPelicula (cerrarSalaC c s) sc
cerrarSalaC (SalaConPelicula c sc pc ic) s
	| s == sc = c
	| otherwise = SalaConPelicula (cerrarSalaC c s) sc pc ic
cerrarSalaC (TicketVendido c tc) s = TicketVendido (cerrarSalaC c s) tc

cerrarSalasC :: Cine -> Int -> Cine
cerrarSalasC (C n) _ = C n
cerrarSalasC (SalaSinPelicula c _) e = cerrarSalasC c e
cerrarSalasC (SalaConPelicula c s p i) e
	| i < e = cerrarSalasC c e
	| otherwise = SalaConPelicula (cerrarSalasC c e) s p i
cerrarSalasC (TicketVendido c t) e = TicketVendido (cerrarSalasC c e) t

cerrarSalasDeLaCadenaC :: [Cine] -> Int -> [Cine]
cerrarSalasDeLaCadenaC [] _ = []
cerrarSalasDeLaCadenaC (x:xs) e = (cerrarSalasC x e):(cerrarSalasDeLaCadenaC xs e)

peliculaC :: Cine -> Sala -> Pelicula
peliculaC (SalaSinPelicula c _) s = peliculaC c s
peliculaC (SalaConPelicula c t p _) s 
	|t == s = p
	|otherwise = peliculaC c s
peliculaC (TicketVendido c _) s = peliculaC c s

venderTicketC :: Cine -> Pelicula -> (Cine, Ticket)
venderTicketC c p = (TicketVendido c t, t)
	where t = nuevoT p (salaC c p) False

ingresarASalaC :: Cine -> Sala -> Ticket -> (Cine, Ticket)
ingresarASalaC c s t = (cineConIngreso c s t, usarT t)
	
cineConIngreso :: Cine -> Sala -> Ticket -> Cine
cineConIngreso (SalaSinPelicula c sa) s t = SalaSinPelicula (cineConIngreso c s t) sa
cineConIngreso (SalaConPelicula c sa p e) s t = SalaConPelicula (cineConIngreso c s t) sa p e
cineConIngreso (TicketVendido c ti) s t 
  |ti == t = agregaEspectadorASala c s
  |otherwise = TicketVendido (cineConIngreso c s t) ti

agregaEspectadorASala :: Cine -> Sala -> Cine  
agregaEspectadorASala (SalaSinPelicula c sa) s = SalaSinPelicula (agregaEspectadorASala c s) sa
agregaEspectadorASala (SalaConPelicula c sa p e) s
	|sa == s = SalaConPelicula c sa p (e+1)
	|otherwise = SalaConPelicula (agregaEspectadorASala c s) sa p e
agregaEspectadorASala (TicketVendido c t) = TicketVendido (agregaEspectadorASala c s) t

pasarA3DUnaPeliculaC :: Cine -> Nombre -> (Cine,Pelicula)
pasarA3DUnaPeliculaC c n = (cineConPeliA3D c n, peliA3D (peliDelCine n c))
	
peliA3D :: Pelicula -> Pelicula
peliA3D p = nuevaP (nombreP p) (generosP p) (actoresP p) True

peliDelCine :: Nombre -> Cine -> Pelicula
peliDelCine n (SalaConPelicula c _ p _)
	| nombreP p == n = p
	| otherwise = peliDelCine n c
peliDelCine n (SalaSinPelicula c s) = peliDelCine n c
peliDelCine n (TicketVendido c t) = peliDelCine n c

cineConPeliA3D :: Cine -> Nombre -> Cine
cineConPeliA3D (C nom) n = C nom
cineConPeliA3D (SalaSinPelicula c s) n = SalaSinPelicula (cineConPeliA3D c n) s
cineConPeliA3D (SalaConPelicula c s p e) n
	|nombreP p == n = SalaConPelicula (cineConPeliA3D c n) s (peliA3D p) e
	|otherwise = SalaConPelicula (cineConPeliA3D c n) s p e
cineConPeliA3D (TicketVendido c t) n = TicketVendido (cineConPeliA3D c n) t
