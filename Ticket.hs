module Ticket (Ticket, nuevoT, salaT, peliculaT, usadoT, usarT, peliculaMenosVistaT, todosLosTicketsParaLaMismaSalaT, cambiarSalaT) 	where

import Tipos
import Pelicula
import Auxiliares

data Ticket = TicketSinUsar Sala Pelicula | TicketUsado Ticket deriving (Show, Eq)

nuevoT :: Pelicula -> Sala -> Bool -> Ticket
nuevoT p s u
  | u = TicketUsado t
  | otherwise = t
  where t = TicketSinUsar s p

peliculaT :: Ticket -> Pelicula
peliculaT (TicketUsado t) = peliculaT t
peliculaT (TicketSinUsar _ p) = p
-- Como el constructor permite el formato (TicketUsado TicketUsado TickeSinUsar sala peli),
-- recursivamente "saco los TicketUsado" hasta llegar a la pelicula en si.

salaT :: Ticket -> Sala
salaT (TicketUsado t) = salaT t
salaT (TicketSinUsar s _) = s

usadoT :: Ticket -> Bool
usadoT (TicketUsado _) = True
usadoT _ = False

usarT :: Ticket -> Ticket
usarT (TicketUsado _) = error "El ticket ya estaba usado"
usarT t = TicketUsado t

peliculaMenosVistaT :: [Ticket] -> Pelicula
peliculaMenosVistaT x = menosVista (pelisEnTickets x) x
  where menosVista [p] _ = p
        menosVista (a:b:ps) ts
          | vecesVista a ts <= vecesVista b ts = menosVista a:ps
          | otherwise = menosVista b:ps

vecesVista :: Pelicula -> [Ticket] -> In\
vecesVista _ [] = 0
vecesVista p (x:xs)
	| usadoT x && p == (peliculaT x) = 1 + vecesVista p xs
	| otherwise = vecesVista p xs

pelisEnTickets :: [Ticket] -> [Pelicula]
pelisEnTickets t = limpiarRepetidos todasLasPelis t
  where todasLasPelis [] = []
        todasLasPelis (t:ts) = peliculaT t:todasLasPelis ts

todosLosTicketsParaLaMismaSalaT :: [Ticket] -> Bool
todosLosTicketsParaLaMismaSalaT (t:tt:ts) = (salaT t == salaT tt) && todosLosTicketsParaLaMismaSalaT tt:ts
todosLosTicketsParaLaMismaSalaT _ = True

cambiarSalaT :: [Ticket] -> Sala -> Sala -> [Ticket]
cambiarSalaT [] _ _ = []
cambiarSalaT (t:ts) s1 s2
	| salaT t == s1 = nuevoT (pelicula t) s2 (usadoT t):cambiarSalaT ts s1 s2
	| otherwise = t:cambiarSalaT ts s1 s2
