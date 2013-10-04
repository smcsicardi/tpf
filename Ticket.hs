module Ticket (Ticket, nuevoT, salaT, peliculaT, usadoT, usarT, peliculaMenosVistaT, todosLosTicketsParaLaMismaSalaT, cambiarSalaT) 	where

import Tipos
import Pelicula

data Ticket = TicketSinUsar Sala Pelicula | TicketUsado Ticket deriving (Show, Eq)

nuevoT :: Pelicula -> Sala -> Bool -> Ticket
nuevoT p s esUsado
	| esUsado = TicketUsado (t)
	| otherwise = t
		where t = TicketSinUsar s p

peliculaT :: Ticket -> Pelicula
peliculaT (TicketSinUsar _ x) = x
peliculaT (TicketUsado (TicketSinUsar _ x)) = x
peliculaT _ = error "No es un ticket valido"

salaT :: Ticket -> Sala
salaT (TicketSinUsar  x _) = x
salaT (TicketUsado (TicketSinUsar x _)) = x
salaT _ = error "No es un ticket valido"

usadoT :: Ticket -> Bool
usadoT (TicketUsado _) = True
usadoT (TicketSinUsar _ _) = False

usarT :: Ticket -> Ticket
usarT (TicketSinUsar s p) = TicketUsado (TicketSinUsar s p)
usarT (TicketUsado _) = error "El ticket ya estaba usado"


-- ##### acá COMIENZA peliculaMenosVistaT

peliculaMenosVistaT :: [Ticket] -> Pelicula
peliculaMenosVistaT x = menosVista (psEnTickets x) x

-- toma la lista de todas las peliculas que hay en los tickets (sin repetidas) y la lista de tickets
menosVista :: [Pelicula] -> [Ticket] -> Pelicula
menosVista [p] _ = p
menosVista (p:ps) ts
	| vecesVista p ts <= vecesVista (laMenosVista) ts = p
	| otherwise = laMenosVista
		where laMenosVista = menosVista ps ts

-- en la especificación esta función se llama 'sumaUsado'; creo que con este nombre se entiende mejor su función
vecesVista :: Pelicula -> [Ticket] -> Integer
vecesVista _ [] = 0
vecesVista p (x:xs)
	| usadoT x && p == (peliculaT x) = 1 + vecesVista p xs
	| otherwise = vecesVista p xs

-- obtiene la lista de todas las peliculas que aparecen en una lista de tickets
psEnTickets :: [Ticket] -> [Pelicula]
psEnTickets x = pelTsAux x []

pelTsAux :: [Ticket] -> [Pelicula] -> [Pelicula]
pelTsAux (x:xs) y
	| sinRepetidos nl = pelTsAux xs nl
	| otherwise = pelTsAux xs y
		where nl = y ++ [peliculaT x]
pelTsAux _ y = y

-- aux
sinRepetidos :: [Pelicula] -> Bool
sinRepetidos (x:xs) = distintoAtodos x xs && sinRepetidos xs
sinRepetidos _ = True

distintoAtodos :: Pelicula -> [Pelicula] -> Bool
distintoAtodos x (y:xs) = not (x == y) && distintoAtodos x xs
distintoAtodos _ _ = True

-- ##### acá TERMINA peliculaMenosVistaT

todosLosTicketsParaLaMismaSalaT :: [Ticket] -> Bool
  todosLosTicketsParaLaMismaSalaT [] = True
  todosLosTicketsParaLaMismaSalaT [_] = True
  todosLosTicketsParaLaMismaSalaT t:tt:ts = (sala t == sala tt) && todosLosTicketsParaLaMismaSalaT tt:ts

cambiarSalaT :: [Ticket] -> Sala -> Sala -> [Ticket]
  cambiarSalaT [] _ _ = []
  cambiarSalaT (t:ts) s1 s2
    | sala t == s1 = (nuevoT (pelicula t) s2 usadoT t ) : cambiarSalaT ts s1 s2
    | otherwise = t : cambiarSalaT ts s1 s2
