module Ticket (Ticket, nuevoT, salaT, peliculaT, usadoT, usarT, peliculaMenosVistaT, todosLosTicketsParaLaMismaSalaT, cambiarSalaT) 	where

import Tipos
import Pelicula

data Ticket = TicketSinUsar Sala Pelicula | TicketUsado Ticket deriving (Show, Eq)

todosLosTicketsParaLaMismaSalaT :: [Ticket] -> Bool
  todosLosTicketsParaLaMismaSalaT [] = True
  todosLosTicketsParaLaMismaSalaT [_] = True
  todosLosTicketsParaLaMismaSalaT t:tt:ts = (sala t == sala tt) && todosLosTicketsParaLaMismaSalaT tt:ts

cambiarSalaT :: [Ticket] -> Sala -> Sala -> [Ticket]
  cambiarSalaT [] _ _ = []
  cambiarSalaT (t:ts) s1 s2
    | sala t == s1 = (nuevoT (pelicula t) s2 usadoT t ) : cambiarSalaT ts s1 s2
    | otherwise = t : cambiarSalaT ts s1 s2
