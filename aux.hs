-- 1era funcion aux
sinRepetidos :: (Eq a) => [a] -> Bool
sinRepetidos (x:xs) = distintoAtodos x xs && sinRepetidos xs
sinRepetidos _ = True

distintoAtodos :: (Eq a) => a -> [a] -> Bool
distintoAtodos x (y:xs) = not (x == y) && distintoAtodos x xs
distintoAtodos _ _ = True

-- 2da funcion aux
listaOrdenada :: (Ord a) => [a] -> Bool
listaOrdenada (x:y:xs) = x <= y && listaOrdenada (y:xs)
listaOrdenada _ = True
