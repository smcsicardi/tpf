-- 1era funcion aux
sinRepetidos :: (Eq a) => [a] -> Bool
sinRepetidos (x:xs) = not (x == y) && distintoAtodos x xs
sinRepetidos _ = True

-- 2da funcion aux
listaOrdenada :: (Ord a) => [a] -> Bool
listaOrdenada (x:y:xs) = x <= y && listaOrdenada (y:xs)
listaOrdenada _ = True
