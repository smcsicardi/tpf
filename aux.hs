-- 1era funcion aux
sinRepetidos :: (Eq a) => [a] -> Bool
sinRepetidos (x:xs) = not (elem x xs) && sinRepetidos xs
sinRepetidos _ = True

-- 2da funcion aux
listaOrdenada :: (Ord a) => [a] -> Bool
listaOrdenada (x:y:xs) = x <= y && listaOrdenada (y:xs)
listaOrdenada _ = True

-- 4ta funcion aux
primeros :: [(a,b)] -> [a]
primeros (x:xs) = fst x : primeros xs
primeros _ = []
