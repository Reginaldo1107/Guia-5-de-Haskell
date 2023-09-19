--pertenece :: (Eq t) => t -> [t] -> Bool segun 
--la siguiente especificacion:
--Eq es el conjunto de tipos de datps a los que se le puede aplicar {Bool Int Integer Float Double ...}
--Devuelve True si el elemento esta en la lista
pertenece :: (Eq t) => t ->[t] -> Bool
pertenece n []  = False  
pertenece n (x:xs) | n == x   = True 
                   | otherwise = pertenece n (xs) 

--o 
pertenece2 :: (Eq t) => t ->[t] -> Bool
pertenece2 n (x:[]) | n/=x  = False  
pertenece2 n (x:xs) | n == x   = True 
                    | otherwise = pertenece2 n (xs) 

--o 
pertenece3 :: (Eq t) => t -> [t] -> Bool
pertenece3 _ [] = False
pertenece3 n (x:xs) | x == n = True
                    | otherwise = n `pertenece3` xs

----------------------------------------------------------

--2.2)todosIguales :: (Eq t) => [t] -> Bool,
--todosIguales :: (Eq t) => [t] -> Bool, que dada una lista devuelve verdadero 
--si y solamente si todos sus elementos son iguales
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = (x == head xs)  && todosIguales (xs)  


-------------------------------------------------------------------------
--2.3








































































--------------------------------------------------------
--compararUnoConLaLista :: (Eq t) => t -> [t] -> Bool
--compararUnoConLaLista x [] = False
--compararUnoConLaLista x (y:ys)  | x == y = True
--                                |otherwise = compararUnoConLaLista x (ys)


--todosIguales :: (Eq t) => [t] -> Bool 
--todosIguales (x:[]) = False  
--todosIguales (x:xs) | compararUnoConLaLista x (xs) == True = True 
    --                |otherwise = todosIguales(xs)

-----------------------------------------------------------------