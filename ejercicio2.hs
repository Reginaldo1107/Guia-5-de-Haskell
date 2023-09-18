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
pertenece2 n (x:[])| n/=x  = False  
pertenece2 n (x:xs) | n == x   = True 
                   | otherwise = pertenece2 n (xs) 

--o 
pertenece3 :: (Eq t) => t -> [t] -> Bool
pertenece3 _ [] = False
pertenece3 n (x:xs)
    | x == n = True
    | otherwise = n `pertenece3` xs


