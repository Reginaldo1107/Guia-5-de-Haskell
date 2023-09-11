--Repaso De Cero :

-- head : Escoge el primero de la lista y lo devuelve 
--ejemplo si fuera una lista de enteros devolveria el 
--primer entero 
--[1,2,3,4,5] --> 1  
-- tail :Elimina el primero de la lista y devuelve la lista sin ese tipo ejemplo :
--[1,2,3,4,5] --> [2,3,4,5]

--lo que significa longitud (x:xs) es la lista -1 ¿Como?
--ejemplo : digamos que tengo una lista [1,2,3,4,5]
--mi x = 1 y mi xs = [2,3,4,5] , digamos como que lo 
--divide 

--Tambien puedo hacer cosas locas como separarlo hasta donde yo quiera , en este caso lo separare 2 veces :
--Sea mi lista [3,2,1,2,3]
--(x:y:xs)
--x = 3
--y = 2
--xs = [1,2,3]

--longitud :: [t] -> Integer
--longitud [] = 0 
--longitud (x:xs) = 1 + longitud (xs)
--Eso es todo lo que se hasta ahora :c
--------------------------------------------------------
-- 1.1) longitud :: [t] -> Integer , que dada una lista devuelve su cantidad de elementos.
longitud :: [t]-> Integer
longitud [] = 0 
longitud (x:xs) = 1 + longitud(xs)

-- 1.2)
ultimo :: [t] -> t 
--según la siguiente especificación:
ultimo (x:[]) = x
ultimo (x:xs) |otherwise =  ultimo(xs)

