--2.1 )

--pertenece :: (Eq t) => t -> [t] -> Bool segun 
--la siguiente especificacion:
--Eq es el conjunto de tipos de datps a los que se le puede aplicar {Bool Int Integer Float Double ...}
--Devuelve True si el elemento esta en la lista
pertenece :: (Eq t ) => t -> [t] -> Bool
pertenece _ [] = False --Hay 0 elementos
pertenece i [x] = (i == x )   -- Hay 1 elemento , este puede ir como no tambien porque esta el caso base del vacio  
pertenece i (x:xs)  | i == x = True
                    |otherwise = pertenece i (xs)

----------------------------------------------------------

--2.2)todosIguales :: (Eq t) => [t] -> Bool,
--todosIguales :: (Eq t) => [t] -> Bool, que dada una lista devuelve verdadero 
--si y solamente si todos sus elementos son iguales

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True -- Suponiendo que si no hay nada tambien son iguales por en una lista vacia no hay elementos entonces son iguales sus elementos 
todosIguales [x] = True
todosIguales (x:[y])   |x == y = True 
                        |otherwise = False
todosIguales (x:xs) | pertenece x (xs) == True = todosIguales (xs)
                    |otherwise = False


-------------------------------------------------------------------------
--2.3
--3. todosDistintos :: (Eq t) => [t] -> Bool 
--segun la siguiente especicacion:
--problema todosDistintos (s: seq⟨T⟩) : B 
--requiere:{True}
--asegura:{resultado = false ↔ existen dos posiciones distintas de s con igual valor }
--Si devuelve true es porque son ditintos todos los elementos 
--si devuelve false es porque un par de elementos son iguales 
todosDistintos2 :: (Eq t) => [t] -> Bool
todosDistintos2 [] = True -- Suponiendo que la lista vacia tiene elementos distintos por tener 0 elementos
todosDistintos2 [x] =True -- Como es 1 elemento tiene sus elementos distintos 
todosDistintos2 (x:[y])  | x /= y  = True
                        |otherwise = False --Con 2 elementos
todosDistintos2 (x:xs)  | pertenece x xs == True = False
                        |otherwise = todosDistintos2 (xs)



---------------------------------------------------------------------
--2.4 ) 4. hayRepetidos :: (Eq t) => [t] -> Bool seg´un la siguiente especicaci´on:
--problema hayRepetidos (s: seq⟨T⟩) : B {
--requiere: { True }
--asegura: { resultado = true ↔ existen dos posiciones distintas de s con igual valor }
--}

hayRepetidos2 :: (Eq t) => [t] ->Bool
hayRepetidos2 [] = False --tomemos que si esta vacio no es repetido 
hayRepetidos2 [x] = False  
hayRepetidos2 (x:xs) | pertenece x (xs) == True = True
                    |otherwise = hayRepetidos2 (xs)


--5. quitar :: (Eq t) => t -> [t] -> [t], 
--que dados un entero x y una lista xs, 
--elimina la primera aparicion de x en
--la lista xs (de haberla).
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x [y]    | x == y = []
                |otherwise = [y]

quitar i (x:xs)     | i == x = (xs) 
                    |otherwise =  x:(quitar i (xs)) 
----------------------------------------------------------------------------------


--6. quitarTodos :: (Eq t) => t -> [t] -> [t],
--que dados un entero x y una lista xs,
--elimina todas las apariciones de x en la lista xs (de haberlas). Es decir:

--problema quitarTodos (e: T, s: seq⟨T⟩) : seq⟨T⟩ {
--requiere: { True }
--asegura: { resultado es igual a s pero sin el elemento e. }
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x [y]   | x==y = []
                    |otherwise = [y]
quitarTodos i (x:xs)    | i == x  = quitarTodos i (xs)
                        | otherwise = x :(quitarTodos i (xs))    


----------------------------------------------------------------------------------
--7. eliminarRepetidos :: (Eq t) => [t] -> [t] 
--que deja en la lista una unica aparicion de cada elemento, eliminando
--las repeticiones adicionales.

eliminarRepetidos2 :: (Eq t) => [t] -> [t]
eliminarRepetidos2 [] = []
eliminarRepetidos2 [x] = [x]
eliminarRepetidos2 (x:[y])  | x == y = [x]
                            |otherwise = (x:[y])
eliminarRepetidos2 (x:xs)   |pertenece x (xs) == True  = x:eliminarRepetidos2 (quitarTodos x (xs))  
                            |otherwise = x: eliminarRepetidos2 (xs)  


--7. eliminarRepetidos :: (Eq t) => [t] -> [t] 
--que deja en la lista una unica aparicion de cada elemento, eliminando
--las repeticiones adicionales.
sonIguales :: (Eq t) => t -> [t] -> Bool
sonIguales x [] = False
sonIguales x  (y:ys)|x == y  = True
                    | otherwise = sonIguales x (ys)

eliminarRepetidos1 :: (Eq t) => [t] -> [t]
eliminarRepetidos1 [] = []
eliminarRepetidos1 (x:xs)    | sonIguales x xs  = eliminarRepetidos1(xs) 
                            |otherwise = x:eliminarRepetidos1(xs)
--------------------------------------------------------------------------------
--8. mismosElementos :: (Eq t) => [t] -> [t] -> Bool,
-- que dadas dos listas devuelve verdadero si y solamente si
--ambas listas contienen los mismos elementos, 
--sin tener en cuenta repeticiones,
--es decir:
--problema mismosElementos (s: seq⟨T⟩, r: seq⟨T⟩) : B {
--requiere: {True}
--asegura: {resultado = true ↔ todo elemento de s pertenece r y viceversa}
--}

mismosElementos :: (Eq t) =>[t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos _  [] = False
mismosElementos [] _ = False

mismosElementos [x] [y] | x == y = True
                        |otherwise = False
mismosElementos (x:xs) (y:ys) | pertenece x (y:ys) = mismosElementos (quitarTodos x xs)  (quitarTodos x (y:ys))
                        |otherwise = False

---------------------------------------------------------------------------------------

--9. capicua :: (Eq t) => [t] -> Bool segun la siguiente especificacion:
--problema capicua (s: seq⟨T⟩) : Bool {
--requiere: { True }
--asegura: { (resultado = true) ↔ (s = reverso(s)) }
--}
--Por ejemplo capicua ['a','c', 'b', 'b', 'c', 'a'] es true
--            capicua ['a', 'c', 'b', 'd', 'a']     es false
--Da true si son iguales y false si no lo son 

reverso :: [t] -> [t]
reverso (x:[]) =[x]
reverso  (x:xs) = reverso(xs) ++ [x]
---------------------------------------

capicua2 :: (Eq t ) => [t] ->Bool
capicua2 [] = True
capicua2 [x] = True
capicua2 (x :[y])| x == y  = True
                | otherwise = False
capicua2 (x:xs)  | (x:xs) == reverso (x:xs) = True
                |otherwise = False

















