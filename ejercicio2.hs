--2.1 )

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
--3. todosDistintos :: (Eq t) => [t] -> Bool 
--segun la siguiente especicacion:
--problema todosDistintos (s: seq⟨T⟩) : B 
--requiere:{True}
--asegura:{resultado = false ↔ existen dos posiciones distintas de s con igual valor }0
--Si devuelve true es porque son ditintos 
--si devuelve false es porque el elemento es igual al de la lista
verificarQueSeanDistintos :: (Eq t) => t -> [t] -> Bool
verificarQueSeanDistintos x [] = True
verificarQueSeanDistintos x  (y:ys) |x == y  = False
                                    | otherwise = verificarQueSeanDistintos x (ys)

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) | verificarQueSeanDistintos x xs == False  = False
                      |otherwise = todosDistintos (xs)

---------------------------------------------------------------------
--2.4 ) 4. hayRepetidos :: (Eq t) => [t] -> Bool seg´un la siguiente especicaci´on:
--problema hayRepetidos (s: seq⟨T⟩) : B {
--requiere: { True }
--asegura: { resultado = true ↔ existen dos posiciones distintas de s con igual valor }
--}
verificarQueSeanIguales :: (Eq t) => t -> [t] -> Bool
verificarQueSeanIguales x [] = False
verificarQueSeanIguales x  (y:ys) |x == y  = True
                                    | otherwise = verificarQueSeanIguales x (ys)

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) | verificarQueSeanIguales x xs == True  = True
                      |otherwise = hayRepetidos (xs)

----------------------------------------------------------------------------------
--5. quitar :: (Eq t) => t -> [t] -> [t], 
--que dados un entero x y una lista xs, 
--elimina la primera aparicion de x en
--la lista xs (de haberla).
recorrerLaLista :: (Eq t) => [t]->[t]
recorrerLaLista[] = []
recorrerLaLista (x:[]) = [x]
recorrerLaLista (x:xs) = x:recorrerLaLista(xs)

quitar :: (Eq t) => t -> [t] -> [t]
quitar x [] = []
quitar x [y] |x==y = []
             |otherwise = [y]
quitar x (y:ys) | x == y = recorrerLaLista(ys)
                |otherwise = y:(quitar x (ys))             


--o

quitar2 :: (Eq t) => t -> [t] -> [t]
quitar2 _ [] = []
quitar2 x (y:xs) | x == y = xs
                | otherwise = y : quitar2 x xs

--Quiero entrar buscando de a uno x:(xs)

--------------------------------------

--6. quitarTodos :: (Eq t) => t -> [t] -> [t],
--que dados un entero x y una lista xs,
--elimina todas las apariciones de x en la lista xs (de haberlas). Es decir:

--problema quitarTodos (e: T, s: seq⟨T⟩) : seq⟨T⟩ {
--requiere: { True }
--asegura: { resultado es igual a s pero sin el elemento e. }
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y:ys)    |x == y  = quitarTodos x ys
                        |otherwise = y:(quitarTodos  x ys)
--o

quitarTodos2 :: (Eq t ) => t -> [t] -> [t]
quitarTodos2 _ [] = []
quitarTodos2 x (y:xs)
    | x == y && noPertenece = xs
    | x == y = quitarTodos2 x xs
    | otherwise = y : quitarTodos2 x xs
    where noPertenece = not (x `pertenece` (y:xs))

--------------------------------------------------------------------------------









































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