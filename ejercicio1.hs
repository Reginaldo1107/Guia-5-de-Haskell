--Somos lo que somos 
--1.1)Longitud :: [t] -> t , segun la siguiente 
--especificacion :
longitud :: [t] -> Integer
longitud [] = 0 
longitud (x:xs) = 1 + longitud(xs)    
--o 
longitud2 :: [t] -> Integer
longitud2 (x:[]) = 1 
longitud2 (x:xs) =1 + longitud2(xs)    

--2. ultimo :: [t] -> t segun la siguiente 
--especificacion:
--Devuelveme el ultimo elemento de la secuencia
ultimo :: [t] -> t
ultimo (x:[]) = x 
ultimo (x:xs) = ultimo (xs) 

--3. principio :: [t] -> [t]
-- segun la siguiente especificacion:
-- resultado = subseq(s, 0, |s| - 1) 
--subseq(s, 0, |s| - 1) = ME DEVUELVE UNA SECUNCIA SACANDOME UN ELEMNTO DE INDICE |S|-1 
--EMPEZANDO POR 0 , RECORDAR QUE LA SECUENCIA EMPIEZA POR 0 ,1,2,3,4
--ejemplo "HOLA" -> "HOL"
principio :: [t] -> [t]
principio (x:[]) = []
principio (x:xs) = x:principio(xs) 
-- O
principio2 :: [t] -> [t]
principio2 [x] = []
principio2 (x:xs) = x : principio2 xs

--reverso :: [t] -> [t] segun la siguiente especificacion:

--problema reverso (s: seq⟨T⟩) : seq⟨T⟩
-- resultado tiene los mismos elementos que s 
--pero en orden inverso.
--ejemplo : "HOLA" --> "ALOH"
reverso :: [t] -> [t]
reverso (x:[]) =[x]
reverso  (x:xs) = reverso(xs) ++ [x]



