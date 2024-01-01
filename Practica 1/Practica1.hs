{-
P R A C T I C A  1
Integrantes:
- DJLP
- ADLG
- JGJMP
-}

module Practica1 where

import Data.List

{-
2 pts. Define la función groupAnagrams tal que recibe una lista de String y devuelve una lista con los anagramas
agrupados. Un anagrama es una palabra o frase formada al reorganizar las letras de otra palabra o frase, utilizando
todas las letras originales exactamente una vez.
-}

groupAnagrams :: [String] -> [[String]]
groupAnagrams [] = []
groupAnagrams (x:xs) = [[x] ++ divide x xs] ++ groupAnagrams(quita (divide x xs) xs)

esAnagrama :: String -> String -> Bool
esAnagrama x y = if sort x == sort y then True else False

divide :: String -> [String] -> [String]
divide [] _ = []
divide x [] = []
divide w (x:xs) | esAnagrama w x = x : divide w xs
 | otherwise = divide w xs

elimina :: Eq a => a -> [a] -> [a]
elimina _ [] = []
elimina x (y:ys) | x == y = elimina x ys
 | otherwise = y : elimina x ys

quita :: [String] -> [String] -> [String]
quita [] [] = []
quita [] y = y
quita x [] = []
quita (x:xs) (y:ys) = quita xs (elimina x (y:ys))

anagrama1 = [ " eat " ," tea " ," tan " ," ate " ," nat " ," bat " ]
anagrama2 = [ " hello " ," " ," world " ," wldro " ," hlloe " ," a " ," aa " ]


{-
2 pts. Define la función subsets tal que recibe una lista de elementos únicos y devuelve el conjunto potencia
-}

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

subset1 = [1 ,2 ,3]
subset2 = [ 'a' , 'b' , 'c' , 'd']


{-
2 pts. El elemento mayoritario es el elemento que aparece más de ⌊n/2⌋ veces, donde n es la longitud de la lista. Define la
función majorityElem tal que recibe una lista y devuelve su elemento mayoritario.
La solución debe ser de complejidad O(n) en tiempo y O(1) en el espacio
-}

majorityElem :: Eq a => [a] -> a
majorityElem xs = majorityAux xs (head xs) 1

majorityAux :: Eq a => [a] -> a -> Int -> a
majorityAux [] opcion _ = opcion
majorityAux (x:xs) opcion contador
    | contador == 0 = majorityAux xs x 1
    | x == opcion = majorityAux xs opcion (contador + 1)
    | otherwise = majorityAux xs opcion (contador - 1)

majority1 = [3 ,2 ,3]
majority2 = [2 ,2 ,1 ,1 ,1 ,2 ,2]


{-
2 pts.  Define la función coins tal que recibe una lista de monedas de diferentes denominaciones y una cantidad total de
dinero, y devuelve si es posible completar la cantidad usando únicamente ese tipo de monedas.
-}

coins :: [Int] -> Int -> Bool
coins [] 0 = True
coins [] _ = False
coins (x:xs) total
  | total < 0 = False
  | total == 0 = True
  | otherwise = coins xs total || coins (x:xs) (total - x)


{-
1 pto. Define la función isBST tal que recibe un árbol binario y devuelve si es un árbol de búsqueda binario válido. Un
BST válido se define de la siguiente manera:
(a) El subárbol izquierdo contiene solo valores menores que la raíz.
(b) El subárbol derecho contiene solo valores mayores que la raíz.
(c) Ambos subárboles deben ser árboles de búsqueda binarios.
-}

data BST a = Empty | Node a (BST a) (BST a) deriving Show

isBST :: BST Int -> Bool
isBST arbol = isBSTAux arbol Nothing Nothing

isBSTAux :: Ord a => BST a -> Maybe a -> Maybe a -> Bool
isBSTAux Empty _ _ = True
isBSTAux (Node valor izq der) valorMin valorMax =
    case (valorMin, valorMax) of
        (Just valMin, Just valMax) -> valMin < valor && valor < valMax && isBSTAux izq valorMin (Just valor) && isBSTAux der (Just valor) valorMax
        (Nothing, Just valMax) -> valor < valMax && isBSTAux izq Nothing (Just valor) && isBSTAux der (Just valor) valorMax
        (Just valMin, Nothing) -> valMin < valor && isBSTAux izq valorMin (Just valor) && isBSTAux der (Just valor) Nothing
        (Nothing, Nothing) -> isBSTAux izq Nothing (Just valor) && isBSTAux der (Just valor) Nothing


{-
1 pto. Define la función kthElem tal que recibe un árbol de búsqueda binaria y un número entero k, y devuelve el k-ésimo
valor más pequeño.
-}

kthElem :: BST a -> Int -> a
kthElem arbol k
    | k <= 0 = error "Valor de k inválido"
    | otherwise = kthElemAux arbol k

kthElemAux :: BST a -> Int -> a
kthElemAux Empty _ = error "Árbol vacío"
kthElemAux (Node valor izq der) k = 
    let ladoIzq = tamano izq
    in if k <= ladoIzq
       then kthElemAux izq k
       else if k == ladoIzq + 1
            then valor
            else kthElemAux der (k - ladoIzq - 1)

tamano :: BST a -> Int
tamano Empty = 0
tamano (Node _ izq der) = 1 + tamano izq + tamano der

