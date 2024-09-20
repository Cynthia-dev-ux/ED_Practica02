agregarElemento :: [a] -> a -> Bool -> [a]
agregarElemento (x:xs) a True = a:(x:xs)
agregarElemento (x:xs) a False = (x:xs) ++[a]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = 0
maximoLista (x:xs)=if x > maximoLista(xs)
        then x
        else maximoLista(xs)



indice :: [a] -> Int -> a
indice [] index = error "no puedes ingresa una lista vacía"
indice (x:xs) index = if index == 0
    then x
    else indice xs (index -1)
    
numerosPares :: [Int] -> [Int]
numerosPares xs =  [x| x<- xs, mod x 2 == 0]