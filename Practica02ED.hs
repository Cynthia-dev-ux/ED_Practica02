longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

sumaLista :: Num a => [a] -> a
sumaLista [a] = a
sumaLista (x:xs) = x + sumaLista(xs)

agregarElemento :: [a] -> a -> Bool -> [a]
agregarElemento [] a bool = 
         if bool
        then  a:[]
        else []++[a]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = 0
maximoLista (x:xs)=if x > maximoLista(xs)
        then x
        else maximoLista(xs)



indice :: [a] -> Int -> a
indice [] index = error "no puedes ingresa una lista vacía"
indice (x:xs) index =  if  index < 0 || index > longitud(x:xs)-1
        then error "indice no válido"
        else  if index == 0
    then x
    else indice xs (index -1)



divisores :: Int -> [Int]
divisores a = [x|x <- [1..a] , mod a x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x:conjunto[a|a<-xs,x/=a] 

numerosPares :: [Int] -> [Int]
numerosPares xs =  [x| x<- xs, mod x 2 == 0]


