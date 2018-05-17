data Compra = Compra {
      usuario :: String,
      productos :: Productos
}

data Producto = Producto {
      nombre :: String,
      categoria :: String,
      precio :: Float
}

type Productos = [Producto]

{-
1)COMPRAS DE DETERMINADO usuario
2) COMPRAS QUE SUPERAN X MONTO
3) COMPRAS QUE TIENEN # Producto
-}

--1)
comprasDe :: String -> [Compra] -> [Compra]
comprasDe nombre []=[]
comprasDe nombre (compra:cs)
      | usuario compra == nombre = compra : comprasDe nombre cs
      | otherwise = comprasDe nombre cs

--2)

-- comprasSuperiorA compra monto
comprasSuperioresA :: [Compra] -> Int -> [Compra]
comprasSuperioresA [] _ = []
comprasSuperioresA (compra:cs) monto
      | precioDeCompra compra >= monto = compra:comprasSuperioresA (cs monto)
      | otherwise = comprasSuperioresA (cs monto)

--3)
comprasConNProductos :: [Compra] -> Int -> [Compra]
comprasConNProductos _ [] = []
comprasConNProductos n (compra:cs)
      | (length . productos) compra == n = [compra] ++ comprasConNProductos n cs -- ó compra :: comprasConNProductos n cs
      | otherwise = comprasConNProductos n cs

-- Para optimizar todas las condiciones que se evaluan en las funciones anteriores,
-- podemos crear una funcion filtro que se encargue de recibir una lista y realizarles el filtro solicitado, devuelve la lista filtrada
filter :: [a] -> (a -> Bool) -> [a]
filter (x:xs) condicion
      | condicion x = x : filter xs condicion
      | otherwise = filter xs condicion
filter [] _ = []

filter' :: [a] -> (a -> Bool) -> [a]
filter' _ [] = []
filter' f(x:xs)
      | f x = x : filter' f xs
      | otherwise = filter f xs

comprasDe' nombre compras = filter' ((==nombre) . usuario) compras -- Al estar de ambos lados, puedo simplificar el compras -> comprasDe' nombre = filter' ((==nombre).usuario)

comprasSuperioresA' monto = filter' ((>= monto) . precioDeCompra)

comprasConNProductos' _ [] = []
comprasConNProductos' n (compra:cs) = filter' ((==n) . length . productos)

-- filter filtra, map transforma
map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = fx : map f xs

precioDeCompra :: [Compras] -> Float
precioDeCompra compra = --sum (map precio (productos compra))
                        sum map precio . productos $ compra
precioDeCompra' = sum map precio . productos

{-
composición de funciones
(f . g) x = f(g x)
(.) f g x ) f(g x)
(.) :: (a->b) -> (c->a) -> c -> b

map filter all any <- Funciones más comunes de orden superior

parcial "Lambdaprop"

type Requisito ) Depto -> Bool
type Busqueda = [Requisito]
data Persona = Persona { mail :: String, busquedas :: [Busqueda]}
data Depto = Depto {
    ambientes :: Int,
    superficie :: Int,
    precio :: Int,
    barrio :: String
}

ordenarSegun _ [] = []
ordenar Segun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x )) xs

-}
