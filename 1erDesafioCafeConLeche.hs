primo = primo' [2..100]

esMultiploDe n = (==0) . (`mod` n)

primo' (x:xs) = x:primos' (filter (not . esMultiploDe x) xs)


































































-- {-
-- Dada la función constante primos con esta definición:
--
-- primos = primos' [2..]
--
-- Implementar la función primos' para que primos sea la lista infinita de números primos.
--
-- REGLAS:
-- Se puede usar la función auxiliar esMultiploDe:
-- esMultiploDe n = (==0) . (`mod` n)
-- NO se puede hacer una función esPrimo, ni ninguna equivalente.
-- La solución de primos' debe estar implementada en UNA ÚNICA LÍNEA DE CÓDIGO,
-- sin otras funciones auxiliares que la dada y las funciones estándares
-- (las que vienen en el Prelude... están en la guía de lenguajes, si quieren una
-- -}
--
-- --esMultiploDe n = (==0) . (`mod` n)
--
-- -- filter' :: [a] -> (a -> Bool) -> [a]
-- -- filter' _ [] = []
-- -- filter' f(x:xs)
-- --       | f x = x : filter' f xs
-- --       | otherwise = filter' f xs
--
-- --primos = primos' [2..]
-- --primos' :: [Int] -> [Int]
-- {-
-- primos' (x:xs) = filter' ( (x esMultiploDe 1 && (filter' ( (x esMultiploDe) [2..x] ) == [x]) ) (x:xs) )
--
-- primos' (x:xs) = filter' ( ( (esMultiploDe 1 x) && (filter' ( (esMultiploDe x) [2..x] ) == [x]) ) (x:xs) )
--
-- primos' (x:xs) = filter' ( ( ( == 1 ) . length . esMultiploDe x [2.. x]) (x:xs) )
-- -}
-- --primos' (x:xs) = filter ( ( ( == 1 ) . length . filter ( (esMultiploDe x) [2..x] ) ) (x:xs) )
--
-- -- filter :: [a] -> (a -> Bool) -> [a]
-- -- filter (x:xs) condicion
-- --       | condicion x = x : (filter (xs condicion))
-- --       | otherwise = (filter (xs condicion))
-- -- filter [] _ = []
--
-- --filter :: (a -> Bool) -> [a] -> [a]
--
-- --primos' (x:xs) = filter ( esMultiploDe [2..x]) (x:xs)
--
-- -- filter(esMultiploDe [2..x] . head (x:xs)) (x:xs)
--
--
--
-- --main :: IO ()    -- This says that main is an IO action.
-- --main = return ()
--
-- --esMultiploDe n = (==0) . (`mod` n)
--
-- primos :: [Int]
-- primos = primos' [2..]
--
-- primos' :: [Int] -> [Int]
-- --primos' (x:xs) = [2,3,5,7,11,13] ++ filter (not . esMultiploDe 13) (filter (not . esMultiploDe 11) (filter (not . esMultiploDe 7) (filter (not . esMultiploDe 5) (filter (not . esMultiploDe 3) (filter (not . esMultiploDe 2) (x:xs))))))
-- primos' (x:xs) = filter((== 0) . (mod x)) (filter ([y|y <- [2..x]]y<=) (x:xs))
