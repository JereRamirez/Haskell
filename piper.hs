Data Usuario = Usuario{
nombre :: Nombre,
seguidos :: [Nombre]
}

type Nombre = String
type Texto = String

data Mensaje = Mensaje {
usuario :: Nombre,
Texto :: Texto,
Repips :: Cantidad,
Favs :: Cantidad }

type Cantidad = Int

usuarios = [ Usuario "@marsupialRengo" ["@don_churrasco", "@dodainOk"], Usuario
"@don_churrasco" [], Usuario "@lapipi" ["@dodainOk"], Usuario "@dodainOk" ["@lapipi",
"@marsupialRengo"] ]

mensajes = [ Mensaje "@dodainOk" "Las personas que viajan en tren se quejan de llenos" 20
100, Mensaje "@lapipi" "En mi mundo todos son un pony" 1 0, Mensaje "@lapipi" "y comen
arcoiris y su popó son mariposas" 0 0, Mensaje "@dodainOk" "No hay problema en cometer
errores, el secreto es no pasarlos a producción" 1 3, ... ]

find f = head . filter f  -- equivale a : find f xs = head . filter f xs O find f xs = (head . filter f) xs O find f xs = (head . filter f) $ xs "Ver mónadas"

--1                                                       (\ u -> nombre u == seguidor)
sigueA seguidor seguido = elem seguido . seguidos . find ((==seguidor) . nombre) $ usuarios

--2
seguidores :: Nombre -> [Nombre]
seguidores seguido = map nombre . filter (flip sigueA seguido . nombre) $ usuarios

--3
trencitoDeUsuarios (u1:u2:us) = sigueA u1 u2 && trencitoDeUsuarios (u2:us)
trencitoDeUsuarios [_] = True
-- ver de hacerlo sin recursividad para el desafio

-- 4a
favear mensaje = mensaje { favs = favs mensaje +1 }

--4b
repipear men usu = Mensaje usu ( usuario men ++ ": " ++ texto men ) 0 0

--4c
repipearTodos :: [Mensaje] -> N -> [Mensaje]
repipearTodos mens usu = map (flip repipear usu) ms ++ map (\m -> m { repips = repips m +1}) ms

--5 hacer nosotros

--6.1
losMasAlgo f  = take 3 . reverse . quickSortBy f  -- losMasAlgo f lista = take 3 . reverse . quickSortBy f lista

losMasRepipieados pips = losMasAlgo repips pips

losMasGrandesPips texto = losMasAlgo (lenght . texto)

losMasPipeadores = losMasAlgo (length . mensajesPipeados)
        where mensajesPipeados u = filter ((== u) . usuario) mensajes

-- 7 ver de definir los tipos
