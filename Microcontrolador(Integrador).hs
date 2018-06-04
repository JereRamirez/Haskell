data Microcontrolador = Microcontrolador {  
         memoria :: [Int],  
         acumuladorA :: Int,  
         acumuladorB :: Int,  
         programCounter :: Int,  
         mensajeDeError :: String, 
         programa :: [Instruccion] 
 } deriving (Show) 

   
 xt8088 = Microcontrolador { 
     acumuladorA = 0, 
     acumuladorB = 0, 
     programCounter = 0, 
     mensajeDeError = "", 
     memoria = replicate 1024 0, 
     programa = [] 
 } 
 
 
 fp20 = Microcontrolador { 
     acumuladorA = 7, 
     acumuladorB = 24, 
     programCounter = 0, 
     mensajeDeError = "", 
     memoria = [], 
     programa = [] 
 }   
 
 
 at8086 = Microcontrolador { 
     acumuladorA = 0, 
     acumuladorB = 0, 
     programCounter = 0, 
     mensajeDeError = "", 
     memoria = [1..20], 
     programa = [] 
 }   
 
 
 
 
 type Instruccion = Microcontrolador -> Microcontrolador 
 
 
 nop :: Instruccion 
 nop microcontrolador = microcontrolador { 
     programCounter = programCounter microcontrolador + 1 
 } 
 
 
 incrementarPC = nop 
 

 add :: Instruccion 
 add microcontrolador = microcontrolador { 
     acumuladorA = acumuladorA microcontrolador + acumuladorB microcontrolador , 
     acumuladorB = 0 
 } 
 
 

 swap :: Instruccion 
 swap microcontrolador = microcontrolador { 
     acumuladorA = acumuladorB microcontrolador, 
     acumuladorB = acumuladorA microcontrolador                                
 } 
 
 
 

 lodv :: Int ->Instruccion 
 lodv val microcontrolador = microcontrolador { 
                                     acumuladorA = val 
                                 } 
 
 

 divide :: Instruccion 
 divide microcontrolador 
     |acumuladorB microcontrolador == 0 = microcontrolador { 
                         mensajeDeError = "DIVISION BY ZERO" 
                         } 
     |otherwise = microcontrolador { 
                             acumuladorA =  div (acumuladorA microcontrolador) (acumuladorB microcontrolador), 
                             acumuladorB = 0  
                         } 
 
 
 lod :: Int -> Instruccion 
 lod addr microcontrolador = microcontrolador { 
     acumuladorA = (memoria microcontrolador) !! (addr-1) 
 } 
 
 
 str :: Int -> Int -> Instruccion 
 str addr val microcontrolador = microcontrolador { 
     memoria = crearNuevaLista addr val (memoria microcontrolador) 1} 
 
 
 crearNuevaLista _ val [] _ = [val] 
 crearNuevaLista addr val (x:xs) posc  
     |addr==posc = val : xs 
     |otherwise = x : crearNuevaLista addr val xs (posc+1) 
 
 

 cargar unPrograma microcontrolador = microcontrolador{  
          programa = unPrograma 
 } 
 
 
 programa1=[(lodv 10), swap, (lodv 22), add] 
 programa2=[(str 1 2),(str 2 0), (lod 2), swap, (lod 1), divide] 
 

 ejecutar microcontrolador = foldl ejecutarInstruccion microcontrolador (programa microcontrolador)  
 ejecutarInstruccion microcontrolador f 
    |(not.programaVacio $ microcontrolador) && (sinError microcontrolador) = f microcontrolador{ 
                                             programCounter = (+1).programCounter $ microcontrolador 
                                            } 
    | otherwise = microcontrolador 
 programaVacio = (==0).length.programa 
 sinError = (=="").mensajeDeError 
 
 

 infz instrucciones microcontrolador  
         |not.estaVacio.acumuladorA $ microcontrolador = ejecutar.cargar instrucciones $ microcontrolador 
         |otherwise = microcontrolador 
 
 
 estaVacio 0 = True  
 estaVacio _ = False 
 
 
 depurar programa microcontrolador = filter (not.esInnecesaria microcontrolador) programa 
 esInnecesaria microcontrolador instruccion = acumuladoresYMemoriaVacia.ejecutar.cargar [instruccion] $ microcontrolador  
 acumuladoresYMemoriaVacia microcontrolador = (estaVacio.acumuladorA $ microcontrolador) && (estaVacio.acumuladorB $ microcontrolador) && (estaVacia.memoria $ microcontrolador) 
 
 
 estaVacia lista = all (==0) lista 
 
 

 estaOrdenadaMenorAMayor = ordenada.memoria 
 ordenada [] = True 
 ordenada [_] = True 
 ordenada (x1:x2:xs) = (x1<=x2) && ordenada (x2:xs) 
 
 
 microDesorden = Microcontrolador{ 
         acumuladorB = 0, 
         acumuladorA = 0, 
         programa = [], 
         programCounter = 0, 
         mensajeDeError = "", 
         memoria = [2,5,1,0,6,9] 
 } 
 

 infinito = Microcontrolador { 
     acumuladorA = 0, 
     acumuladorB = 0, 
     programCounter = 0, 
     mensajeDeError = "", 
     memoria = repeat 0, 
     programa = [] 
 } 
