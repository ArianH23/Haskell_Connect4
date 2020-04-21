import System.Random

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result


createBoard :: Int -> Int -> [[[Char]]]
-- Funcion que devuelve un tablero de n filas y m columnas
createBoard n m = splitEvery m (take (n * m) rows)

rows :: [[Char]]
-- Funcion recursiva que sirve para crear el tablero
rows = ['_'] : rows


splitEvery :: Int -> [[Char]] -> [[[Char]]]
-- Funcion que divide una lista en sublistas cada n elementos
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

escogerEstrategia :: IO Int
-- Funcion que pide al usuario contra que bot desea jugar
escogerEstrategia = do
    putStrLn "Que estrategia quieres que el bot tenga?: 1.Random 2.Greedy 3.Smart"

    e <- getLine
    let num = (read e :: Int)
    if (num <= 3) && (num >= 1) then
        return (num)
    else
        do
            putStrLn "Estrategia no valida, vuelve a escoger por favor.\n"
            escogerEstrategia



muestraBoard' :: [[Char]] -> IO ()
-- Funcion que ayuda a "muestraBoard" a mostrar el tablero por la pantalla
muestraBoard' [] = return()
muestraBoard' (b:bs) = do
    putStr $ id (b ++ "|")
    muestraBoard' bs

muestraBoard :: [[[Char]]] -> IO ()
--Muestra en la pantalla el tablero recibido
muestraBoard [] = return()
muestraBoard (b:bs) = do 
    muestraBoard bs
    putStr $ id ("|")
    muestraBoard' b
    putStrLn ""

primerTurno :: IO Bool
-- Funcion que pide al usuario quien quiere que juegue primero, él, o el bot.
primerTurno = do
    putStrLn "\nQuieres tener el primer movimiento (escribe 1) o el segundo (escribe 2)?"
    putStrLn "Recuerda que el bot siempre sera las X y tu los O?"

    t <- getLine
    
    let num = (read t :: Int)

    if (num /= 1) && (num /= 2) then
        do
        putStrLn "Opción no valida, vuelve a escoger por favor."
        primerTurno
    else
        if num == 1 then
            return (True)
        else
            return (False)

escogerDimensiones :: IO (Int, Int)
--Funcion que pide al usuario las dimensiones del tablero en el que desea jugar
escogerDimensiones = do
    putStrLn "Elige el numero de filas del tablero"
    n <- getLine
    putStrLn "Elige el numero de columnas del tablero"
    m <- getLine
    let x = (read n :: Int)
    let y = (read m :: Int)

    return((x,y))

main :: IO ()
main = do 
    (n,m) <- escogerDimensiones
    let b = createBoard n m
    putStrLn ""
    muestraBoard b
    e <- escogerEstrategia
    t <- primerTurno

    if e == 1 then
        partida b t estrategia1
    else if e == 2 then
        partida b t estrategia2
    else
        partida b t estrategia3

ponerFicha :: Bool -> [[[Char]]] -> Int -> [[[Char]]]
-- Funcion que tras recibir un tablero, jugador, y columna, 
-- devuelve un tablero tras haber colocado en el anterior la ficha del jugador en su respectiva columna
ponerFicha isPlayer (f:fs) columnaE
    | f !! (columnaE) == "_" = (replaceB  f columnaE isPlayer) : fs
    | f !! (columnaE) == "X" || f !! (columnaE) == "O" = f : (ponerFicha isPlayer fs columnaE )
    | otherwise = [f]


replaceB :: [[Char]] -> Int -> Bool -> [[Char]]
-- Funcion que se encarga de substituir la posicion columnaE de la lista (f:fs) por la ficha del jugador indicado
replaceB (f:fs)  columnaE isPlayer
    | columnaE == 0 = 
                if isPlayer then
                    "O" : fs
                else
                    "X" : fs
    | otherwise = f : replaceB fs (columnaE - 1) isPlayer

jugadaRealizada :: Bool -> Int -> IO ()
-- Muestra en pantalla el tablero tras la ultima jugada que se ha realizado y apunta hacia el.
-- E indica cual ha sido ese movimiento y por quien.
jugadaRealizada isPlayer pos
    |isPlayer = putStrLn ("Has puesto ficha en la posición " ++ (show (pos + 1)) ++ " ↖\n")
    |otherwise = putStrLn ("El bot ha puesto ficha en la posición " ++ (show (pos + 1)) ++ " ↖\n")

partida:: [[[Char]]] -> Bool -> ([[[Char]]] -> Bool -> IO Int) -> IO ()
-- Funcion que se encargara de actualizar los turnos mientras la partida continue interactuando con el usuario
partida board t estrategia = do
    posicionNuevaFicha <- estrategia board t
    let nboard = ponerFicha t board posicionNuevaFicha 
    muestraBoard nboard
    putStrLn ""

    jugadaRealizada t posicionNuevaFicha

    if comprobarWin t nboard then
        do
        ganador t
        return()
    else
        do
        if empate nboard then
            do
            putStrLn "Habeis empatado!"
            return()
        else
            do
            partida nboard (not t) estrategia
            return()

empate :: [[[Char]]] -> Bool
-- Indica si el tablero esta completo, y por lo tanto, si ha habido un empate
empate board = not (foldr (||) False (map (any ("_"==)) board))

ganador :: Bool -> IO ()
-- Muestra en pantalla el texto del correspondiente ganador de la partida si ha habido ganador
ganador t = do
    if t then
        putStrLn "Has ganado tú!"
    else
        putStrLn "Ha ganado el bot!"

turnoJugador :: [[[Char]]] -> IO Int
--Funcion que se encarga de pedir al jugador el numero de columna en el que poner la ficha, entre 1 y N.
turnoJugador board = do
    let columnas = (length (head board))
    putStrLn ("Elige donde quieres poner la ficha entre el 1 y el " ++ show columnas ++ "\n")
    columnaE <- getLine
    -- print (head b)
    let validPos = map (+1) (validPositions board)

    if not (any ((read columnaE :: Int)==) validPos) then
        do
        putStrLn("Posición inválida\n")
        turnoJugador board
    
    else
        return ((read columnaE :: Int) -1)

estrategia1 :: [[[Char]]] -> Bool -> IO Int
-- Funcion de la estrategia1 de movimientos aleatorios
estrategia1 board isPlayer = do
    if isPlayer then
        do
        x <- turnoJugador board
        return (x)
    else
        do
        let validPos = validPositions board
        posrandom <- (randInt 0 ((length validPos) - 1))
        let posicionNuevaFicha = validPos !! posrandom
        -- putStrLn ("El bot ha colocado ficha en la posicion " ++ (show (posicionNuevaFicha+1) )++ "\n")
        -- let nboard = ponerFicha board posicionNuevaFicha t
        -- muestraBoard nboard
        return(posrandom)


estrategia3 :: [[[Char]]] -> Bool -> IO Int
-- Funcion de la estrategia3 smart. Detalles en la documentación
estrategia3 board isPlayer = do 
        if isPlayer then
            do
            x <- turnoJugador board
            return (x)
        else
            do
            let validPos = validPositions board

            (mejoresPosiciones, maxGlobal) <- greedy board validPos


            --Si el bot puede ganar la partida, la ganara
            if maxGlobal == 4 then
                do  -- Comprueba si puedes ganar, en tal caso, se hace alguna de las posibles jugadas ganadoras
                    randWinningPos <- randInt 0 ((length mejoresPosiciones) - 1)
                    -- print mejoresPosiciones
                    -- print ( "Voy a ganar y devuelvo" ++show (mejoresPosiciones !! randPos))

                    return (validPos !! (mejoresPosiciones !! randWinningPos))
            
            else

                do  --Comprueba si el jugador puede ganar y bloquea esa jugada si es posible
                -- print 1
                let posiblesMovimientosJugador = map (ponerFicha True board ) (validPos) --- Boards posibles del jugador
                -- print (sol)
                -- print 2
                let jugadaGanadoraJugador = map (comprobarWin True) posiblesMovimientosJugador
                -- print sol2
                if any (True==) jugadaGanadoraJugador then
                    do 
                    let posNextMove = (posOfTrueArray 0 jugadaGanadoraJugador) !! 0
                    let movimientoBloqueador = validPos !! posNextMove
                    -- print ( "estoy en movimientoBloqueador y devuelvo" ++show movimientoBloqueador)

                    return (movimientoBloqueador)
                
                else
                    --Si hay alguna columna por la que pueda ganar el bot si el jugador tira alli justo antes, el bot no tirara alli
                    --Si hay alguna columna por la que el jugador pueda ganar si el bot tira alli, el bot evitara tirar alli

                    do
                    let espaciosDobles = posOfTrueArray 0 (columnas2Espacios board)
                    -- print espaciosDobles

                    let tableroSiJuegaBot = map (ponerFicha False board ) (espaciosDobles)
                    let tableroSiJuegaPlayer = map (ponerFicha True board ) (espaciosDobles)

                    let botPlayer = zipWith (ponerFicha True) tableroSiJuegaBot espaciosDobles
                    let playerBot = zipWith (ponerFicha False) tableroSiJuegaPlayer espaciosDobles

                    let evita1 = map (comprobarWin True) botPlayer
                    let evita2 = map (comprobarWin False) playerBot
                    
                    let posicionesaEvitar = zipWith (||) evita1 evita2
                    
                    let movimientosCorrectos = (posOfFalseArray 0 posicionesaEvitar)
                    -- && length movimientosCorrectos /= (length espaciosDobles -1)
                    if length movimientosCorrectos > 0 then
                        do
                            --------------------------------------------
                        let posiblesMovimientosCorrectos = map (espaciosDobles !!) movimientosCorrectos
                        
                        let horiJugador = map fst (posiblesHorizontales board posiblesMovimientosCorrectos True)
                        print (posiblesHorizontales board posiblesMovimientosCorrectos True)
                        -- print 4

                        let dondepuedejugaroponente = (map (ponerFicha True board) posiblesMovimientosCorrectos)

                        -- let finalizar = zipWith (ponerFicha True) dondepuedejugaroponente (map (validPositions) (map (transpose) dondepuedejugaroponente))

                        let puedeGanarsiHaceestemov = puedeGanarEnOtraJugada True dondepuedejugaroponente 


                        let consecHoriJugador = potencial3enRayaH True horiJugador posiblesMovimientosCorrectos
                        -- print consecHoriJugador
                        -- ---Poner if por aqui de any==true
                        --                     let movimientos3H = posOfTrueArray consecHoriJugador 0
                        --                     let boardsposible3H = map (posiblesMovimientosJugador !!) (movimientos3H)
    
                        -- print "hola1"
                        -- let deboBloquear = puedeGanarEnOtraJugada True boardsposible3H
                        -- print "hola2"
                        -- print movimientos3H
                        -- print boardsposible3H
                        
                        -- print deboBloquear
                        -- print (posOfTrueArray deboBloquear 0)
                        ----------------------------------------------------
                        if any (True==) puedeGanarsiHaceestemov then
                            do
                            print posiblesMovimientosCorrectos
                            -- print "hola3"
                            let a = posOfTrueArray 0 puedeGanarsiHaceestemov
                            -- print a
                            -- let b = movimientos3H !! (a!!0)
                            let c = posiblesMovimientosCorrectos !! (a !! 0)
                            -- print ( "estoy en 3 h y devuelvo" ++show c)
                            print a
                            -- print b
                            print c
                            return (c)

                        else
                            do
                            (soluciones, x) <- greedy board posiblesMovimientosCorrectos

                            let solucionesU = (elementosUnicos soluciones)

                            let movimientoValidoMasGreedy = posiblesMovimientosCorrectos !! (solucionesU !! (((length solucionesU) - 1) `div` 2))

                            return movimientoValidoMasGreedy

                    
                    else
                        --Comprueba si en 2 movimientos puede ganar el jugador si no intento bloquear nada
                        
                        do
                        let horiJugador = map fst (posiblesHorizontales board validPos True)
                        print (posiblesHorizontales board validPos True)

                        -- print 4
                        let consecHoriJugador = potencial3enRayaH True horiJugador validPos
                        -- print consecHoriJugador
    ---Poner if por aqui de any==true
                        let movimientos3H = posOfTrueArray 0 consecHoriJugador
                        let boardsposible3H = map (posiblesMovimientosJugador !!) (movimientos3H)
    
                        -- print "hola1"
                        let deboBloquear = puedeGanarEnOtraJugada True boardsposible3H
                        -- print "hola2"
                        -- print movimientos3H
                        -- print boardsposible3H
                        
                        -- print deboBloquear
                        -- print (posOfTrueArray deboBloquear 0)
                        if any (True==) deboBloquear then
                            do
                            -- print "hola3"
                            let a = posOfTrueArray 0 deboBloquear
                            -- print a
                            let b = movimientos3H !! (a !! 0)
                            let c = validPos !! b
                            -- print ( "estoy en 3 h y devuelvo" ++show c)
                            return (c)

                        else
                            --No quedan mas opciones
                            do
                            let mejoresPosicionesU = (elementosUnicos mejoresPosiciones)
                            -- print ( "estoy en predet h y devuelvo" ++show (validPos!!(mejoresPosicionesU !! (((length mejoresPosicionesU) - 1) `div` 2))))

                            return (validPos!!(mejoresPosicionesU !! (((length mejoresPosicionesU) - 1) `div` 2)))

columnasEspacios :: Int -> [[Char]] -> Int
-- Dada una columna, indica cuantos espacios libres hay en ella.
columnasEspacios num [] = num
columnasEspacios num (b:bs)
    |b == "_" = columnasEspacios (num + 1) bs
    |otherwise = num


columnas2Espacios :: [[[Char]]] -> [Bool]
-- Dado un board, devuelve una lista de booleanos que indica que columnas tienen 2 o mas espacios libres.
columnas2Espacios board = map (>= 2) (map (columnasEspacios 0) (map reverse (transpose board))) 

puedeGanarEnOtraJugada :: Bool -> [[[[Char]]]] -> [Bool]
-- Dado un jugador y una serie de tableros, indica en cual de estos tableros el jugador puede ganar poniendo una sola ficha
puedeGanarEnOtraJugada _ [] = []
puedeGanarEnOtraJugada isPlayer (b:bs)
    |isPlayer = 
        [foldl (||) (False) (map (comprobarWin True) (map (ponerFicha True b ) (validPositions  b)))] ++ puedeGanarEnOtraJugada isPlayer bs
    |otherwise = []

contarFichasConsecutivas :: Int -> Bool -> [[Char]] -> Int
--Funcion que indica la longitud de una raya empezando en una posicion concreta en adelante para el jugador dado.
contarFichasConsecutivas n _ [] = n 
contarFichasConsecutivas n isPlayer (f:fs)
    |isPlayer = 
        if f == "O"  then 
            contarFichasConsecutivas (n+1) isPlayer fs
        else n
    |not isPlayer = 
        if f == "X"  then 
            contarFichasConsecutivas (n+1) isPlayer fs
        else n
    |otherwise = n

potencial3enRayaH :: Bool -> [[[Char]]] -> [Int] -> [Bool]
-- Dada una lista de lineas, una lista de posiciones y un jugador. Recorriendo ambas listas simultaniamente, 
-- indica si el tamaño de la raya que forma el jugador en esa linea a partir de esa posicion es mayor o igual que 3
potencial3enRayaH _ [] _ = []
potencial3enRayaH isPlayer (f:fs) (p:ps) = (contarFichasConsecutivas 0 isPlayer (drop (p) f) + contarFichasConsecutivas 0 isPlayer (drop ((length f) - p) (reverse f)) >= 3)
                                        : potencial3enRayaH isPlayer fs ps

estrategia2 :: [[[Char]]] -> Bool -> IO Int
-- Funcion de la estrategia2 greedy. Detalles en la documentación
estrategia2 board isPlayer = do
        if isPlayer then
            do
            x <- turnoJugador board
            return (x)
        else
            do
        -- print (map reverse (transpose board))
        let validPos = validPositions board
        -- print validPos
        (mejoresPosiciones, maxGlobal) <- greedy board validPos
        -- print (maxGlobal)
        -- let (listaGreedyS, listaGreedyC)
        -- print (posOfFalse (zipWith (==) (diagonals board) (diagonals (ponerFicha board 0 False))) 0)
        -- print maxGlobal
        -- -- print xd
        -- print mejoresPosiciones
        -- print "hi"
        randPos <- randInt 0 ((length mejoresPosiciones) - 1)
        -- print validPos
        -- print mejoresPosiciones
        -- print (elementosUnicos mejoresPosiciones)
        let mejoresPosicionesU = (elementosUnicos mejoresPosiciones)
        let bestOfbests = map (validPos !!) mejoresPosicionesU
        let sol = map (ponerFicha True board ) (bestOfbests)
        -- print (sol)
        let sol2 = map (comprobarWin True) sol
        -- print sol2
        if any (True==) sol2 then
            do 
            let posNextMove = (posOfTrueArray 0 sol2) !! 0
            
            let nextMove = (bestOfbests !! posNextMove)
            -- let nboard = ponerFicha board nextMove t

            -- muestraBoard nboard

            -- putStrLn ("El bot ha colocado ficha en la posicion " ++ (show ((nextMove) + 1) ) ++ " :)\n")
            return (nextMove)
            -- if comprobarWin t nboard then
            --     do

        else
            do
            -- print posOfMax
            -- putStrLn ("El bot ha colocado ficha en la posicion " ++ (show ((mejoresPosiciones !! randPos) + 1) ) ++ "\n")
            -- putStrLn "venga dew"
            return (validPos !! (mejoresPosiciones !! randPos))


greedy :: [[[Char]]] -> [Int] -> IO ([Int], Int)
-- Funcion que dado un tablero y sus posiciones validas, devuelve
-- una serie de posiciones validas que pueden estar repetidas indicando
-- en que columnas se puede formar la raya mas grande para el bot,
-- (las repeticiones se deben a que pueden formarse rayas diagonales, horizontales
-- o verticales en el momento en el que se coloque la ficha en la posicion)
-- y cual es la longitud de esa raya que se va a formar.
greedy board validPos =
    do 
    -- let validPos = validPositions (transpose board)
        -- print validPos
    let posColumnas = (posiblesColumnas board validPos False)
    let consecVerticales =  (map (length) (map (takeWhile ("X"==)) posColumnas))
    let posHorizontales = map (fst) (posiblesHorizontales board validPos False)
    let consecHorizontales = consecutivosHorizontales False posHorizontales validPos

    let posDiagonales1 = posiblesDiagonales board validPos False
    let diag1 = map fst posDiagonales1
    let positions1 = map snd posDiagonales1
    let consecDiagonales1 = (consecutivosHorizontales False diag1 positions1)

    let posDiagonales2 = posiblesDiagonales board validPos True
    let diag2 = map fst posDiagonales2
    let positions2 = map snd posDiagonales2
    let consecDiagonales2 = (consecutivosHorizontales False diag2 positions2)

    -- print posDiagonales2s)
    -- print posHorizontales
    let maxGlobal = maximum (consecVerticales ++ consecHorizontales ++ consecDiagonales1 ++ consecDiagonales2)
    let consecutivosFiltrado = map (posicionValorEnLista maxGlobal 0) ([consecVerticales] ++ [consecHorizontales] ++ [consecDiagonales1] ++ [consecDiagonales2])
    let mejoresPosiciones = uneListas consecutivosFiltrado

    return ((mejoresPosiciones, maxGlobal))

comprobarWin :: Bool -> [[[Char]]] -> Bool
-- Funcion que dado un jugador y un tablero, confirma si el jugador ha ganado en ese tablero
comprobarWin isPlayer board = checkHorizontal board isPlayer || checkVertical board isPlayer || checkDiagonals board isPlayer

posDiferencia :: [[Char]] -> [[Char]] -> Int -> Int
-- Funcion que dada dos listas diferentes, te devuelve en que posicion son diferentes
posDiferencia (b1:b1s) (b2:b2s) pos
    |b1 /= b2 = pos
    |otherwise = posDiferencia b1s b2s (pos+1)
    
diagonalDiferente :: [[[Char]]] -> [[[Char]]] -> ([[Char]], Int)
-- Dados 2 boards diferentes, devuelve la diagonal diferente del segundo, y a que altura del tablero se encuentra esta posicion.
diagonalDiferente board1 board2 = ((diagonals board2) !! x, posDiferencia ((diagonals board1) !! x) ((diagonals board2) !! x) 0)
    where x = ((posOfFalseArray 0 (zipWith (==) (diagonals board1) (diagonals board2))) !! 0)

posiblesDiagonales :: [[[Char]]] -> [Int] -> Bool -> [([[Char]], Int)]
-- Dado un board, sus posiciones validas, y un booleano que indica si invertir o no el board,
-- devuelve una lista de parejas, primero de aquellas diagonales que se forman al poner una ficha en
-- el board dado, y en cada posicion valida, y segundo, la altura a la que esta la nueva ficha colocada.
-- Si invertida es True, entonces antes de hacer todo lo mencionado anteriormente, se girara el tablero 
-- para trabajar con las diagonales inversas.
posiblesDiagonales _ [] _ = []
posiblesDiagonales board (p:ps) invertida 
    |not invertida = (diagonalDiferente board (ponerFicha False board p)) : posiblesDiagonales board ps invertida
    |otherwise = (diagonalDiferente (map reverse board) (map reverse (ponerFicha False board p))) : posiblesDiagonales board ps invertida

posOfFalseArray :: Int -> [Bool] -> [Int]
-- Dada una lista y un numero que indica la posicion en esta lista, deuelve una nueva lista que contiene
-- las posiciones de aquellos valores a False en la lista dada inicialmente
posOfFalseArray pos [] = []
posOfFalseArray pos (a:as)
    |not a = [pos] ++ posOfFalseArray (pos + 1) as
    |otherwise = posOfFalseArray (pos + 1) as

posOfTrueArray :: Int -> [Bool] -> [Int]
-- Dada una lista y un numero que indica la posicion en esta lista, deuelve una nueva lista que contiene
-- las posiciones de aquellos valores a True en la lista dada inicialmente
posOfTrueArray _ [] = []
posOfTrueArray pos (a:as)
    |a = [pos] ++ posOfTrueArray (pos + 1) as
    |otherwise = posOfTrueArray (pos + 1) as 

-- posOfFalse :: [Bool] -> Int -> Int
-- posOfFalse [] pos = pos
-- posOfFalse (f:fs) pos
--     |not f = pos
--     |otherwise = posOfFalse fs (pos+1)

uneListas :: [[Int]] -> [Int]
-- Funcion que une un conjuento de listas separadas.
uneListas (x:[]) = x
uneListas (x:xs) = x ++ uneListas xs

elementosUnicos :: [Int] -> [Int]
-- Funcion que ordena una lista y devuelve los mismos elementos sin repeticiones.
elementosUnicos xs = eliminaRepeticiones $ sort xs
  where
    eliminaRepeticiones []  = []
    eliminaRepeticiones [x] = [x]
    eliminaRepeticiones (x1:x2:xs)
      | x1 == x2  = eliminaRepeticiones (x1:xs)
      | otherwise = x1 : eliminaRepeticiones (x2:xs)

consecutivosHorizontales :: Bool -> [[[Char]]] -> [Int] -> [Int]
consecutivosHorizontales _ [] _ = []
consecutivosHorizontales isPlayer (f:fs) (p:ps) = contarFichasConsecutivas 0 isPlayer (drop (p) f) + contarFichasConsecutivas 0 isPlayer (drop ((length f) - p) (reverse f))
                                        : consecutivosHorizontales isPlayer fs ps

posiblesColumnas :: [[[Char]]] -> [Int] -> Bool -> [[[Char]]]
posiblesColumnas _ [] _ = []
posiblesColumnas board (p:ps) isPlayer = (dropWhile ("_"==) ((map reverse (transpose (ponerFicha isPlayer board p ))) !! p)) : posiblesColumnas board ps isPlayer

-- posiblesHorizontales :: [[[Char]]] -> [Int] -> Bool -> [[[Char]]]
-- Dado un board, sus posiciones validas, y un jugador, 
posiblesHorizontales     _ [] _ = []
posiblesHorizontales board (p:ps) isPlayer = (cogerUltimaHorizontalConValor p 0 (ponerFicha isPlayer board p )) : posiblesHorizontales board ps isPlayer

cogerUltimaHorizontalConValor :: Int -> Int -> [[[Char]]] -> ([[Char]], Int)
-- Funcion que dada una posicion y un tablero, devuelve una pareja: la horizontal no vacia que este mas arriba en el
-- tablero en la posicion dada, y a que altura está.
cogerUltimaHorizontalConValor _ hori (f:[]) = (f, hori)
cogerUltimaHorizontalConValor pos hori (f:fs) 
    |(head fs) !! pos == "_" = (f, hori)
    |otherwise = cogerUltimaHorizontalConValor pos (hori+1) fs

posicionValorEnLista :: Int -> Int -> [Int] -> [Int]
-- Dado una lista y un valor, devuelve una nueva lista indicando en que posiciones de la lista anterior
-- se encuentra dicho valor.
posicionValorEnLista _  _ []  = []
posicionValorEnLista valor pos (a:as)
    |a == valor = [pos] ++ posicionValorEnLista valor (pos+1) as
    |otherwise = posicionValorEnLista valor (pos + 1) as

validPositions :: [[[Char]]] -> [Int]
-- Funcion que devuelve la posicion de aquellas columnas en las que se puede poner una ficha dado un cierto board
validPositions board = validPositions' (transpose board)

validPositions' :: [[[Char]]] -> [Int]
-- Funcion auxiliar de validPositions que ayuda a conseguir las posiciones validas
validPositions' board = posOfTrueArray 0 (map (any ("_"==)) board) 





diagonals :: [[[Char]]] -> [[[Char]]]
--Funcion que dada una lista de listas rectangular, devuelve sus diagonales como lista de listas
diagonals []       = []
diagonals ([]:xs) = xs
diagonals x      = zipWith (++) (map ((:[]) . head) x ++ repeat [])
                                    ([]:(diagonals (map tail x)))


transpose :: [[[Char]]] -> [[[Char]]]
--Funcion que dada una lista de listas rectangular, devuelve su transpuesta
transpose ([]:_) = []
transpose b = (map head b) : transpose (map tail b)

checkDiagonals :: [[[Char]]] -> Bool -> Bool
-- Funcion que dado un tablero y un jugador, confirma si el jugador ha ganado por alguna de las diagonales del tablero.
checkDiagonals board isPlayer = checkHorizontal (diagonals board)  isPlayer || checkHorizontal (diagonals (map reverse board)) isPlayer

checkVertical :: [[[Char]]] -> Bool -> Bool
-- Funcion que dado un tablero y un jugador, confirma si el jugador ha ganado por alguna vertical del tablero.
checkVertical board isPlayer = checkHorizontal (transpose board) isPlayer

checkHorizontal :: [[[Char]]] -> Bool -> Bool
-- Funcion que dado un tablero y un jugador, confirma si el jugador ha ganado por alguna horizontal del tablero.
checkHorizontal board isPlayer = foldl (||) (False) (map (confirmWin isPlayer 0) board)

confirmWin :: Bool -> Int -> [[Char]] -> Bool
-- Funcion que dada una lista, y un jugador, indica si el jugador tiene 4 fichas consecutivas en la lista.
confirmWin isPlayer 4 _ = True
confirmWin isPlayer fichasSeguidas [] = False
confirmWin isPlayer fichasSeguidas (f:fs) 
    |f == "_" = confirmWin isPlayer 0 fs
    |isPlayer == True = 
        if f == "O" then
            confirmWin isPlayer (fichasSeguidas+1) fs
        else
            confirmWin isPlayer 0 fs
            
    |(not isPlayer) = 
        if f == "X" then    
            confirmWin isPlayer (fichasSeguidas+1) fs
        else
            confirmWin isPlayer 0 fs


sort :: [Int] -> [Int]
-- Funcion que ordena una lista de enteros de menor a mayor.
sort [] = []
sort [x] = [x]
sort (x:xs) = (sort menors) ++ [x] ++ (sort majors)
    where 
        menors = [y | y<-xs , y < x]
        majors = [y | y<-xs , y >= x]