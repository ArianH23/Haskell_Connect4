import System.Random
-- ******************
-- Antes de empezar a leer la documentación del código, cabe decir que hay una distincion entre jugador y usuario:
-- Usuario es aquel humano que está interactuando con el programa
-- El termino "Jugador" hace referencia tanto al bot como al usuario.
-- En el programa suele ser representado como un booleano isPlayer que es True si es el usuario, y False si se trata del bot.

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result


createBoard :: Int -> Int -> [[[Char]]]
-- Función que devuelve un tablero de n filas y m columnas
createBoard n m = splitEvery m (take (n * m) rows)

rows :: [[Char]]
-- Función recursiva que sirve para crear el tablero
rows = ['_'] : rows

splitEvery :: Int -> [[Char]] -> [[[Char]]]
-- Función que divide una lista en sublistas cada n elementos
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

escogerEstrategia :: IO Int
-- Función que pide al usuario contra que bot desea jugar
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
-- Función que ayuda a "muestraBoard" a mostrar el tablero por la pantalla
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
-- Función que pide al usuario quien quiere que juegue primero, él, o el bot.
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
-- Función que pide al usuario las dimensiones del tablero en el que desea jugar
escogerDimensiones = do
    putStrLn "Elige el numero de filas del tablero"
    n <- getLine
    putStrLn "Elige el numero de columnas del tablero"
    m <- getLine
    let x = (read n :: Int)
    let y = (read m :: Int)

    return((x,y))

main :: IO ()
-- Función principal del programa, con la que se empieza la ejecución.
main = do 
    (n, m) <- escogerDimensiones
    let b = createBoard n m
    putStrLn ""
    muestraBoard b
    putStrLn ""
    e <- escogerEstrategia
    t <- primerTurno

    if e == 1 then
        partida b t estrategia1
    else if e == 2 then
        partida b t estrategia2
    else
        partida b t estrategia3

ponerFicha :: Bool -> [[[Char]]] -> Int -> [[[Char]]]
-- Función que tras recibir un tablero, jugador, y columna, 
-- devuelve un tablero tras haber colocado en el anterior la ficha del jugador en su respectiva columna
ponerFicha isPlayer (f:fs) columnaE
    | f !! (columnaE) == "_" = (replaceB  f columnaE isPlayer) : fs
    | f !! (columnaE) == "X" || f !! (columnaE) == "O" = f : (ponerFicha isPlayer fs columnaE )
    | otherwise = [f]


replaceB :: [[Char]] -> Int -> Bool -> [[Char]]
-- Función que se encarga de substituir la posicion columnaE de la lista (f:fs) por la ficha del jugador indicado
replaceB (f:fs)  columnaE isPlayer
    | columnaE == 0 = 
                if isPlayer then
                    "O" : fs
                else
                    "X" : fs
    | otherwise = f : replaceB fs (columnaE - 1) isPlayer

jugadaRealizada :: Bool -> Int -> IO ()
-- Muestra en pantalla el tablero tras la ultima jugada que se ha realizado, apunta hacia el,
-- e indica cual ha sido ese movimiento y por quien.
jugadaRealizada isPlayer pos
    |isPlayer = putStrLn ("Has colocado ficha en la columna " ++ (show (pos + 1)) ++ " ↖\n")
    |otherwise = putStrLn ("El bot ha colocado ficha en la columna " ++ (show (pos + 1)) ++ " ↖\n")

partida :: [[[Char]]] -> Bool -> ([[[Char]]] -> Bool -> IO Int) -> IO ()
-- Función que se encarga de actualizar toda la situación de la partida mientra el usuario continue interactuando.
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
-- Muestra en pantalla el texto del correspondiente ganador de la partida si ha habido un ganador
ganador t = do
    if t then
        putStrLn "Has ganado tú!"
    else
        putStrLn "Ha ganado el bot!"

turnoUsuario :: [[[Char]]] -> IO Int
--Función que se encarga de pedir al usuario el numero de columna en el que poner la ficha, entre 1 y N.
turnoUsuario board = do
    let columnas = (length (head board))
    putStrLn ("Elige en que columna quieres poner la ficha entre la 1 y la " ++ show columnas ++ "\n")
    columnaE <- getLine
    let validPos = map (+1) (validPositions board)

    if not (any ((read columnaE :: Int)==) validPos) then
        do
        putStrLn("\nLa posición " ++ show (id (read columnaE :: Int))  ++ " es inválida\n")
        muestraBoard board
        putStrLn ""
        turnoUsuario board
    
    else
        return ((read columnaE :: Int) -1)

estrategia1 :: [[[Char]]] -> Bool -> IO Int
-- Función de la estrategia1 de movimientos aleatorios
estrategia1 board isPlayer = do
    if isPlayer then
        do
        x <- turnoUsuario board
        return (x)
    else
        do
        let validPos = validPositions board
        posrandom <- (randInt 0 ((length validPos) - 1))
        let posicionNuevaFicha = validPos !! posrandom

        return(posrandom)

estrategia2 :: [[[Char]]] -> Bool -> IO Int
-- Función de la estrategia2 greedy. Detalles en la documentación
estrategia2 board isPlayer = do
        if isPlayer then
            do
            x <- turnoUsuario board
            return (x)
        else
            do
            let validPos = validPositions board
            (mejoresPosiciones, maxGlobal) <- greedy board validPos

            randPos <- randInt 0 ((length mejoresPosiciones) - 1)
            
            let mejoresPosicionesU = (elementosUnicos mejoresPosiciones)
            let bestOfbests = map (validPos !!) mejoresPosicionesU
            let sol = map (ponerFicha True board ) (bestOfbests)

            let sol2 = map (comprobarWin True) sol

            if any (True==) sol2 then
                do 
                let posNextMove = (posOfTrueArray 0 sol2) !! 0
                
                let nextMove = (bestOfbests !! posNextMove)
                
                return (nextMove)
            else
                do

                return (validPos !! (mejoresPosiciones !! randPos))

estrategia3 :: [[[Char]]] -> Bool -> IO Int
-- Función de la estrategia3 smart. Detalles en la documentación
estrategia3 board isPlayer = do 
        if isPlayer then
            do
            x <- turnoUsuario board
            return (x)
        else
            do
            let validPos = validPositions board

            (mejoresPosiciones, maxGlobal) <- greedy board validPos


            --Si el bot puede ganar la partida, la ganara
            if maxGlobal >= 4 then
                do  -- Comprueba si puedes ganar, en tal caso, se hace alguna de las posibles jugadas ganadoras
                    randWinningPos <- randInt 0 ((length mejoresPosiciones) - 1)


                    return (validPos !! (mejoresPosiciones !! randWinningPos))
            
            else

                do  --Comprueba si el jugador puede ganar y bloquea esa jugada si es posible
                let posiblesMovimientosJugador = map (ponerFicha True board ) (validPos) --- Boards posibles del jugador

                let jugadaGanadoraJugador = map (comprobarWin True) posiblesMovimientosJugador

                if any (True==) jugadaGanadoraJugador then
                    do 
                    let posNextMove = (posOfTrueArray 0 jugadaGanadoraJugador) !! 0
                    let movimientoBloqueador = validPos !! posNextMove

                    return (movimientoBloqueador)
                
                else
                    --Si hay alguna columna por la que pueda ganar el bot si el jugador tira alli justo antes, el bot no tirara alli
                    --Si hay alguna columna por la que el jugador pueda ganar si el bot tira alli, el bot evitara tirar alli

                    do
                    let espaciosDobles = posOfTrueArray 0 (columnas2EspaciosOMas board)
                    let espacioUnico = posOfTrueArray 0 (columna1Espacio board)

                    let tableroSiJuegaBot = map (ponerFicha False board ) (espaciosDobles)
                    let tableroSiJuegaPlayer = map (ponerFicha True board ) (espaciosDobles)

                    let botPlayer = zipWith (ponerFicha True) tableroSiJuegaBot espaciosDobles
                    let playerBot = zipWith (ponerFicha False) tableroSiJuegaPlayer espaciosDobles

                    let evita1 = map (comprobarWin True) botPlayer
                    let evita2 = map (comprobarWin False) playerBot
                    
                    let posicionesaEvitar = zipWith (||) evita1 evita2
                    
                    let movimientosCorrectos = (posOfFalseArray 0 posicionesaEvitar)
                    
                    let posibles = map (espaciosDobles !!) movimientosCorrectos

                    let posiblesMovimientosCorrectos = sort (espacioUnico ++ posibles)

                    if length posiblesMovimientosCorrectos > 0 then --Si hay movimientos validos posibles tras el filtrado de posiciones anterior
                        do
                                                                    -- El bot comprueba si el usuario puede ganar en 2 movimientos de alguna forma
                        let horiJugador =  (map (posiblesHorizontales True board) posiblesMovimientosCorrectos )

                        let dondepuedejugaroponente = (map (ponerFicha True board) posiblesMovimientosCorrectos)

                        let puedeGanarsiHaceestemov = puedeGanarEnOtraJugada True dondepuedejugaroponente 

                        let consecHoriJugador = esLinea3enRaya True horiJugador posiblesMovimientosCorrectos


                        if any (True==) puedeGanarsiHaceestemov then --Hay un posible movimiento y el bot ha de bloquearlo
                            do

                            let a = posOfTrueArray 0 puedeGanarsiHaceestemov

                            let c = posiblesMovimientosCorrectos !! (a !! ((length a) `div` 2))

                            return (c)

                        else                                        --No lo hay, por lo tanto puedo utilizar la estrategia greedy mejorada
                            do
                            (soluciones, x) <- greedyMejorado board posiblesMovimientosCorrectos

                            let solucionesU = (elementosUnicos soluciones)

                            let movimientoValidoMasGreedy = posiblesMovimientosCorrectos !! (solucionesU !! (((length solucionesU) - 1) `div` 2))

                            return movimientoValidoMasGreedy


                    else
                        --Posiblemente el bot va a perder si se encuentra aqui.

                        -- El bot comprueba si el usuario puede ganar en 2 movimientos de alguna forma
                        
                        do
                        let horiJugador = (map (posiblesHorizontales True board) validPos)

                        let consecHoriJugador = esLinea3enRaya True horiJugador validPos

                        let movimientos3H = posOfTrueArray 0 consecHoriJugador
                        let boardsposible3H = map (posiblesMovimientosJugador !!) (movimientos3H)
    
                        let deboBloquear = puedeGanarEnOtraJugada True boardsposible3H

                        if any (True==) deboBloquear then   --Hay un posible movimiento y el bot ha de bloquearlo
                            do
                            let a = posOfTrueArray 0 deboBloquear
                            let b = movimientos3H !! (a !! 0)
                            let c = validPos !! b
                            return (c)

                        else
                            --No quedan mas opciones, el bot intenta un greedy
                            do
                            (soluciones, x) <- greedyMejorado board validPos

                            let solucionesU = (elementosUnicos soluciones)

                            let movimientoValidoMasGreedy = posiblesMovimientosCorrectos !! (solucionesU !! (((length solucionesU) - 1) `div` 2))

                            return movimientoValidoMasGreedy


greedy :: [[[Char]]] -> [Int] -> IO ([Int], Int)
-- Función que dado un tablero y sus posiciones validas, devuelve
-- una serie de posiciones respecto a las dadas en validPos indicando en que validPos
-- se puede formar la raya mas grande para el bot,
-- (las repeticiones se deben a que pueden formarse rayas diagonales, horizontales
-- o verticales en el momento en el que se coloque la ficha en una misma posicion)
-- y cual es la longitud de esa raya que se va a formar.
greedy board validPos =
    do 
        
    let posColumnas = map (dropWhile ("_"==)) (map (posiblesColumnas False board) validPos)
    let consecVerticales =  (map (length) (map (takeWhile ("X"==)) posColumnas))
    let posHorizontales = (map (posiblesHorizontales False board) validPos)
    let consecHorizontales = zipWith (contarFichasConsecutivasDadaPosicion False) posHorizontales validPos

    let posDiagonales1 = posiblesDiagonales board validPos False
    let diag1 = map fst posDiagonales1
    let positions1 = map snd posDiagonales1
    let consecDiagonales1 = zipWith (contarFichasConsecutivasDadaPosicion False) diag1 positions1

    let posDiagonales2 = posiblesDiagonales board validPos True
    let diag2 = map fst posDiagonales2
    let positions2 = map snd posDiagonales2
    let consecDiagonales2 = zipWith (contarFichasConsecutivasDadaPosicion False) diag2 positions2

    let maxGlobal = maximum (consecVerticales ++ consecHorizontales ++ consecDiagonales1 ++ consecDiagonales2)
    let consecutivosFiltrado = map (posicionValorEnLista maxGlobal 0) ([consecVerticales] ++ [consecHorizontales] ++ [consecDiagonales1] ++ [consecDiagonales2])
    let mejoresPosiciones = uneListas consecutivosFiltrado

    return ((mejoresPosiciones, maxGlobal))
    

greedyMejorado :: [[[Char]]] -> [Int] -> IO ([Int], Int)
-- Función que dado un tablero y sus posiciones validas, devuelve
-- una serie de posiciones respecto a las dadas en validPos indicando
-- en que columnas se puede formar la raya mas grande para el bot, que ademas,
-- ofrezca la posibilidad de poder formar un 4 en raya en algun futuro,
-- y cual es la longitud de esa raya que se pueda formar.
greedyMejorado board validPos =
    do 

    let posColumnas = (map (posiblesColumnas False board) validPos )
    let consecVerticales =  (map (length) (map (takeWhile (crossOrEmpty)) posColumnas))
    let posToConsider = map (>=4) consecVerticales
    let verticalesConPotencial = zipWith (numeroXsConsecutivas) posToConsider posColumnas

    let posHorizontales = (map (posiblesHorizontales False board)  validPos)
    let horizontalesAConsiderar = zipWith (contarFichasConsecutivasYEspacios)  posHorizontales validPos
    let horizontalesConPotencial = zipWith3 (contarFichasGreedyM False) horizontalesAConsiderar validPos posHorizontales

    let posDiagonales1 = posiblesDiagonales board validPos False
    let diag1 = map fst posDiagonales1
    let positions1 = map snd posDiagonales1
    let diagonales1AConsiderar = zipWith (contarFichasConsecutivasYEspacios)  diag1 positions1
    let diagonales1ConPotencial = zipWith3 (contarFichasGreedyM False) horizontalesAConsiderar positions1 diag1


    let posDiagonales2 = posiblesDiagonales board validPos True
    let diag2 = map fst posDiagonales2
    let positions2 = map snd posDiagonales2
    let diagonales2AConsiderar = zipWith (contarFichasConsecutivasYEspacios)  diag2 positions2
    let diagonales2ConPotencial = zipWith3 (contarFichasGreedyM False) horizontalesAConsiderar positions2 diag2


    let maxGlobal = maximum (verticalesConPotencial ++ horizontalesConPotencial ++ diagonales1ConPotencial ++ diagonales2ConPotencial)
    let consecutivosFiltrado = map (posicionValorEnLista maxGlobal 0) ([verticalesConPotencial] ++ [horizontalesConPotencial] ++ [diagonales1ConPotencial] ++ [diagonales2ConPotencial])
    let mejoresPosiciones = uneListas consecutivosFiltrado


    return ((mejoresPosiciones, maxGlobal))

columnasEspacios :: Int -> [[Char]] -> Int
-- Dada una columna, indica cuantos espacios libres hay en ella.
columnasEspacios num [] = num
columnasEspacios num (b:bs)
    |b == "_" = columnasEspacios (num + 1) bs
    |otherwise = num


columnas2EspaciosOMas :: [[[Char]]] -> [Bool]
-- Dado un board, devuelve una lista de booleanos que indica que columnas tienen 2 o mas espacios libres.
columnas2EspaciosOMas board = map (>= 2) (map (columnasEspacios 0) (map reverse (transpose board))) 

columna1Espacio :: [[[Char]]] -> [Bool]
-- Dado un board, devuelve una lista de booleanos que indica que columnas tienen 1 unico espacio libre.
columna1Espacio board = map (== 1) (map (columnasEspacios 0) (map reverse (transpose board))) 

puedeGanarEnOtraJugada :: Bool -> [[[[Char]]]] -> [Bool]
-- Dado un jugador y una serie de tableros, indica en cual de estos tableros el jugador puede ganar poniendo una sola ficha
puedeGanarEnOtraJugada _ [] = []
puedeGanarEnOtraJugada isPlayer (b:bs)
    |isPlayer = 
        [foldl (||) (False) (map (comprobarWin True) (map (ponerFicha True b ) (validPositions  b)))] ++ puedeGanarEnOtraJugada isPlayer bs
    |otherwise = []

contarFichasConsecutivas :: Int -> Bool -> [[Char]] -> Int
--Función que indica la longitud de una raya empezando en una posicion concreta en adelante para el jugador dado.
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

esLinea3enRaya :: Bool -> [[[Char]]] -> [Int] -> [Bool]
-- Dada una lista de lineas, una lista de posiciones y un jugador. Recorriendo ambas listas simultaniamente, 
-- indica si el tamaño de la raya que forma el jugador en esa linea a partir de esa posicion es mayor o igual que 3
esLinea3enRaya _ [] _ = []
esLinea3enRaya isPlayer (f:fs) (p:ps) = (contarFichasConsecutivas 0 isPlayer (drop (p) f) + contarFichasConsecutivas 0 isPlayer (drop ((length f) - p) (reverse f)) >= 3)
                                        : esLinea3enRaya isPlayer fs ps

crossOrEmpty :: [Char] -> Bool
-- Función que dada un String, devuelve True si es "X" o "_". Devolverá False en caso contrario
crossOrEmpty a = a =="X" || "_" == a

numeroXsConsecutivas :: Bool -> [[Char]] -> Int
-- Función que dada una fila de strings, ignora todos los "_" y cuenta todas las "X" que hayan consecutivamente despues.
numeroXsConsecutivas b f 
    |b = length (takeWhile ("X"==) (dropWhile ("_"==) f))
    |otherwise = 0

contarFichasConsecutivasYEspacios :: [[Char]] -> Int -> Bool
-- Función que dada una lista y una posición de la lista, determina si desde esa posición, contando hacia adelante y hacia atras
-- de la lista, hay mas de 4 "X" o "_" consecutivamente
contarFichasConsecutivasYEspacios f p = (length (takeWhile (crossOrEmpty) (drop (p) f)) + length (takeWhile (crossOrEmpty) (drop ((length f) - p) (reverse f)) ))>=4
                   
contarFichasGreedyM :: Bool -> Bool -> Int -> [[Char]] -> Int
-- Función que dado un jugador, un booleano como indicador, una posicion, y una fila.
-- Si el indicador es True, se devuelve la suma de fichas consecutivas del jugador en la posicion indicada de la fila.
-- En caso contrario, se devueleve 0
contarFichasGreedyM isPlayer b p f
    | b = contarFichasConsecutivas 0 isPlayer (drop (p) f) + contarFichasConsecutivas 0 isPlayer (drop ((length f) - p) (reverse f))
    | otherwise = 0

comprobarWin :: Bool -> [[[Char]]] -> Bool
-- Función que dado un jugador y un tablero, confirma si el jugador ha ganado en ese tablero
comprobarWin isPlayer board = checkFilas board isPlayer || checkVertical board isPlayer || checkDiagonals board isPlayer

posDiferencia :: [[Char]] -> [[Char]] -> Int -> Int
-- Función que dada dos listas diferentes, te devuelve en que posicion son diferentes
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
-- el board dado en cada posicion valida, y segundo, la altura a la que esta la nueva ficha colocada.
-- Si invertida es True, entonces antes de hacer todo lo mencionado anteriormente, se invertirá el tablero 
-- para trabajar con las diagonales inversas.
posiblesDiagonales _ [] _ = []
posiblesDiagonales board (p:ps) invertida 
    |not invertida = (diagonalDiferente board (ponerFicha False board p)) : posiblesDiagonales board ps invertida
    |otherwise = (diagonalDiferente (map reverse board) (map reverse (ponerFicha False board p))) : posiblesDiagonales board ps invertida

posOfFalseArray :: Int -> [Bool] -> [Int]
-- Dada una lista de booleanos y un numero que indica la posicion en esta lista, deuelve una nueva lista que contiene
-- las posiciones de aquellos valores a False en la lista dada inicialmente
posOfFalseArray pos [] = []
posOfFalseArray pos (a:as)
    |not a = [pos] ++ posOfFalseArray (pos + 1) as
    |otherwise = posOfFalseArray (pos + 1) as

posOfTrueArray :: Int -> [Bool] -> [Int]
-- Dada una lista de booleanos y un numero que indica la posicion en esta lista, deuelve una nueva lista que contiene
-- las posiciones de aquellos valores a True en la lista dada inicialmente
posOfTrueArray _ [] = []
posOfTrueArray pos (a:as)
    |a = [pos] ++ posOfTrueArray (pos + 1) as
    |otherwise = posOfTrueArray (pos + 1) as 


uneListas :: [[Int]] -> [Int]
-- Función que une un conjunto de listas separadas.
uneListas (x:[]) = x
uneListas (x:xs) = x ++ uneListas xs

elementosUnicos :: [Int] -> [Int]
-- Función que ordena una lista y devuelve los mismos elementos sin repeticiones.
elementosUnicos xs = eliminaRepeticiones $ sort xs
  where
    --Funcion que elimina repeticiones consecutivas
    eliminaRepeticiones []  = []
    eliminaRepeticiones [x] = [x]
    eliminaRepeticiones (x1:x2:xs)
      | x1 == x2  = eliminaRepeticiones (x1:xs)
      | otherwise = x1 : eliminaRepeticiones (x2:xs)


contarFichasConsecutivasDadaPosicion :: Bool -> [[Char]] -> Int -> Int
-- Función que dado un jugador, una fila y una posicion
-- Devuelve la suma de fichas consecutivas del jugador en la posicion indicada de la fila.
contarFichasConsecutivasDadaPosicion isPlayer f p = contarFichasConsecutivas 0 isPlayer (drop (p) f) + contarFichasConsecutivas 0 isPlayer (drop ((length f) - p) (reverse f))

posiblesColumnas :: Bool -> [[[Char]]] -> Int -> [[Char]]
-- Funcion que dado un jugador, el board, y una posición, devuelve la columna que se vería afectada
-- por un movimiento del jugador sobre esa posicion.
posiblesColumnas isPlayer board p = (((map reverse (transpose (ponerFicha isPlayer board p ))) !! p))

posiblesHorizontales :: Bool -> [[[Char]]] -> Int -> [[Char]]
-- Funcion que dado un jugador, el board, y una posición, devuelve la horizontal que se vería afectada
-- por un movimiento del jugador sobre esa posicion.
posiblesHorizontales isPlayer board p  = (cogerUltimaHorizontalConValor p (ponerFicha isPlayer board p ))

cogerUltimaHorizontalConValor :: Int -> [[[Char]]] -> [[Char]]
-- Función que dada una posicion y un tablero, la horizontal no vacia que este mas arriba en el
-- tablero en la posicion dada.
cogerUltimaHorizontalConValor _ (f:[]) = (f)
cogerUltimaHorizontalConValor pos (f:fs) 
    |(head fs) !! pos == "_" = (f)
    |otherwise = cogerUltimaHorizontalConValor pos fs

posicionValorEnLista :: Int -> Int -> [Int] -> [Int]
-- Dado una lista y un valor, devuelve una nueva lista indicando en que posiciones de la lista anterior
-- se encuentra dicho valor.
posicionValorEnLista _  _ []  = []
posicionValorEnLista valor pos (a:as)
    |a == valor = [pos] ++ posicionValorEnLista valor (pos+1) as
    |otherwise = posicionValorEnLista valor (pos + 1) as

validPositions :: [[[Char]]] -> [Int]
-- Función que dado un board, devuelve las posiciones de aquellas columnas donde se pueden colocar fichas.
validPositions board = validPositions' (transpose board)

validPositions' :: [[[Char]]] -> [Int]
-- Función auxiliar de validPositions que ayuda a conseguir las posiciones validas
validPositions' board = posOfTrueArray 0 (map (any ("_"==)) board) 


diagonals :: [[[Char]]] -> [[[Char]]]
--Función que dada una lista de listas rectangular, devuelve sus diagonales como lista de listas
diagonals []       = []
diagonals ([]:xs) = xs
diagonals x      = zipWith (++) (map ((:[]) . head) x ++ repeat [])
                                    ([]:(diagonals (map tail x)))


transpose :: [[[Char]]] -> [[[Char]]]
--Función que dada una lista de listas rectangular, devuelve su transpuesta
transpose ([]:_) = []
transpose b = (map head b) : transpose (map tail b)

checkDiagonals :: [[[Char]]] -> Bool -> Bool
-- Función que dado un tablero y un jugador, confirma si el jugador ha ganado por alguna de las diagonales del tablero.
checkDiagonals board isPlayer = checkFilas (diagonals board)  isPlayer || checkFilas (diagonals (map reverse board)) isPlayer

checkVertical :: [[[Char]]] -> Bool -> Bool
-- Función que dado un tablero y un jugador, confirma si el jugador ha ganado por alguna vertical del tablero.
checkVertical board isPlayer = checkFilas (transpose board) isPlayer

checkFilas :: [[[Char]]] -> Bool -> Bool
-- Función que dado un tablero y un jugador, confirma si el jugador ha ganado por alguna fila de las dadas del tablero.
checkFilas board isPlayer = foldl (||) (False) (map (confirmWin isPlayer 0) board)

confirmWin :: Bool -> Int -> [[Char]] -> Bool
-- Función que dada una lista, y un jugador, indica si el jugador tiene 4 fichas consecutivas en la lista.
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
-- Función que ordena una lista de enteros de menor a mayor.
sort [] = []
sort [x] = [x]
sort (x:xs) = (sort menors) ++ [x] ++ (sort majors)
    where 
        menors = [y | y<-xs , y < x]
        majors = [y | y<-xs , y >= x]