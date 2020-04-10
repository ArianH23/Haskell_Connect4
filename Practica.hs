import System.Random

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result


-- createBoard :: String -> String -> [[[Char]]]
createBoard n m = splitEvery m (take (n * m) rows)
    -- where x = read n :: Int
    --       y = read m :: Int

rows = ['_'] : rows


-- splitEvery :: Int -> [[Char]] -> [[[Char]]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

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


muestraBoard [] = return()
muestraBoard (b:bs) = do 
    muestraBoard bs
    print b

primerTurno = do
    putStrLn "\nQuieres tener el primer movimiento (escribe 0) o el segundo (escribe 1)?"
    putStrLn "Recuerda que el bot siempre sera las X y tu los O?"

    t <- getLine
    
    let num = (read t :: Int)

    if (num /= 0) && (num /= 1) then
        do
        putStrLn "Opción no valida, vuelve a escoger por favor."
        primerTurno
    else
        if num == 0 then
            return (True)
        else
            return(False)

escogerDimensiones = do
    putStrLn "Elige el numero de filas del tablero"
    n <- getLine
    putStrLn "Elige el numero de columnas del tablero"
    m <- getLine
    let x = (read n :: Int)
    let y = (read m :: Int)
    if (x < 4 && y < 4) || x <1 || y <1 then
        do
        putStrLn "Al menos uno de los dos valores tiene que ser mayor o igual que 4 y ambos positivos para que el juego tenga sentido."
        putStrLn "Por favor, vuelve a introducir las dimensiones.\n"
        escogerDimensiones
    else 
        return((x,y))

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
        partida b t estrategia2


    
ponerFicha' board isPlayer columnaE = ponerFicha board columnaE isPlayer

ponerFicha :: [[[Char]]] -> Int -> Bool -> [[[Char]]]
ponerFicha (f:fs) columnaE isPlayer 
    | f !! (columnaE) == "_" = (replaceB f columnaE isPlayer) : fs
    | f !! (columnaE) == "X" || f !! (columnaE) == "O" = f : (ponerFicha fs columnaE isPlayer)
    | otherwise = [f]


replaceB :: [[Char]] -> Int -> Bool -> [[Char]]
replaceB (f:fs)  columnaE isPlayer
    | columnaE == 0 = 
                if isPlayer then
                    "O" : fs
                else
                    "X" : fs
    | otherwise = f : replaceB fs (columnaE - 1) isPlayer

-- partida :: [[[Char]]] -> Bool -> ([[[Char]]] -> Bool-> IO Int) -> IO Int
partida board t estrategia = do
    posicionNuevaFicha <- estrategia board t
    let nboard = ponerFicha board posicionNuevaFicha t
    muestraBoard nboard
    putStrLn ""
    if comprobarWin t nboard then
        do
        ganador t
        return()
    else
        do
        partida nboard (not t) estrategia
        return()

ganador t = do
    if t then
        putStrLn "Has ganado tú!"
    else
        putStrLn "Ha ganado el bot!"

turnoJugador :: [[[Char]]] -> IO Int
turnoJugador board = do
    let columnas = (length (head board))
    putStrLn ("Elige donde quieres poner la ficha entre el 1 y el " ++ show columnas ++ "\n")
    columnaE <- getLine
    -- print (head b)
    let validPos = map (+1) (validPositions (transpose board))

    if not (any ((read columnaE :: Int)==) validPos) then
        do
        putStrLn("Posición inválida\n")
        turnoJugador board
    
    else
        return ((read columnaE :: Int) -1)

estrategia1 :: [[[Char]]] -> Bool -> IO Int
estrategia1 board isPlayer = do
    if isPlayer then
        do
        let columnas = (length (head board))
        putStrLn ("Elige donde quieres poner la ficha entre el 1 y el " ++ show columnas ++ "\n")
        columnaE <- getLine
        -- print (head b)
        let validPos = map (+1) (validPositions (transpose board))
    
        if not (any ((read columnaE :: Int)==) validPos) then
            do
            putStrLn("Posición inválida\n")
            turnoJugador board
        
        else
            return ((read columnaE :: Int) -1)
    else
        do
        let validPos = validPositions (transpose board)
        posrandom <- (randInt 0 ((length validPos) - 1))
        let posicionNuevaFicha = validPos !! posrandom
        -- putStrLn ("El bot ha colocado ficha en la posicion " ++ (show (posicionNuevaFicha+1) )++ "\n")
        -- let nboard = ponerFicha board posicionNuevaFicha t
        -- muestraBoard nboard
        return(posrandom)


estrategia2 :: [[[Char]]] -> Bool -> IO Int
estrategia2 board isPlayer = do
        if isPlayer then
            do
            let columnas = (length (head board))
            putStrLn ("Elige donde quieres poner la ficha entre el 1 y el " ++ show columnas ++ "\n")
            columnaE <- getLine
            -- print (head b)
            let validPos = map (+1) (validPositions (transpose board))
        
            if not (any ((read columnaE :: Int)==) validPos) then
                do
                putStrLn("Posición inválida\n")
                turnoJugador board
            
            else
                return ((read columnaE :: Int) -1)
        else
            do
        -- print (map reverse (transpose board))
        let validPos = validPositions (transpose board)
        -- print validPos
        let posColumnas = (posiblesColumnas board validPos False)
        let consecVerticales =  (map (length) (map (takeWhile ("X"==)) posColumnas))
        let posHorizontales = (posiblesHorizontales board validPos False)
        let consecHorizontales = consecutivosHorizontales posHorizontales validPos

        let posDiagonales1 = posiblesDiagonales board validPos False
        let diag1 = map fst posDiagonales1
        let positions1 = map snd posDiagonales1
        let consecDiagonales1 = (consecutivosHorizontales diag1 positions1)

        let posDiagonales2 = posiblesDiagonales board validPos True
        let diag2 = map fst posDiagonales2
        let positions2 = map snd posDiagonales2
        let consecDiagonales2 = (consecutivosHorizontales diag2 positions2)

        -- print posDiagonales2s)
        -- print posHorizontales
        let maxGlobal = maximum (consecVerticales ++ consecHorizontales ++ consecDiagonales1 ++ consecDiagonales2)
        let xd = map (maxPos maxGlobal 0) ([consecVerticales] ++ [consecHorizontales] ++ [consecDiagonales1] ++ [consecDiagonales2])
        let mejoresPosiciones = uneListas xd
        -- print (maxGlobal)
    
        -- print (posOfFalse (zipWith (==) (diagonals board) (diagonals (ponerFicha board 0 False))) 0)
        

        randPos <- randInt 0 ((length mejoresPosiciones) - 1)
        -- print validPos
        -- print mejoresPosiciones
        -- print (elementosUnicos mejoresPosiciones)
        let mejoresPosicionesU = (elementosUnicos mejoresPosiciones)

        let sol = map (ponerFicha' board True) (mejoresPosicionesU)
        -- print (sol)
        let sol2 = map (comprobarWin True) sol
        -- print sol2
        if any (True==) sol2 then
            do 
            let posNextMove = posOfTrueMove sol2 0
            let nextMove = mejoresPosicionesU !! posNextMove
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
            return (mejoresPosiciones !! randPos)


comprobarWin t board = checkHorizontal board t || checkVertical board t || checkDiagonals board t

posDiferencia (b1:b1s) (b2:b2s) pos
    |b1 /= b2 = pos
    |otherwise = posDiferencia b1s b2s (pos+1)
    
diagonalDiferente board1 board2 = ((diagonals board2) !! x, posDiferencia ((diagonals board1) !! x) ((diagonals board2) !! x) 0)
    where x = (posOfFalse (zipWith (==) (diagonals board1) (diagonals board2)) 0)


posiblesDiagonales _ [] _ = []
posiblesDiagonales board (p:ps) invertida 
    |not invertida = (diagonalDiferente board (ponerFicha board p False)) : posiblesDiagonales board ps invertida
    |otherwise = (diagonalDiferente (map reverse board) (map reverse (ponerFicha board p False))) : posiblesDiagonales board ps invertida

posOfTrueMove [] pos = pos
posOfTrueMove (f:fs) pos
    |f = pos
    |otherwise = posOfTrueMove fs (pos+1)

posOfFalse [] pos = pos
posOfFalse (f:fs) pos
    |not f = pos
    |otherwise = posOfFalse fs (pos+1)

uneListas (x:[]) = x
uneListas (x:xs) = x ++ uneListas xs

elementosUnicos xs = borrar $ sort xs
  where
    borrar []  = []
    borrar [x] = [x]
    borrar (x1:x2:xs)
      | x1 == x2  = borrar (x1:xs)
      | otherwise = x1 : borrar (x2:xs)

analizaHorizontal n [] = n
analizaHorizontal n (f:fs)
    |f == "X" = analizaHorizontal (n+1) fs
    |otherwise = n

consecutivosHorizontales [] _ = []
consecutivosHorizontales (f:fs) (p:ps) = analizaHorizontal 0 (drop (p) f) + analizaHorizontal 0 (drop ((length f) - p) (reverse f))
                                        : consecutivosHorizontales fs ps

posiblesColumnas _ [] _ = []
posiblesColumnas board (p:ps) isPlayer = (dropWhile ("_"==) ((map reverse (transpose (ponerFicha board p isPlayer))) !! p)) : posiblesColumnas board ps isPlayer

posiblesHorizontales     _ [] _ = []
posiblesHorizontales board (p:ps) isPlayer = (cogerUltimaHorizontalVacia p (ponerFicha board p isPlayer)) : posiblesHorizontales board ps isPlayer

cogerUltimaHorizontalVacia _ (f:[]) = f
cogerUltimaHorizontalVacia pos (f:fs) 
    |(head fs) !! pos == "_" = f
    |otherwise = cogerUltimaHorizontalVacia pos fs

maxPos _  _ []  = []
maxPos max pos (a:as)
    |a == max = [pos] ++ maxPos max (pos+1) as
    |otherwise = maxPos max (pos + 1) as

-- validPositions :: [[[Char]]] -> [Int]
validPositions board = posOfTrue (map (any ("_"==)) board) 0

maximasOcurrencias  _ _ max [] = max
maximasOcurrencias isPlayer count max (f:fs)
    |isPlayer = 
        if f == "O" then
            if max < (count + 1) then
                maximasOcurrencias isPlayer (count+1) (max+1) fs
            else 
                maximasOcurrencias isPlayer (count+1) max fs

        else 
            maximasOcurrencias isPlayer 0 max fs
            
    |not isPlayer = 
        if f == "X" then
            if max < (count + 1) then
                maximasOcurrencias isPlayer (count+1) (max+1) fs
            else 
                maximasOcurrencias isPlayer (count+1) max fs

        else 
            maximasOcurrencias isPlayer 0 max fs

-- valoresMaximos :: [[[Char]]] -> [[Int]]
valoresMaximos [] = []
valoresMaximos (b:bs) = (map (maximasOcurrencias False 0 0) b ++ map (maximasOcurrencias False 0 0) (transpose b)
                        ++ map (maximasOcurrencias False 0 0) (diagonals (transpose b))
                        ++ map (maximasOcurrencias False 0 0) (diagonals (map reverse (transpose b))) )
                        : valoresMaximos bs

boardsPosibles _ [] _ = []
boardsPosibles board (p:ps) isPlayer = (ponerFicha board p isPlayer) : boardsPosibles board ps isPlayer

posOfTrue [] _ = []
posOfTrue (x:xs) y
    |x = y : posOfTrue xs (y+1)
    |otherwise = posOfTrue xs (y+1)


diagonals []       = []
diagonals ([]:xs) = xs
diagonals x      = zipWith (++) (map ((:[]) . head) x ++ repeat [])
                                    ([]:(diagonals (map tail x)))

transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

checkDiagonals board isPlayer = checkHorizontal (diagonals board)  isPlayer || checkHorizontal (diagonals (map reverse board)) isPlayer

checkVertical board isPlayer = checkHorizontal (transpose board) isPlayer

checkHorizontal :: [[[Char]]] -> Bool -> Bool
checkHorizontal board isPlayer = foldl (||) (False) (map (confirmWin isPlayer 0) board)

confirmWin :: Bool -> Int -> [[Char]] -> Bool
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
sort [] = []
sort [x] = [x]
sort (x:xs) = (sort menors) ++ [x] ++ (sort majors)
    where 
        menors = [y | y<-xs , y < x]
        majors = [y | y<-xs , y >= x]