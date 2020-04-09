import System.Random

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result


-- createBoard :: String -> String -> [[[Char]]]
createBoard n m = splitEvery y (take (x * y) rows)
    where x = read n :: Int
          y = read m :: Int
rows = ['_'] : rows


-- splitEvery :: Int -> [[Char]] -> [[[Char]]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list


muestraBoard [] = return()
muestraBoard (b:bs) = do 
    muestraBoard bs
    print b

main = do 
    putStrLn "Elige el numero de filas del tablero"
    n <- getLine
    putStrLn "Elige el numero de columnas del tablero"
    m <- getLine
    let b = createBoard n m
    muestraBoard b
    putStrLn "Que estrategia quieres que el bot tenga?: 1.Random 2.Greedy 3.Smart"
    e <- getLine
    play b False (read e::Int)


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


play board t estrategia = do 
    -- print (transpose board)
    -- print (board)
    -- print (diagonals board)
    -- print (diagonals (reverse board))
    -- print ((valoresMaximos([transpose board])))

    if t then
        do 
        let columnas = (length (head board))
        putStrLn ("Elige donde quieres poner la ficha entre el 1 y el " ++ show columnas ++ "\n")
        columnaE <- getLine
        -- print (head b)
        let validPos = map (+1) (validPositions (transpose board))

        if not (any ((read columnaE :: Int)==) validPos) then
            do
            putStrLn("Posición inválida\n")
            play board t estrategia
        
        else
            do
            let nboard = ponerFicha board ((read columnaE :: Int) - 1) t
            muestraBoard nboard
            putStrLn "\n"
            
            if checkHorizontal nboard t || checkVertical nboard t || checkDiagonals nboard t then
                do
                putStrLn "Has ganado tú!\n"
                return()
            else            
                play nboard (not t) estrategia
    else
        do
        if estrategia == 1 then
            do
            let validPos = validPositions (transpose board)
            posrandom <- randInt 0 ((length validPos) - 1)
            let posicionNuevaFicha = validPos !! posrandom
            putStrLn ("El bot ha colocado ficha en la posicion " ++ (show (posicionNuevaFicha+1) )++ "\n")
            let nboard = ponerFicha board posicionNuevaFicha t
            muestraBoard nboard
            
            if checkHorizontal nboard t || checkVertical nboard t || checkDiagonals nboard t then
                do
                putStrLn "Ha ganado el bot!\n"
                return()
            else            
                play nboard (not t) estrategia
    
        else if estrategia == 2 then 
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

            -- print posDiagonales2
            -- print consecDiagonales2
            -- print (posiblesDiagonales board validPos)
            -- print consecDiagonales1
            -- print posHorizontales
            let maxGlobal = maximum (consecVerticales ++ consecHorizontales ++ consecDiagonales1 ++ consecDiagonales2)
            let xd = map (maxPos maxGlobal 0) ([consecVerticales] ++ [consecHorizontales] ++ [consecDiagonales1] ++ [consecDiagonales2])
            let mejoresPosiciones = uneListas xd
            -- print (maxGlobal)
            -- print xd
            -- print mejoresPosiciones
            -- print (consecVerticales)
            -- print (consecHorizontales)
            -- print ([consecVerticales] ++ [consecHorizontales])

            -- muestraBoard (ponerFicha board 1 False)
            -- let possibleBoards = boardsPosibles board validPos False
            -- let bestValues = map maximum (valoresMaximos possibleBoards)
            -- let posOfMax = maxPos (maximum bestValues) 0 bestValues

            -- print (diagonals board)
            -- print (diagonals (ponerFicha board 0 False))
            -- print (zipWith (==) (diagonals board) (diagonals (ponerFicha board 0 False)))
            -- print (posOfFalse (zipWith (==) (diagonals board) (diagonals (ponerFicha board 0 False))) 0)
            

            randPos <- randInt 0 ((length mejoresPosiciones) - 1)
            -- print posOfMax
            let nboard = ponerFicha board (validPos !! (mejoresPosiciones !! randPos) ) t
            muestraBoard nboard
            putStrLn ("El bot ha colocado ficha en la posicion " ++ (show ((mejoresPosiciones !! randPos) + 1) ) ++ "\n")

            if checkHorizontal nboard t || checkVertical nboard t || checkDiagonals nboard t then
                do
                putStrLn "Ha ganado el bot!\n"
                return()
            else            
                play nboard (not t) estrategia

        else
            return()
       
        return()

        
posDiferencia (b1:b1s) (b2:b2s) pos
    |b1 /= b2 = pos
    |otherwise = posDiferencia b1s b2s (pos+1)
    
diagonalDiferente board1 board2 = ((diagonals board2) !! x, posDiferencia ((diagonals board1) !! x) ((diagonals board2) !! x) 0)
    where x = (posOfFalse (zipWith (==) (diagonals board1) (diagonals board2)) 0)


posiblesDiagonales _ [] _ = []
posiblesDiagonales board (p:ps) invertida 
    |not invertida = (diagonalDiferente board (ponerFicha board p False)) : posiblesDiagonales board ps invertida
    |otherwise = (diagonalDiferente (map reverse board) (map reverse (ponerFicha board p False))) : posiblesDiagonales board ps invertida

posOfFalse [] pos = pos
posOfFalse (f:fs) pos
    |not f = pos
    |otherwise = posOfFalse fs (pos+1)

uneListas (x:[]) = x
uneListas (x:xs) = x ++ uneListas xs

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