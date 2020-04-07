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
    play b True (read e::Int)


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
    if t then
        do 
        let columnas = (length (head board))
        putStrLn ("Elige donde quieres poner la ficha entre el 1 y el " ++ show columnas ++ "\n")
        columnaE <- getLine
        -- print (head b)
        let validPos = map (+1) (validPositions (transpose board))

        if not (any ((read columnaE :: Int)==) validPos) then
            do
            putStrLn("Posición invalida")
            play board t estrategia
        
        else
            do
            let nboard = ponerFicha board ((read columnaE :: Int) - 1) t
            muestraBoard nboard
            putStrLn "\n"
            
            if checkHorizontal nboard t || checkVertical nboard t || checkDiagonals nboard t then
                do
                putStrLn "Has ganado tú!"
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
            putStrLn ("El bot ha colocado ficha en la posicion " ++ (show (posicionNuevaFicha+1) ))
            let nboard = ponerFicha board posicionNuevaFicha t
            muestraBoard nboard
            
            if checkHorizontal nboard t || checkVertical nboard t || checkDiagonals nboard t then
                do
                putStrLn "Ha ganado el bot!"
                return()
            else            
                play nboard (not t) estrategia
    
        else 
            return()

       
        return()

-- validPositions :: [[[Char]]] -> [Int]
validPositions board = posOfTrue (map (any ("_"==)) board) 0

maximasOcurrencias [] _ count = [count]
maximasOcurrencias (f:fs) isPlayer count 
    |isPlayer = 
        if f == "O" then
            maximasOcurrencias fs isPlayer (count+1)
        else 
            if count>0 then
                [count] ++ maximasOcurrencias fs isPlayer 0
            else
                maximasOcurrencias fs isPlayer 0
    |not isPlayer =
        if f == "X" then
            maximasOcurrencias fs isPlayer (count+1)
        else 
            if count>0 then
                [count] ++ maximasOcurrencias fs isPlayer 0
            else
                maximasOcurrencias fs isPlayer 0

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