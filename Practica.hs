-- createBoard :: String -> String -> [[[Char]]]
createBoard n m = splitEvery y (take (x * y) rows)
    where x = read n :: Int
          y = read m :: Int

rows = "-" ++ rows


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
    play b True


-- ponerFicha :: [[[Char]]] -> Int -> Bool -> [[[Char]]]
ponerFicha (f:fs) columnaE isPlayer 
    | f !! (columnaE - 1) == '-' = (replaceB f columnaE isPlayer) : fs
    | f !! (columnaE - 1) == 'X' || f !! (columnaE -1) == 'O' = f : (ponerFicha fs columnaE isPlayer)
    -- | otherwise = f


-- replaceB :: [[Char]] -> Int -> Bool -> [[Char]]
replaceB (f:fs)  columnaE isPlayer
    | columnaE == 1 = 
                if isPlayer then
                    'O' : fs
                else
                    'X' : fs
    | otherwise = f : replaceB fs (columnaE - 1) isPlayer


play board t  = do 
    
        return()
    -- -- if t then
    -- --     do 
        let columnas = (length (head board))
        putStrLn ("Elige donde quieres poner la ficha entre el 1 y el " ++ show columnas)
        columnaE <- getLine
        -- print (head b)
        let nboard = ponerFicha board (read columnaE :: Int) t
        muestraBoard nboard
        putStrLn "\n"
        
        if checkHorizontal nboard t then
            do
            putStrLn "Has ganado!"
            return()
        else
            play nboard (not t)
    -- else
        -- return()


checkHorizontal board isPlayer = foldl (||) (False) (map (checkHorizontal' isPlayer 0) board)

-- checkHorizontal' :: Bool -> Int -> [[Char]] -> Bool
checkHorizontal' isPlayer 4 _ = True

checkHorizontal' isPlayer fichasSeguidas [] = False

checkHorizontal' isPlayer fichasSeguidas (f:fs) 
    -- |fichasSeguidas == 4 = True
    -- |l == [] = False
    |f == '-' = checkHorizontal' isPlayer 0 fs
    -- |f == "O" = True
    -- |f == "_" = checkHorizontal' isPlayer 0 fs
    
    |isPlayer == True = 
        if f == 'O' then
            checkHorizontal' isPlayer (fichasSeguidas+1) fs
        else
            checkHorizontal' isPlayer 0 fs
            
    |(not isPlayer) = 
        if f == 'X' then    
            checkHorizontal' isPlayer (fichasSeguidas+1) fs
        else
            checkHorizontal' isPlayer 0 fs
        
    |otherwise = False