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
    play b True


ponerFicha :: [[[Char]]] -> Int -> Bool -> [[[Char]]]
ponerFicha (f:fs) pos player 
    | f !! (pos - 1) == "_" = (replaceB f True pos) : fs
    | f !! (pos - 1) == "X" || f !! (pos -1) == "O" = f : (ponerFicha fs pos True)
    | otherwise = [f]


replaceB (f:fs) player pos
    | pos == 1 = "X" : fs
    | otherwise = f : replaceB fs player (pos-1)


play b t  = do 
    if t then
        do 
        putStrLn ("Elige donde quieres poner la ficha entre el 1 y el " ++ show (length  (head b)))
        pos <- getLine
        -- print (head b)
        let nboard = ponerFicha b (read pos :: Int) True
        muestraBoard nboard
        play nboard True
    else
        return()