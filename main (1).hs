
somar :: [Int] -> Int
somar [] = 0
somar (x:xs) = x + somar xs

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (_:x) =1+ tamanho x

av :: Int -> Int -> Int
av x y = x `div` y

menor :: [Int] -> Int
menor [] = 0
menor [x] = x
menor (x:y) = let m = menor y in if x < m then x else m

maior :: [Int] -> Int 
maior [] = 0
maior [x] = x
maior (x:y) = let m = maior y in if x > m then x else m

main :: IO ()
main = do
    putStrLn "Digite os números (separados por espaços):"
    input <- getLine
    let lista = map read (words input) :: [Int]
    
    let tam = tamanho lista
    putStrLn ("tamanho:" ++ show tam)
    
    
    let soma = somar lista
    putStrLn ("soma:" ++ show soma)
    
    print(av soma tam)
    
    let menorr = menor lista
    putStrLn ("menor:" ++ show menorr)
    
    let maiorr = maior lista
    putStrLn ("maior" ++ show maiorr)
