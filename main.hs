
add :: Int -> Int -> Int
add x y = x + y

mult :: Int -> Int -> Int 
mult x y = x * y

sub :: Int -> Int -> Int
sub x y = x - y

divs :: Int -> Int -> Int
divs x y = x `div` y 

operacao :: Int -> Int -> Int -> IO ()
operacao n x y
    |n == 1 = print (add x y)
    |n == 2 = print (mult x y)
    |n == 3 = print (sub x y)
    |n == 4 = print (divs x y)
    |otherwise = putStrLn "Numero inválida"



main :: IO()
main = do
    putStrLn "digite (1) para soma"
    putStrLn "digite (2) para multiplicacao"
    putStrLn "digite (3) para subtracao"
    putStrLn "digite (4) para divisao"
    numero <- getLine
    let n = read numero :: Int
    putStrLn "digite o primeiro numero"
    n1 <- getLine
    let x = read n1 :: Int
    putStrLn "digite o segundo numero"
    n2 <- getLine
    let y = read n2 :: Int
    operacao n x y
    
    
    