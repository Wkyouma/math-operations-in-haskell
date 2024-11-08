data Arvore a = Nulo |
                Leaf a |
                Branch a (Arvore a) (Arvore a)
    deriving Show

-- Função emOrdem para percorrer a árvore
emOrdem :: Arvore a -> [a]
emOrdem (Branch x l r) = emOrdem l ++ [x] ++ emOrdem r
emOrdem (Leaf x) = [x]
emOrdem Nulo = []

-- Função insere para inserir um elemento na árvore
insere :: (Ord a) => Arvore a -> a -> Arvore a
insere (Branch x l r) y
    | y > x     = Branch x l (insere r y)
    | y < x     = Branch x (insere l y) r
    | otherwise = Branch x l r  -- Caso y seja igual a x, não insere duplicado
insere (Leaf x) y
    | y > x     = Branch x Nulo (Leaf y)
    | y < x     = Branch x (Leaf y) Nulo
    | otherwise = Leaf x  -- Não insere duplicado
insere Nulo y = Leaf y

-- Função main para testar
main :: IO()
main = do
    let arvore = Branch 12 (Branch 4 (Leaf 2) (Branch 8 (Leaf 6) Nulo)) (Leaf 16)
    print(emOrdem arvore)
    let arvoreAtualizada = insere arvore 15  
    print (emOrdem arvoreAtualizada)
