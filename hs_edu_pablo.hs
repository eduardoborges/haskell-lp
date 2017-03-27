-- Exercícios de Haskell
-- Disciplina: COMP0209 - Linguagens de Programação para Sistemas de Informação 

-- @author: Eduardo Borges <eduardo.borges@dcomp.ufs.br>
-- @author: Pablo Lima <pablo.lima@dcomp.ufs.br>
-- @date: 26/03/17


-- |--------  QUESTÃO 1 --------------------------------------------------------------------|
-- | Escreva uma função em Haskell para calcular o mínimo múltiplo comum de três números    |
-- | inteiros positivos.                                                                    |
-- |----------------------------------------------------------------------------------------|

calc_mmc            :: Int->Int->Int
calc_mmc x y        | (x == y) = x
                    | (x == 0) || (y == 0) = 0
                    | otherwise = div (x * y) (calc_mdc x y)

calc_mmc_3num       :: Int->Int->Int->Int
calc_mmc_3num x y z | (x == 0)  = calc_mmc y z
                    | (y == 0)  = calc_mmc x z
                    | (z == 0)  = calc_mmc x y
                    | otherwise = calc_mmc x (calc_mmc y z)

-- |--------  QUESTÃO 2 ---------------------------------------------------------------------|
-- | Escreva uma função em Haskell para calcular o máximo divisor comum dos elementos de uma |
-- | lista de inteiros positivos.                                                            |
-- |-----------------------------------------------------------------------------------------|

calc_mdc            :: Int->Int->Int
calc_mdc x y        | (x < y)   = calc_mdc y x
                    | (x == y)  = x
                    | (y == 0)  = x
                    | (x == 0)  = y
                    | otherwise = calc_mdc y (mod x y)

calc_mdc_3num       :: Int->Int->Int->Int
calc_mdc_3num x y z = calc_mdc x (calc_mdc y z) 

-- |--------  QUESTÃO 3 --------------------------------------------------------------------------------------|
-- | Escreva uma função em Haskell para retornar True caso ocorra ataque entre, pelo menos duas               | 
-- | rainhas quaisquer, dentre oito posicionadas em um tabuleiro de xadrez. A posição de cada rainha é        |
-- | representada por uma dupla de inteiros representando a linha e coluna. A entrada da função é uma lista   |
-- | de duplas de inteiros de comprimento igual a oito.                                                       | 
-- |----------------------------------------------------------------------------------------------------------|

-- NAO CONSEGUIMOS :(

-- |--------  QUESTÃO 4 ------------------------------------------------------------------------------------|                                                                                  |
-- | Ler duas strings e imprimir a posição em que a primeira ocorra na segunda, ou -1, caso a               |
-- | primeira não seja substring da segunda                                                     	        |
-- |--------------------------------------------------------------------------------------------------------|

substr ::String->String->Int->Int
substr toFind (x:toCompare) position  | toCompare == "" = -1
                                      | toFind == take (length toFind) (x:toCompare) = position
                                      | otherwise = substr toFind toCompare (position + 1)

-- essa parada pra fazer o I/O (prof que fez)
strstr :: IO ()
strstr = do putStrLn "Entre com a primeira string" 
            s1 <- getLine
            putStrLn "Entre com a segunda string" 
            s2 <- getLine
            putStrLn (show (substr s1 s2 0))

-- |--------  QUESTÃO 5 --------------------------------------------------------------------------------------|
-- | Escreva uma função para que, quando usada com a função map aplicada a uma lista de números, retorne uma  |
-- | lista em que cada elemento é levado ao denominador da fração 2/x.                                        |   
-- | Ex.                                                                                                      |
-- | map 2_sob_x [1..10]                                                                                      |
-- | [2.0,1.0,0.6666666666666666,0.5,0.4,0.3333333333333333,0.2857142857142857,0.25,0.2222222222222222,0.2]   |                                                | 
-- |----------------------------------------------------------------------------------------------------------|

fn_map_under_x            :: Float -> Float
fn_map_under_x item_to_map = 2/item_to_map