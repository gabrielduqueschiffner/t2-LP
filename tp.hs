import System.Random
import Data.List (intercalate)

data Celula = Celula { marcado :: Bool, bombado :: Bool, exibida :: Bool, closeBombs :: Int } deriving (Show)

type Coordenada = (Int, Int)

type Matriz = [[Celula]]

-- Função para criar uma matriz NxN de células não marcadas, não bombadas e não exibidas
createMatriz :: Int -> Matriz
createMatriz n = replicate n (replicate n (Celula False False False 0))

-- Função para gerar uma coordenada aleatória entre 0 e N. Essa função também 
-- verifica se na determinada posição gerada já existe uma bomba.

gerarCoordenadaAleatoria :: Int -> IO Coordenada
gerarCoordenadaAleatoria n = do
  linha <- randomRIO (0, n-1)
  coluna <- randomRIO (0, n-1)
  return (linha, coluna)

-- Função para gerar e marcar quatro coordenadas aleatórias. Essa função também 
-- verifica se na determinada posição gerada já existe uma bomba.Caso sim, ela é 
-- chamada novamente para gerar uma nova coordenada válida para inserção da bomba.
gerarEMarcarCoordenadas :: Int -> Int -> Matriz -> IO Matriz
gerarEMarcarCoordenadas _ 0 matriz = return matriz
gerarEMarcarCoordenadas n count matriz = do
  coordenada <- gerarCoordenadaAleatoria n
  let celula = matriz !! fst coordenada !! snd coordenada
  if bombado celula
    then gerarEMarcarCoordenadas n count matriz
    else do
      let matrizAtualizada = atualizarBombado coordenada matriz
      gerarEMarcarCoordenadas n (count - 1) matrizAtualizada




-- Função para colocar bomba em uma coordenada
atualizarBombado :: Coordenada -> Matriz -> Matriz
atualizarBombado (linha, coluna) matriz = 
  let linhaAtualizada = atualizarLinha coluna (matriz !! linha)
  in atualizarMatriz linha linhaAtualizada matriz
  where
    atualizarLinha :: Int -> [Celula] -> [Celula]
    atualizarLinha _ [] = []
    atualizarLinha coluna (celula : celulas)
      | coluna == 0 = celula { bombado = True } : celulas
      | otherwise = celula : atualizarLinha (coluna - 1) celulas

    atualizarMatriz :: Int -> [Celula] -> Matriz -> Matriz
    atualizarMatriz _ _ [] = []
    atualizarMatriz linha linhaAtualizada (linhaAtual : linhas)
      | linha == 0 = linhaAtualizada : linhas
      | otherwise = linhaAtual : atualizarMatriz (linha - 1) linhaAtualizada linhas


-- Função para imprimir matriz e as respectivas bombas
imprimirMatriz :: Int -> Matriz -> IO ()
imprimirMatriz n matriz = do
  mapM_ (imprimirColuna n) [0..n-1]
  where
    imprimirColuna :: Int -> Int -> IO ()
    imprimirColuna n coluna = do
      mapM_ (imprimirCelula coluna) [0..n-1]
      putStrLn ""

    imprimirCelula :: Int -> Int -> IO ()
    imprimirCelula coluna linha
      | bombado (matriz !! linha !! coluna) = putStr "O "
      | otherwise = putStr "E "



-- Função para imprimir número de bombas próximas a todas as cédulas
imprimirBombasProximas :: Matriz -> IO ()
imprimirBombasProximas matriz = mapM_ imprimirCoordenada coordenadas
  where
    coordenadas = [(linha, coluna) | linha <- [0..length matriz - 1], coluna <- [0..length (head matriz) - 1]]

    imprimirCoordenada :: Coordenada -> IO ()
    imprimirCoordenada coordenada = do
      let numBombas = bombasProximas coordenada matriz
      putStrLn (show coordenada ++ " = " ++ show numBombas)

bombasProximas :: Coordenada -> Matriz -> Int
bombasProximas (linha, coluna) matriz = count
  where
    celula = matriz !! linha !! coluna
    vizinhos = [(linha + 1, coluna), (linha - 1, coluna), (linha, coluna + 1), (linha, coluna - 1)]
    validVizinhos = filter (validCoord matriz) vizinhos
    count = length $ filter (isBombado matriz) validVizinhos

    validCoord :: Matriz -> Coordenada -> Bool
    validCoord matriz (linha, coluna) =
      linha >= 0 && linha < length matriz && coluna >= 0 && coluna < length (head matriz)

    isBombado :: Matriz -> Coordenada -> Bool
    isBombado matriz (linha, coluna) = bombado (matriz !! linha !! coluna)

    isBomba :: Matriz -> Coordenada -> Bool
    isBomba matriz (linha, coluna) = isBombado matriz (linha, coluna) && (linha, coluna) /= (linha, coluna)
----------------------------------------------------------------------------------------------------------------	

-- Função para imprimir os vizinhos válidos
imprimirVizinhos :: Matriz -> IO ()
imprimirVizinhos matriz = do
  let coordenadas = [(linha, coluna) | linha <- [0..(length matriz - 1)], coluna <- [0..(length (head matriz) - 1)]]
  mapM_ (imprimirVizinhosCoord matriz) coordenadas

imprimirVizinhosCoord :: Matriz -> Coordenada -> IO ()
imprimirVizinhosCoord matriz coordenada = do
  let vizinhos = obterVizinhosValidos matriz coordenada
  putStr (show coordenada ++ " = ")
  putStrLn (intercalate "," (map show vizinhos))

obterVizinhosValidos :: Matriz -> Coordenada -> [Coordenada]
obterVizinhosValidos matriz (linha, coluna) = filter (coordValida matriz) vizinhos
  where
    vizinhos = [(linha + 1, coluna), (linha - 1, coluna), (linha, coluna + 1), (linha, coluna - 1)]

coordValida :: Matriz -> Coordenada -> Bool
coordValida matriz (linha, coluna) =
  linha >= 0 && linha < length matriz && coluna >= 0 && coluna < length (head matriz)
----------------------------------------------------------------------------------------------------------------	

-- Função principal que solicita ao usuário o valor de N e o número de bombas, e gera a matriz correspondente
main :: IO ()
main = do
  putStrLn "Digite o valor de N:"
  n <- readLn :: IO Int

  putStrLn "Digite o número de bombas:"
  numBombas <- readLn :: IO Int
  let maxBombs = div (n * n) 2

  validatedNumBombs <-
    case numBombas of
      n | n <= 0 -> do
        putStrLn "Valor inserido negativo ou zero, número de bombas: 1"
        return 1
      n | n > maxBombs -> do
        putStrLn ("Valor inserido acima do limite, número de bombas será o máximo permitido pelas restrições: " ++ show maxBombs)
        return maxBombs
      n -> do
        putStrLn ("Valor de bombas inserido corretamente. O valor de bombas será, então: " ++ show n)
        return n

  let matriz = createMatriz n
  matrizFinal <- gerarEMarcarCoordenadas n validatedNumBombs matriz
  imprimirMatriz n matrizFinal
  imprimirVizinhos matrizFinal
  imprimirBombasProximas matrizFinal