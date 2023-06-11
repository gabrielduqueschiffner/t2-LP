import System.Random
import Data.List (intercalate)
import Text.ParserCombinators.Parsec

data Celula = Celula { marcado :: Bool, bombado :: Bool, exibida :: Bool, closeBombs :: Int } deriving (Show)

type Coordenada = (Int, Int)
type Matriz = [[Celula]]

-- Function to create an NxN matrix of unmarked, unbombed, and unexposed cells
createMatriz :: Int -> Matriz
createMatriz n = replicate n (replicate n (Celula False False False 0))

-- Function to generate a random coordinate between 0 and N. This function also
-- checks if there is already a bomb at the generated position.
gerarCoordenadaAleatoria :: Int -> IO Coordenada
gerarCoordenadaAleatoria n = do
  linha <- randomRIO (0, n-1)
  coluna <- randomRIO (0, n-1)
  return (linha, coluna)

-- Function to generate and mark four random coordinates. This function also
-- checks if there is already a bomb at the generated position. If so, it calls itself again to generate a new valid coordinate for bomb placement.
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

-- Function to place a bomb at a coordinate
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

-- Function to print the matrix and its bombs
imprimirMatriz :: Int -> Matriz -> IO ()
imprimirMatriz n matriz = do
  mapM_ (imprimirColuna n) [0..n-1]
  where
    imprimirColuna :: Int -> Int -> IO ()
    imprimirColuna n coluna = do
      mapM_ (imprimirCelula coluna) [0..n-1]
      putStrLn ""

    imprimirCelula :: Int -> Int -> IO ()
    imprimirCelula coluna linha = do
      let celula = matriz !! linha !! coluna
      if bombado celula
        then putStr "B"
        else if marcado celula
               then putStr "M"
               else putStr "."

-- Function to convert the user input string into a coordinate
converterCoordenada :: String -> Coordenada
converterCoordenada entrada =
  case parse coordenadaParser "" entrada of
    Left _ -> (-1, -1)
    Right (coluna, linha) -> (linha, coluna)
  where
    coordenadaParser :: Parser (Int, Int)
    coordenadaParser = do
      coluna <- letraParaNumero <$> upper
      linha <- read <$> many1 digit
      return (coluna, linha)

    letraParaNumero :: Char -> Int
    letraParaNumero letra = fromEnum letra - fromEnum 'A'

-- Function to update the matrix after a move
atualizarMatrizJogada :: Coordenada -> Matriz -> Matriz
atualizarMatrizJogada (linha, coluna) matriz = 
  let celulaAtualizada = (matriz !! linha !! coluna) { exibida = True }
  in atualizarLinha coluna (atualizarMatriz linha celulaAtualizada (matriz !! linha)) matriz
  where
    atualizarLinha :: Int -> [Celula] -> Matriz -> Matriz
    atualizarLinha _ _ [] = []
    atualizarLinha coluna linhaAtualizada (linhaAtual : linhas)
      | coluna == 0 = linhaAtualizada : linhas
      | otherwise = linhaAtual : atualizarLinha (coluna - 1) linhaAtualizada linhas

    atualizarMatriz :: Int -> Celula -> [Celula] -> [Celula]
    atualizarMatriz _ _ [] = []
    atualizarMatriz coluna celulaAtualizada (celula : celulas)
      | coluna == 0 = celulaAtualizada : celulas
      | otherwise = celula : atualizarMatriz (coluna - 1) celulaAtualizada celulas

-- Function to perform a move
jogada :: Int -> Matriz -> IO Matriz
jogada n matriz = do
  putStrLn "Digite a coordenada da jogada (Ex: A1):"
  entrada <- getLine
  let coordenada = converterCoordenada entrada
  if coordenada == (-1, -1) || coordenadaValida n coordenada (matriz !! fst coordenada !! snd coordenada)
    then do
      putStrLn "Coordenada inválida! Tente novamente."
      jogada n matriz
    else do
      let matrizAtualizada = atualizarMatrizJogada coordenada matriz
      imprimirMatriz n matrizAtualizada
      return matrizAtualizada

-- Function to check if a coordinate is valid
coordenadaValida :: Int -> Coordenada -> Celula -> Bool
coordenadaValida n (linha, coluna) celula =
  linha >= 0 && linha < n && coluna >= 0 && coluna < n && not (exibida celula)

-- Function to play the game
jogar :: Int -> Matriz -> IO ()
jogar n matriz = do
  imprimirMatriz n matriz
  matrizAtualizada <- jogada n matriz
  if gameOver matrizAtualizada
    then putStrLn "Game Over!"
    else if vitoria matrizAtualizada
           then putStrLn "Você venceu!"
           else jogar n matrizAtualizada

-- Function to check if the game is over (if a bomb is exposed)
gameOver :: Matriz -> Bool
gameOver matriz = any (\linha -> any (\celula -> bombado celula && exibida celula) linha) matriz

-- Function to check if the player has won (all non-bomb cells are exposed)
vitoria :: Matriz -> Bool
vitoria matriz = all (\linha -> all (\celula -> exibida celula || bombado celula) linha) matriz

-- Function to start the game
main :: IO ()
main = do
  putStrLn "Digite o tamanho da matriz:"
  n <- readLn
  matriz <- gerarEMarcarCoordenadas n 4 (createMatriz n)
  jogar n matriz
