import System.Random
import Text.Read

type CampoMinado = [[Maybe Int]]
type Posicao = (Int, Int)

-- Função para criar um tabuleiro vazio com as dimensões especificadas
criaTabuleiro :: Int -> Int -> CampoMinado
criaTabuleiro linhas colunas = replicate linhas (replicate colunas Nothing)

-- Função para verificar se uma posição é válida dentro do tabuleiro
posicaoValida :: CampoMinado -> Posicao -> Bool
posicaoValida campoMinado (x, y) =
  x >= 0 && x < length campoMinado && y >= 0 && y < length (head campoMinado)

-- Função para inserir as minas no tabuleiro
insereMinas :: CampoMinado -> [Posicao] -> CampoMinado
insereMinas campoMinado posicoesMinas =
  foldl (\campo p -> updateElement campo p (Just (-1))) campoMinado posicoesMinas

-- Função para atualizar um elemento em uma determinada posição do tabuleiro
updateElement :: CampoMinado -> Posicao -> Maybe Int -> CampoMinado
updateElement campoMinado (x, y) elemento =
  take x campoMinado
    ++ [take y (campoMinado !! x) ++ [elemento] ++ drop (y + 1) (campoMinado !! x)]
    ++ drop (x + 1) campoMinado

-- Função para gerar posições aleatórias para as minas
geraPosicaoMinas :: Int -> Int -> Int -> StdGen -> [Posicao]
geraPosicaoMinas linhas colunas minas gen =
  take minas $ removeDuplicates $ zip (randomRs (0, linhas - 1) gen) (randomRs (0, colunas - 1) gen)

-- Função para remover elementos duplicados de uma lista
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- Função para contar o número de minas ao redor de uma determinada posição
contarMinasAoRedor :: CampoMinado -> Posicao -> Int
contarMinasAoRedor campoMinado (x, y) =
  length $
    filter (\(i, j) -> posicaoValida campoMinado (i, j) && campoMinado !! i !! j == Just (-1)) $
      [(i, j) | i <- [max 0 (x - 1) .. min (x + 1) (length campoMinado - 1)], j <- [max 0 (y - 1) .. min (y + 1) (length (head campoMinado) - 1)]]

-- Função para exibir o campo minado na tela
printCampoMinado :: CampoMinado -> IO ()
printCampoMinado campoMinado = do
  putStrLn "Campo Minado:"
  mapM_ printLinha campoMinado
  where
    printLinha linha = do
      let linhaStr = map (maybe "*" show) linha
      putStrLn (unwords linhaStr)

-- Função para jogar o campo minado
jogar :: CampoMinado -> IO ()
jogar campoMinado = do
  printCampoMinado campoMinado
  putStrLn "Informe a posição (linha e coluna) que deseja abrir:"
  posicaoStr <- getLine
  case parsePosicao posicaoStr of
    Just posicao ->
      if posicaoValida campoMinado posicao
        then do
          let minasAoRedor = contarMinasAoRedor campoMinado posicao
          if minasAoRedor == -1
            then do
              putStrLn "Boom! Você perdeu!"
            else do
              let campoMinadoAtualizado = updateElement campoMinado posicao (Just minasAoRedor)
              jogar campoMinadoAtualizado
        else do
          putStrLn "Posição inválida. Por favor, tente novamente."
          jogar campoMinado
    Nothing -> do
      putStrLn "Posição inválida. Por favor, tente novamente."
      jogar campoMinado

-- Função para fazer o parsing da posição informada pelo jogador
parsePosicao :: String -> Maybe Posicao
parsePosicao str =
  case words str of
    [linha, coluna] -> do
      l <- readMaybe linha
      c <- readMaybe coluna
      Just (l, c)
    _ -> Nothing

-- Função para fazer o parsing das dimensões do tabuleiro
parseDimensoes :: String -> Maybe (Int, Int)
parseDimensoes str =
  case words str of
    [linhas, colunas] -> do
      l <- readMaybe linhas
      c <- readMaybe colunas
      Just (l, c)
    _ -> Nothing

-- Função principal
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Campo Minado!"
  putStrLn "Informe as dimensões do tabuleiro (linhas e colunas):"
  dimensoesStr <- getLine
  case parseDimensoes dimensoesStr of
    Just (linhas, colunas) -> do
      putStrLn "Informe a quantidade de minas:"
      minasStr <- getLine
      case readMaybe minasStr of
        Just minas -> do
          gen <- getStdGen
          let posicoesMinas = geraPosicaoMinas linhas colunas minas gen
              campoMinado = insereMinas (criaTabuleiro linhas colunas) posicoesMinas
          jogar campoMinado
        Nothing -> putStrLn "Quantidade inválida. O jogo será encerrado."
    Nothing -> putStrLn "Dimensões inválidas. O jogo será encerrado."
