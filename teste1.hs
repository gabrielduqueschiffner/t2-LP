import System.IO
import System.Random
import Text.Read (readMaybe)

type Posicao = (Int, Int)
type CampoMinado = [[Maybe Bool]]

-- Função para criar um tabuleiro vazio
criaTabuleiro :: Int -> Int -> CampoMinado
criaTabuleiro linhas colunas =
  replicate linhas (replicate colunas Nothing)

-- Função para imprimir o campo minado
printCampoMinado :: CampoMinado -> IO ()
printCampoMinado campoMinado =
  mapM_ putStrLn $ map (unwords . map mostrarPosicao) campoMinado
  where
    mostrarPosicao :: Maybe Bool -> String
    mostrarPosicao (Just True) = "X"
    mostrarPosicao (Just False) = "-"
    mostrarPosicao Nothing = "*"




-- Função para gerar uma lista de posições de minas aleatórias
geraPosicaoMinas :: Int -> Int -> Int -> StdGen -> [Posicao]
geraPosicaoMinas quantLinhas quantColunas quantidadeMinas gen =
  take quantidadeMinas posicoesAleatorias
  where
    indices = [(x, y) | x <- [0..quantLinhas-1], y <- [0..quantColunas-1]]
    posicoesAleatorias = map (indices !!) posicoesAleatoriasIndices
    posicoesAleatoriasIndices = take quantidadeMinas (randomRs (0, length indices - 1) gen)

-- Função para inserir minas no campo minado
insereMinas :: CampoMinado -> [Posicao] -> CampoMinado
insereMinas campoMinado posicoesMinas =
  foldl (\campoMinado' posicao -> updateElement campoMinado' posicao (Just True)) campoMinado posicoesMinas

-- Função para atualizar um elemento em uma posição do campo minado
updateElement :: CampoMinado -> Posicao -> Maybe Bool -> CampoMinado
updateElement campoMinado (x, y) novoValor =
  take x campoMinado ++
  [take y (campoMinado !! x) ++ [novoValor] ++ drop (y + 1) (campoMinado !! x)] ++
  drop (x + 1) campoMinado

-- Função para contar as minas ao redor de uma posição
contarMinasAoRedor :: CampoMinado -> Posicao -> Int
contarMinasAoRedor campoMinado (x, y) =
  length $ filter (== Just True) [campoMinado !! i !! j | i <- [x - 1 .. x + 1], j <- [y - 1 .. y + 1], posicaoValida campoMinado (i, j)]

-- Função para verificar se uma posição é válida no campo minado
posicaoValida :: CampoMinado -> Posicao -> Bool
posicaoValida campoMinado (x, y) =
  x >= 0 && x < length campoMinado && y >= 0 && y < length (head campoMinado)

-- Função para abrir uma posição no campo minado
abrirPosicao :: CampoMinado -> Posicao -> CampoMinado
abrirPosicao campoMinado posicao@(x, y)
  | not (posicaoValida campoMinado posicao) = campoMinado
  | campoMinado !! x !! y /= Nothing = campoMinado
  | minasAoRedor > 0 = updateElement campoMinado posicao (Just True)
  | otherwise = abrirPosicoesVizinhas campoMinado posicao
  where
    minasAoRedor = contarMinasAoRedor campoMinado posicao

-- Função para abrir posições vizinhas recursivamente
abrirPosicoesVizinhas :: CampoMinado -> Posicao -> CampoMinado
abrirPosicoesVizinhas campoMinado posicao@(x, y)
  | not (posicaoValida campoMinado posicao) = campoMinado
  | campoMinado !! x !! y /= Nothing = campoMinado
  | minasAoRedor > 0 = updateElement campoMinado posicao (Just True)
  | otherwise = foldl abrirPosicoesVizinhas (updateElement campoMinado posicao (Just True)) posicoesVizinhas
  where
    minasAoRedor = contarMinasAoRedor campoMinado posicao
    posicoesVizinhas = filter (posicaoValida campoMinado) [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], (i, j) /= posicao]

-- Função para fazer uma jogada no campo minado
fazerJogada :: CampoMinado -> Posicao -> CampoMinado
fazerJogada campoMinado posicao =
  abrirPosicao campoMinado posicao

-- Função para verificar se todas as posições não minadas foram abertas
todasPosicoesAbertas :: CampoMinado -> Bool
todasPosicoesAbertas campoMinado =
  all (all (== Just True)) campoMinado


-- Função principal para jogar o Campo Minado
jogar :: CampoMinado -> IO ()
jogar campoMinado = do
  printCampoMinado campoMinado
  putStrLn "Informe a posição (linha e coluna) que deseja abrir:"
  posicaoStr <- getLine
  case parsePosicao posicaoStr of
    Just posicao -> do
      let novoCampoMinado = fazerJogada campoMinado posicao
      if todasPosicoesAbertas novoCampoMinado
        then putStrLn "Parabéns, você ganhou!"
        else if novoCampoMinado == campoMinado
               then putStrLn "Boom! Você perdeu!"
               else jogar novoCampoMinado
    Nothing -> do
      putStrLn "Posição inválida!"
      jogar campoMinado

-- Função auxiliar para fazer o parse da posição
parsePosicao :: String -> Maybe Posicao
parsePosicao str =
  case words str of
    [linhaStr, colunaStr] ->
      case (readMaybe linhaStr, readMaybe colunaStr) of
        (Just linha, Just coluna) -> Just (linha - 1, coluna - 1)
        _ -> Nothing
    _ -> Nothing

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Informe a quantidade de linhas do tabuleiro:"
  linhasStr <- getLine
  putStrLn "Informe a quantidade de colunas do tabuleiro:"
  colunasStr <- getLine
  putStrLn "Informe a quantidade de minas:"
  minasStr <- getLine
  case (readMaybe linhasStr, readMaybe colunasStr, readMaybe minasStr) of
    (Just quantLinhas, Just quantColunas, Just quantMinas) ->
      if quantLinhas > 0 && quantColunas > 0 && quantMinas > 0 && quantMinas <= quantLinhas * quantColunas
        then do
          gen <- getStdGen
          let posicoesMinas = geraPosicaoMinas quantLinhas quantColunas quantMinas gen
              campoMinado = insereMinas (criaTabuleiro quantLinhas quantColunas) posicoesMinas
          jogar campoMinado
        else putStrLn "Configuração inválida!"
    _ -> putStrLn "Entrada inválida!"
