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
  mapM_ (putStrLn . unwords . map mostrarPosicao) campoMinado
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
    posicoesAleatoriasIndices = take quantidadeMinas (randomRs (0, length indices - 1) gen)
    posicoesAleatorias = map (indices !!) posicoesAleatoriasIndices

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
  length $ filter (== Just True) [campoMinado !! i !! j | i <- [max 0 (x - 1) .. min (x + 1) (length campoMinado - 1)], j <- [max 0 (y - 1) .. min (y + 1) (length (head campoMinado) - 1)], posicaoValida campoMinado (i, j)]

-- Função para verificar se uma posição é válida no campo minado
posicaoValida :: CampoMinado -> Posicao -> Bool
posicaoValida campoMinado (x, y) =
  x >= 0 && x < length campoMinado && y >= 0 && y < length (head campoMinado)

-- Função para abrir uma posição no campo minado
abrirPosicao :: CampoMinado -> Posicao -> CampoMinado
abrirPosicao campoMinado posicao@(x, y)
  | not (posicaoValida campoMinado posicao) = campoMinado
  | campoMinado !! x !! y /= Nothing = campoMinado
  | minasAoRedor > 0 = updateElement campoMinado posicao (Just False)
  | otherwise = abrirPosicoesVizinhas novoCampoMinado posicao
  where
    minasAoRedor = contarMinasAoRedor campoMinado posicao
    novoCampoMinado = updateElement campoMinado posicao (Just False)

-- Função para abrir as posições vizinhas de forma recursiva
abrirPosicoesVizinhas :: CampoMinado -> Posicao -> CampoMinado
abrirPosicoesVizinhas campoMinado (x, y)
  | not (posicaoValida campoMinado (x, y)) = campoMinado
  | campoMinado !! x !! y /= Nothing = campoMinado
  | minasAoRedor > 0 = updateElement campoAtualizado (x, y) (Just False)
  | otherwise = abrirPosicoesVizinhas campoAtualizado (x, y)
  where
    minasAoRedor = contarMinasAoRedor campoMinado (x, y)
    campoAtualizado = foldl abrirPosicoesVizinhas (updateElement campoMinado (x, y) (Just False)) vizinhos
    vizinhos = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], posicaoValida campoMinado (i, j)]

-- Função para fazer uma jogada no campo minado
fazerJogada :: CampoMinado -> Posicao -> CampoMinado
fazerJogada campoMinado posicao =
  abrirPosicao campoMinado posicao

-- Função para verificar se todas as posições não minadas foram abertas
todasPosicoesAbertas :: CampoMinado -> Bool
todasPosicoesAbertas campoMinado =
  all (\linha -> all (\posicao -> posicao /= Nothing && posicao /= Just True) linha) campoMinado


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
  putStrLn "Bem-vindo ao Campo Minado!"
  putStrLn "Informe as dimensões do tabuleiro (linhas e colunas):"
  dimensoesStr <- getLine
  case words dimensoesStr of
    [linhasStr, colunasStr] ->
      case (readMaybe linhasStr, readMaybe colunasStr) of
        (Just linhas, Just colunas) -> do
          putStrLn "Informe a quantidade de minas:"
          minasStr <- getLine
          case readMaybe minasStr of
            Just minas -> do
              gen <- getStdGen
              let posicoesMinas = geraPosicaoMinas linhas colunas minas gen
                  campoMinado = insereMinas (criaTabuleiro linhas colunas) posicoesMinas
              jogar campoMinado
            Nothing -> putStrLn "Quantidade de minas inválida!"
        _ -> putStrLn "Dimensões inválidas!"
    _ -> putStrLn "Entrada inválida!"
