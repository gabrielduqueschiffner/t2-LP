import System.Random (randomRIO)

type Posicao = (Int, Int)
type Tabuleiro = [[Char]]
type CampoMinado = [[Char]]

-- Função para criar um novo tabuleiro vazio
novoTabuleiro :: Int -> Int -> Tabuleiro
novoTabuleiro linhas colunas = replicate linhas (replicate colunas '#')

-- Função para imprimir o tabuleiro
imprimirTabuleiro :: Tabuleiro -> IO ()
imprimirTabuleiro tabuleiro = do
  putStrLn "  0123456789"
  putStrLn " ------------"
  putStr $ unlines $ zipWith (\i row -> show i ++ "|" ++ row) [0..] tabuleiro
  putStrLn " ------------"

-- Função para atualizar um elemento do tabuleiro
updateElement :: Tabuleiro -> Int -> Int -> Char -> Tabuleiro
updateElement tabuleiro x y valor =
  take y tabuleiro ++
  [take x (tabuleiro !! y) ++ [valor] ++ drop (x + 1) (tabuleiro !! y)] ++
  drop (y + 1) tabuleiro

-- Função para contar as minas adjacentes em uma posição
contarMinasAdjacentes :: CampoMinado -> Posicao -> Int
contarMinasAdjacentes campoMinado (x, y) =
  length $ filter (== '*') vizinhos
  where
    vizinhos = [campoMinado !! i !! j | i <- [y-1..y+1], j <- [x-1..x+1], i >= 0, i < length campoMinado, j >= 0, j < length (head campoMinado), (i, j) /= (y, x)]

-- Função para realizar uma jogada
realizarJogada :: CampoMinado -> Tabuleiro -> Posicao -> (CampoMinado, Tabuleiro)
realizarJogada campoMinado tabuleiro posicao@(x, y)
  | campoMinado !! y !! x == '*' = (campoMinado, novoTabuleiro) -- Mina encontrada, retorna o estado atual com a mina revelada
  | otherwise = (campoMinado, novoTabuleiro)
  where
    minasAoRedor = contarMinasAdjacentes campoMinado posicao
    novoTabuleiro = updateElement tabuleiro x y (head $ show minasAoRedor)

-- Função principal do jogo
jogarCampoMinado :: Int -> Int -> Int -> IO ()
jogarCampoMinado linhas colunas quantidadeMinas = do
  let tabuleiro = novoTabuleiro linhas colunas
  minas <- gerarMinasAleatorias linhas colunas quantidadeMinas
  let campoMinado = atualizarMinas tabuleiro minas
  loopJogo campoMinado tabuleiro

-- Função para gerar posições aleatórias para as minas
gerarMinasAleatorias :: Int -> Int -> Int -> IO [Posicao]
gerarMinasAleatorias linhas colunas quantidadeMinas = do
  let posicoes = [(x, y) | y <- [0..linhas-1], x <- [0..colunas-1]]
  indicesAleatorios <- gerarIndicesAleatorios quantidadeMinas posicoes []
  return $ map (\idx -> posicoes !! idx) indicesAleatorios

-- Função para gerar índices aleatórios sem repetição
gerarIndicesAleatorios :: Int -> [a] -> [Int] -> IO [Int]
gerarIndicesAleatorios 0 _ indices = return indices
gerarIndicesAleatorios quantidade lista indices = do
  indiceAleatorio <- randomRIO (0, length lista - 1)
  if indiceAleatorio `elem` indices
    then gerarIndicesAleatorios quantidade lista indices
    else gerarIndicesAleatorios (quantidade - 1) lista (indiceAleatorio : indices)

-- Função para atualizar as minas no tabuleiro
atualizarMinas :: Tabuleiro -> [Posicao] -> CampoMinado
atualizarMinas tabuleiro minas = foldr (\(x, y) campo -> updateElement campo x y '*') tabuleiro minas

-- Loop principal do jogo
loopJogo :: CampoMinado -> Tabuleiro -> IO ()
loopJogo campoMinado tabuleiro = do
  imprimirTabuleiro tabuleiro
  putStrLn "Informe a posição (linha e coluna) que deseja abrir:"
  posicaoStr <- getLine
  case readPosicao posicaoStr of
    Nothing -> do
      putStrLn "Posição inválida! Tente novamente."
      loopJogo campoMinado tabuleiro
    Just posicao -> do
      let (novoCampoMinado, novoTabuleiro) = realizarJogada campoMinado tabuleiro posicao
      if campoMinado == novoCampoMinado
        then putStrLn "Você encontrou uma mina! Fim de jogo."
        else if novoTabuleiro == tabuleiro
               then putStrLn "Essa posição já foi aberta. Tente novamente."
               else if venceuJogo novoTabuleiro
                      then do
                        imprimirTabuleiro novoTabuleiro
                        putStrLn "Parabéns! Você venceu o jogo!"
                      else loopJogo novoCampoMinado novoTabuleiro

-- Função para converter uma string de posição para o tipo Posicao
readPosicao :: String -> Maybe Posicao
readPosicao str =
  case words str of
    [x, y] -> do
      lin <- readMaybe x :: Maybe Int
      col <- readMaybe y :: Maybe Int
      return (lin, col)
    _ -> Nothing

-- Função para verificar se todas as posições válidas foram abertas
venceuJogo :: Tabuleiro -> Bool
venceuJogo = all (notElem '#')

-- Função principal
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Campo Minado!"
  putStrLn "Quantas linhas terá o tabuleiro?"
  linhasStr <- getLine
  putStrLn "Quantas colunas terá o tabuleiro?"
  colunasStr <- getLine
  putStrLn "Quantas minas terá o tabuleiro?"
  minasStr <- getLine
  case (readMaybe linhasStr :: Maybe Int, readMaybe colunasStr :: Maybe Int, readMaybe minasStr :: Maybe Int) of
    (Just linhas, Just colunas, Just minas) -> jogarCampoMinado linhas colunas minas
    _ -> putStrLn "Entrada inválida. Reinicie o jogo e tente novamente."
