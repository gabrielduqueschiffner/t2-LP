import System.Random
import System.IO

type Posicao = (Int, Int)
type CampoMinado = [[Bool]]

-- Função para criar um campo minado vazio com um tamanho específico
criaTabuleiro :: Int -> Int -> CampoMinado
criaTabuleiro quantLinhas quantColunas = replicate quantLinhas (replicate quantColunas False)

-- Função para gerar posições aleatórias para as minas
geraPosicaoMinas :: Int -> Int -> Int -> StdGen -> [Posicao]
geraPosicaoMinas quantLinhas quantColunas quantidadeMinas gen =
  take quantidadeMinas $ randomPositions gen
  where
    indices = [(x, y) | x <- [0..quantLinhas-1], y <- [0..quantColunas-1]]
    randomPositions = shuffle indices

-- Função para embaralhar uma lista usando um gerador de números aleatórios
shuffle :: RandomGen g => [a] -> g -> [a]
shuffle [] _ = []
shuffle lst gen =
  let (index, newGen) = randomR (0, length lst - 1) gen
      (rest, (selected:remaining)) = splitAt index lst
  in selected : shuffle (rest ++ remaining) newGen

-- Função para colocar as minas no campo minado
insereMinas :: CampoMinado -> [Posicao] -> CampoMinado
insereMinas campoMinadoGerado posicaoMinas =
  foldl (\mf (x, y) -> updateElement mf x y True) campoMinadoGerado posicaoMinas

-- Função para atualizar um elemento em uma posição específica na matriz
updateElement :: [[a]] -> Int -> Int -> a -> [[a]]
updateElement matrix rowIndex colIndex newValue =
  take rowIndex matrix ++
  [take colIndex (matrix !! rowIndex) ++ [newValue] ++ drop (colIndex + 1) (matrix !! rowIndex)] ++
  drop (rowIndex + 1) matrix

-- Função para exibir o campo minado no console
printCampoMinado :: CampoMinado -> IO ()
printCampoMinado campoMinadoGerado = mapM_ (putStrLn . unwords . map showRow) campoMinadoGerado
  where
    showRow True = "X"
    showRow False = "_"

-- Função para verificar se uma posição é válida no campo minado
posicaoValida :: CampoMinado -> Posicao -> Bool
posicaoValida campoMinado (x, y) =
  x >= 0 && x < length campoMinado && y >= 0 && y < length (head campoMinado)

-- Função para contar o número de minas ao redor de uma posição
contarMinasAoRedor :: CampoMinado -> Posicao -> Int
contarMinasAoRedor campoMinado (x, y) =
  length $ filter (== True) $ map (\(a, b) -> campoMinado !! a !! b) posicoesVizinhas
  where
    posicoesVizinhas =
      [ (x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
        (x, y - 1),                 (x, y + 1),
        (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)
      ]

-- Função para fazer a jogada do jogador
fazerJogada :: CampoMinado -> Posicao -> CampoMinado
fazerJogada campoMinado posicao@(x, y)
  | not (posicaoValida campoMinado posicao) || campoMinado !! x !! y = campoMinado -- Posição inválida ou já aberta
  | otherwise = abrirPosicao campoMinado posicao






-- Função para abrir uma posição no campo minado
abrirPosicao :: CampoMinado -> Posicao -> CampoMinado
abrirPosicao campoMinado (x, y)
  | not (posicaoValida campoMinado (x, y)) || campoMinado !! x !! y = campoMinado -- Posição inválida ou já aberta
  | contarMinasAoRedor campoMinado (x, y) > 0 = updateElement campoMinado x y False -- Posição aberta (não vazia)
  | otherwise = abrirVizinhas (updateElement campoMinado x y False) (x, y)


-- Função para abrir as posições vizinhas recursivamente
abrirVizinhas :: CampoMinado -> Posicao -> CampoMinado
abrirVizinhas campoMinado posicao@(x, y)
  | not (posicaoValida campoMinado posicao) || campoMinado !! x !! y = campoMinado -- Posição inválida ou já aberta
  | contarMinasAoRedor campoMinado posicao > 0 = updateElement campoMinado x y False -- Posição aberta (não vazia)
  | otherwise =
      let vizinhas = filter (posicaoValida campoMinado) $
                     [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
                      (x, y - 1), (x, y + 1),
                      (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
          campoMinadoAtualizado = updateElement campoMinado x y False
          campoMinadoAberto = foldl abrirPosicao campoMinadoAtualizado vizinhas
      in if campoMinadoAtualizado == campoMinadoAberto
           then campoMinadoAtualizado
           else abrirVizinhas campoMinadoAberto posicao








-- Função para verificar se todas as posições sem minas foram abertas
todasPosicoesAbertas :: CampoMinado -> Bool
todasPosicoesAbertas campoMinado =
  all (notElem True) campoMinado

-- Função para ler a posição escolhida pelo jogador
lerPosicao :: IO Posicao
lerPosicao = do
  putStrLn "Informe a posição (linha e coluna) que deseja abrir:"
  posicao <- getLine
  let [x, y] = map read (words posicao) :: [Int]
  return (x - 1, y - 1)


-- Função para realizar as jogadas até que o jogo acabe ou seja ganho
jogar :: CampoMinado -> IO ()
jogar campoMinado = do
  printCampoMinado campoMinado
  posicao <- lerPosicao
  let novoCampoMinado = fazerJogada campoMinado posicao
  if todasPosicoesAbertas novoCampoMinado
    then putStrLn "Parabéns, você ganhou!"
    else if novoCampoMinado == campoMinado && campoMinado !! fst posicao !! snd posicao
           then putStrLn "Boom! Você perdeu!"
           else jogar novoCampoMinado
           

-- Função principal para selecionar o tamanho do tabuleiro, quantidade de minas, receber os dados e comandos de menu
main :: IO ()
main = do
  -- Quantidade de linhas do tabuleiro
  putStrLn "Informe a quantidade de linhas do tabuleiro:"
  linhas <- getLine
  let quantLinhas = read linhas :: Int

  -- Quantidade de colunas do tabuleiro
  putStrLn "Informe a quantidade de colunas do tabuleiro:"
  colunas <- getLine
  let quantColunas = read colunas :: Int

  -- Quantidade de minas
  putStrLn "Informe a quantidade de minas:"
  minas <- getLine
  let quantidadeMinas = read minas :: Int

  -- Geração do campo minado com as minas
  gen <- newStdGen
  let posicaoMinas = geraPosicaoMinas quantLinhas quantColunas quantidadeMinas gen
      campoMinadoGerado = insereMinas (criaTabuleiro quantLinhas quantColunas) posicaoMinas

  -- Início do jogo
  jogar campoMinadoGerado
