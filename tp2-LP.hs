-- Nome Matheus
-- Nome Gabriel Duque Schiffner Oliveira - 201965033a

import System.Random
import System.IO


type Posicao = (Int, Int)
type CampoMinado = [[Bool]]

-- Funcao que verifica se a quantidade de minas é valida para o tabuleiro gerado
--quantMinas :: Int -> Int -> Bool
--quantMinas tamanhoTabuleiro quantidadeMinas = if quantidadeMinas <= 0
       --                            then putStrLn ("Quantidade inválida, valor abaixo do permitido") 
        --                                  False
        --                         else if quantidadeMinas >= (tamanhoTabuleiro/2)
        --                                 then putStrLn ("Quantidade inválida, valor acima do permitido") 
         --                                 False
         --                              else  True



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
--posicaoValida :: CampoMinado -> Posicao -> Bool
--posicaoValida campoMinado (x, y) =
  --x >= 0 && x < length campoMinado && y >= 0 && y < length (head campoMinado)

-- Função para contar o número de minas ao redor de uma posição
--contarMinasAoRedor :: CampoMinado -> Posicao -> Int
--contarMinasAoRedor campoMinado (x, y) =
--  length $ filter (== True) $ map (campoMinado !!) posicoesVizinhas
  --where
    --posicoesVizinhas =
      --[ (x - 1, y),
        --(x, y - 1),
        --(x, y + 1),
        --(x + 1, y)
      --]
    --posicoesValidas = filter (posicaoValida campoMinado) posicoesVizinhas

-- Funcao das jogadas

--jogadaCampoMinado :: CampoMinado -> [Posicao] -> Char -> CampoMinado
--jogadaCampoMinado campoMinadoGerado posicaoJogada tipoJogada =
 --   | tipoJogada == +_
 --   | tipoJogada == -_
 --   | Otherwise = 

-- Funcao principal para selecionar o tamanho do tabuleiro, quantidade de minas, receber os dados e comandos de menu

main :: IO ()
main = do
  -- Quantidade de linhas do tabuleiro
  putStrLn "Informe a quantidade de linhas do tabuleiro"
  quantLin <- getLine
  let quantLinhas = read quantLin :: Int
  
  -- Verifica se a quantidade de linhas é válida
  let linhasValidas = quantLinhas > 0
  if not linhasValidas then putStrLn "Quantidade de linhas inválida!"
  else do
    -- Quantidade de colunas do tabuleiro
    putStrLn "Informe a quantidade de colunas do tabuleiro"
    quantCol <- getLine
    let quantColunas = read quantCol :: Int

    -- Verifica se a quantidade de colunas é válida
    let colunasValidas = quantColunas > 0
    if not colunasValidas then putStrLn "Quantidade de colunas inválida!"
    else do
      -- Quantidade de minas
      putStrLn ("Informe a quantidade de minas entre 1 e " ++ show ((quantLinhas * quantColunas) `div` 2))
      quantMinas <- getLine
      let quantidadeMinas = read quantMinas :: Int

      -- Verifica se a quantidade de minas é válida
      let minasValidas = quantidadeMinas > 0 && quantidadeMinas < (quantLinhas * quantColunas) `div` 2
      if not minasValidas then putStrLn "Quantidade de minas inválida!"
      else do
        -- Gera posições aleatórias para as minas
        gen <- newStdGen
        let posicaoMinas = geraPosicaoMinas quantLinhas quantColunas quantidadeMinas gen

        -- Cria o campo minado vazio
        let campoMinado = criaTabuleiro quantLinhas quantColunas

        -- Insere as minas no campo minado
        let campoMinadoComMinas = insereMinas campoMinado posicaoMinas

        -- Imprime o campo minado
        printCampoMinado campoMinadoComMinas



