------MATHEUS PEDRO ZANCANELLA BARBOZA 202035005------------------------------
------GABRIEL ----------------------------------------------------------------

import System.Random
import Data.List (intercalate)
import Data.Char (ord,chr)


data Celula = Celula { marcado :: Bool, bombado :: Bool, exibida :: Bool, closeBombs :: Int } deriving (Show)

type Coordenada = (Int, Int)

type Matriz = [[Celula]]

-- Função para criar uma matriz NxN de células não marcadas, não bombadas e não exibidas
createMatriz :: Int -> Matriz
createMatriz n = replicate n (replicate n (Celula False False False 0))

-- Função para gerar uma coordenada aleatória entre 0 e N. Essa função também------------------------------------- 
-- verifica se na determinada posição gerada já existe uma bomba.-------------------------------------------------
gerarCoordenadaAleatoria :: Int -> IO Coordenada
gerarCoordenadaAleatoria n = do
  linha <- randomRIO (0, n-1)
  coluna <- randomRIO (0, n-1)
  return (linha, coluna)
------------------------------------------------------------------------------------------------------------------
-- Função para gerar e marcar quatro coordenadas aleatórias. Essa função também-----------------------------------
-- verifica se na determinada posição gerada já existe uma bomba.Caso sim, ela é----------------------------------
-- chamada novamente para gerar uma nova coordenada válida para inserção da bomba.--------------------------------
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
------------------------------------------------------------------------------------------------------------------



------------------------Função para colocar bomba em uma coordenada-----------------------------------------------
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
------------------------------------------------------------------------------------------------------------------

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
----------------------------------------------------------

-- Função para imprimir matriz com as respectivas bombas:
imprimirMatrizBombas :: Int -> Matriz -> IO ()
imprimirMatrizBombas n matriz = do
  putStrLn " " 
  mapM_ (imprimirColuna n) [0..n-1]
  where
    imprimirColuna :: Int -> Int -> IO ()
    imprimirColuna n coluna = do
      mapM_ (imprimirCelula coluna) [0..n-1]
      putStrLn ""

    imprimirCelula :: Int -> Int -> IO ()
    imprimirCelula coluna linha = do
      let celula = matriz !! linha !! coluna
      putStr $ show (closeBombs celula) ++ " "
----------------------------------------------------------


---Função para imprimir número de bombas próximas a todas as células--------------------------------------------
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


conteudoBombado :: Matriz -> Coordenada -> IO ()
conteudoBombado matriz (x, y) = do
  let celula = (matriz !! y) !! x
  putStrLn $ "Conteúdo da coordenada " ++ show (x, y) ++ ": " ++ show (bombado celula)
  
chamarConteudoBombado :: Matriz -> [Coordenada] -> IO ()
chamarConteudoBombado matriz [] = return ()  -- Caso base: lista vazia, encerra a função
chamarConteudoBombado matriz (coord:coords) = do
  conteudoBombado matriz coord
  chamarConteudoBombado matriz coords  -- Chamada recursiva para processar o restante da lista
  
contarTrues :: Matriz -> [Coordenada] -> Int
contarTrues _ [] = 0  -- Caso base: lista vazia, retorna 0
contarTrues matriz (coord:coords) =
  let celula = (matriz !! (snd coord)) !! (fst coord)
      truesRestantes = contarTrues matriz coords
  in
    if bombado celula
      then 1 + truesRestantes
      else truesRestantes
 
 
-----------------------------------------------------------------------------------------------------------------	
------Função utilizada durante o processo de desenvolvimento para imprimir os vizinhos válidos-------------------
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
 
 
contarTruesVizinhos :: Matriz -> Coordenada -> Int
contarTruesVizinhos matriz coord =
  let vizinhos = obterVizinhosValidos matriz coord
  in contarTrues matriz vizinhos

coordenadasMatriz :: Int -> [Coordenada]
coordenadasMatriz n = [(x, y) | x <- [0..n-1], y <- [0..n-1]]

----------------------------------------------------------------------------------------------------------------

printResultado :: Matriz -> (Coordenada, Int) -> Matriz
printResultado matriz ((x, y), resultado) =
  let celula = (matriz !! y) !! x
      celulaAtualizada = celula { closeBombs = resultado }
      linhaAtualizada = take x (matriz !! y) ++ [celulaAtualizada] ++ drop (x + 1) (matriz !! y)
  in take y matriz ++ [linhaAtualizada] ++ drop (y + 1) matriz


----------------------------------------------------------------------------------------------------------------	
----------------------------------Função utilizada para imprimir matriz durante o jogo--------------------------

imprimirMatrizJogo :: Matriz -> IO ()
imprimirMatrizJogo matriz = do
  let n = length matriz
  imprimirCabecalhos n
  imprimirMatrizAux n matriz

imprimirCabecalhos :: Int -> IO ()
imprimirCabecalhos n = do
  putStrLn ""
  putStrLn $ replicate 2 ' ' ++ cabecalhos n

cabecalhos :: Int -> String
cabecalhos n = unwords $ map intToChar [0..n-1]

intToChar :: Int -> String
intToChar n = [chr (n + ord 'A')]

imprimirMatrizAux :: Int -> Matriz -> IO ()
imprimirMatrizAux n matriz = do
  mapM_ (imprimirColuna n matriz) [0..n-1]
  putStrLn ""

imprimirColuna :: Int -> Matriz -> Int -> IO ()
imprimirColuna n matriz coluna = do
  putStr $ show (coluna + 1) ++ " "
  mapM_ (imprimirCelula matriz coluna) [0..n-1]
  putStrLn ""

imprimirCelula :: Matriz -> Int -> Int -> IO ()
imprimirCelula matriz coluna linha = do
  let celula = (matriz !! linha) !! coluna
  if marcado celula
    then putStr "M "
    else if not (exibida celula)
           then putStr "* "
           else if bombado celula
                  then putStr "O "
                  else putStr $ show (closeBombs celula) ++ " "
----------------------------------------------------------------------------------------------------------------
-----------------------------Função para realizar a revelação de uma célula-------------------------------------		  
alterarExibicaoCelula :: Matriz -> Coordenada -> Matriz
alterarExibicaoCelula matriz (x, y) =
  let celula = (matriz !! y) !! x
      celulaAtualizada = celula { exibida = not (exibida celula) }
      linhaAtualizada = take x (matriz !! y) ++ [celulaAtualizada] ++ drop (x + 1) (matriz !! y)
  in take y matriz ++ [linhaAtualizada] ++ drop (y + 1) matriz
----------------------------------------------------------------------------------------------------------------
------------------------------Função utilizada para marcar ou desmarcar uma célula------------------------------
alterarMarcacaoCelula :: Matriz -> Coordenada -> Matriz
alterarMarcacaoCelula matriz (x, y) =
  let celula = (matriz !! y) !! x
      celulaAtualizada = celula { marcado = not (marcado celula) }
      linhaAtualizada = take x (matriz !! y) ++ [celulaAtualizada] ++ drop (x + 1) (matriz !! y)
  in take y matriz ++ [linhaAtualizada] ++ drop (y + 1) matriz
------------------------------Função para simular lógica do jogo-----------------------------------------------

loopJogo :: Matriz -> Int -> Bool -> IO ()
loopJogo matriz numBombas emAndamento = do
  if emAndamento
    then imprimirMatrizJogo matriz
    else putStrLn " "
  if emAndamento
    then do
      putStrLn "Insira um comando (exemplo: A1):"
      comando <- getLine
      let coordenada = parseCoordenada comando
      case coordenada of
        Just (linha, coluna) -> do
          let celula = matriz !! coluna !! linha
          if length comando == 2
            then do
              if exibida celula || marcado celula
                then do
                  putStrLn "Coordenada já foi exibida ou está marcada. Escolha outra coordenada."
                  loopJogo matriz numBombas emAndamento
                else if bombado celula
                  then do
                    putStrLn "Você acertou uma bomba! Fim de jogo."
                    loopJogo matriz numBombas False
                  else do
                    let matrizAtualizada = alterarExibicaoCelula matriz (linha, coluna)
                        numCelulasNaoExibidas = contarCelulasNaoExibidas matrizAtualizada
                    if (numCelulasNaoExibidas == numBombas) && (contarCelulasMarcadas matrizAtualizada == numBombas)
                      then do
                        putStrLn "Você venceu!"
                        loopJogo matrizAtualizada numBombas False
                      else loopJogo matrizAtualizada numBombas True
            else if length comando == 3
              then do
                let primeiroSimbolo = head comando
                if primeiroSimbolo == '+'
                  then do
                    if (marcado celula) || (contarCelulasMarcadas matriz == numBombas) || (exibida celula)
                      then do
                        putStrLn "Coordenada já está marcada (ou exibida) ou você já marcou o número máximo de bombas existentes. Insira um comando válido."
                        loopJogo matriz numBombas emAndamento
                      else do
                        let matrizAtualizada = alterarMarcacaoCelula matriz (linha, coluna)
                            numCelulasNaoExibidas = contarCelulasNaoExibidas matrizAtualizada
                        if (numCelulasNaoExibidas == numBombas) && (contarCelulasMarcadas matrizAtualizada == numBombas)
                          then do
                            putStrLn "Você venceu!"
                            loopJogo matrizAtualizada numBombas False
                          else loopJogo matrizAtualizada numBombas emAndamento
                  else if primeiroSimbolo == '-'
                    then do
                      if marcado celula
                        then do
                          let matrizAtualizada = alterarMarcacaoCelula matriz (linha, coluna)
                          loopJogo matrizAtualizada numBombas emAndamento
                        else do
                          putStrLn "Coordenada não está marcada. Insira um comando válido."
                          loopJogo matriz numBombas emAndamento
                    else do
                      putStrLn "Comando inválido. Insira um comando válido."
                      loopJogo matriz numBombas emAndamento
              else do
                putStrLn "Comando inválido. Insira um comando válido."
                loopJogo matriz numBombas emAndamento
        Nothing -> do
          putStrLn "Comando inválido. Insira um comando válido."
          loopJogo matriz numBombas emAndamento
    else do
      putStrLn "Jogo finalizado."
      imprimirMatrizFinal matriz



------------------------------------Funções utilizadas para contar células marcadas e não exibidas-------------------------------------------------------
contarCelulasNaoExibidas :: Matriz -> Int
contarCelulasNaoExibidas matriz = length $ filter (\celula -> not (exibida celula)) $ concat matriz

contarCelulasMarcadas :: Matriz -> Int
contarCelulasMarcadas matriz = length $ filter (\celula -> (marcado celula)) $ concat matriz
---------------------------------------------------------------------------------------------------------------------------------------------------------


parseCoordenada :: String -> Maybe Coordenada
parseCoordenada comando
  | length comando == 2 = do
      let (colunaChar:linhaStr) = comando
      let coluna = ord colunaChar - ord 'A'
      let linha = (read linhaStr :: Int) - 1
      if coluna >= 0 && linha >= 0
        then Just (linha, coluna)
        else Nothing
  | length comando == 3 = do
      let (sinal:colunaChar:linhaStr) = comando
      let coluna = ord colunaChar - ord 'A'
      let linha = (read linhaStr :: Int) - 1 
      if coluna >= 0 && linha >= 0
        then Just (linha, coluna)
        else Nothing
  | otherwise = Nothing


--------------------------------------------------------------------------------------------------------------
imprimirMatrizFinal :: Matriz -> IO ()
imprimirMatrizFinal matriz = do
  let n = length matriz
  imprimirMatriz n matriz
  where
    imprimirMatriz :: Int -> Matriz -> IO ()
    imprimirMatriz n matriz = do
      mapM_ (imprimirColuna n) [0..n-1]
      putStrLn ""

    imprimirColuna :: Int -> Int -> IO ()
    imprimirColuna n coluna = do
      mapM_ (imprimirCelula coluna) [0..n-1]
      putStrLn ""

    imprimirCelula :: Int -> Int -> IO ()
    imprimirCelula coluna linha = do
      let celula = (matriz !! linha) !! coluna
      if bombado celula
        then putStr "O "
        else putStr $ show (closeBombs celula) ++ " "


--------------------------------------------------------------------------------------------------------------
---Função principal que solicita ao usuário o valor de N e o número de bombas, e gera a matriz correspondente
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
  -- Contagem de bombas ao redor das cédulas ---
  let matrizAtualizada = foldl printResultado matrizFinal resultadosFormatados
      coordenadas = coordenadasMatriz n
      resultados = map (contarTruesVizinhos matrizFinal) coordenadas
      resultadosFormatados = zip coordenadas resultados
  imprimirMatriz n matrizFinal
  loopJogo matrizAtualizada validatedNumBombs True
  --- Fim contagem de bombas ao redor das cédulas ---
  
  
  
  -- let numTruesVizinhos = contarTruesVizinhos matrizFinal coordenada
  -- putStrLn $ "Número de Trues nos vizinhos: " ++ show coordenada ++ show numTruesVizinhos