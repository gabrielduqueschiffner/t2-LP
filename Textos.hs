module Textos
(mensagemBemVindo
,mensagemPerdeu
,mensagemVenceu
,mainMenu
,menuInstrucoes
)where





mensagemBemVindo :: IO()
mensagemBemVindo = do
    putStrLn "             Bem Vindo ao jogo Campo Minado" 

mensagemPerdeu :: IO() 
mensagemPerdeu = do
    putStrLn "Game Over! Você foi explodido!"

mensagemVenceu :: IO()
mensagemVenceu = do
    putStrLn "Parabéns! Você venceu!" 

    

mainMenu :: IO()
mainMenu = do
    putStrLn "" 
    putStrLn "|-----------------------------------------------------------------------------|" 
    putStrLn "|                                 Menu                                        |" 
    putStrLn "|                           (1) Iniciar Jogo                                  |" 
    putStrLn "|                           (2) Instruções do Jogo                            |" 
    putStrLn "|                           (3) Sair do Jogo                                  |" 
    putStrLn "|-----------------------------------------------------------------------------|" 



menuInstrucoes :: IO()
menuInstrucoes = do
    putStrLn "" 
    putStrLn "|-----------------------------------------------------------------------------|" 
    putStrLn "|                          Instruçoes do Jogo                                 |" 
    putStrLn "|                                                                             |" 
    putStrLn "|             Revela, exemplo: A1 - Revela posição (0,1).                     |"
    putStrLn "|             Marca, exemplo: +A1 - Marca posicao como Bombada (0,1).         |"
    putStrLn "|             Desmarca, exemplo: -A1 - Desmarca posição como Bombada (0,1).   |" 
    putStrLn "|-----------------------------------------------------------------------------|" 