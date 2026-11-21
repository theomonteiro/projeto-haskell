{-# OPTIONS_GHC -Wall #-}
-- =============================================================
-- MAIN: demonstração das três camadas
-- Todo o texto é montado por funções puras; main é a única função com IO.
-- =============================================================
module Main (main) where

import Tipos (Partida(..))
import Classificacao (classificacao, ateRodada, maiorGoleada, mediaGols)
import Leitura (lerCampeonato, todosOsErros, tabelaFormatada, descreverErro)
import Dados (dadosCampeonato, dadosComErros)

-- | Título de uma seção da saída
secao :: String -> String
secao titulo = unlines [linha, " " ++ titulo, linha]
  where
    linha = replicate 62 '='

-- | Partida em uma linha: "R3  Coritiba 3 x 0 Maringa"
descreverPartida :: Partida -> String
descreverPartida p = "R" ++ show (rodada p) ++ "  " ++ mandante p ++ " " ++ show (golsMandante p)
                  ++ " x " ++ show (golsVisitante p) ++ " " ++ visitante p

-- | Seções 1 a 3: tabela final, tabela intermediária e estatísticas
relatorio :: [Partida] -> String
relatorio ps = concat
    [ secao "1. TABELA FINAL DO CAMPEONATO"
    , tabelaFormatada (classificacao ps)
    , "\n"
    , secao "2. TABELA APOS A 3a RODADA (ateRodada 3)"
    , tabelaFormatada (classificacao (ateRodada 3 ps))
    , "\n"
    , secao "3. ESTATISTICAS"
    , "Partidas lidas:      " ++ show (length ps) ++ "\n"
    , "Maior goleada:       " ++ maybe "nenhuma partida" descreverPartida (maiorGoleada ps) ++ "\n"
    , "Media de gols:       " ++ maybe "sem partidas" show (mediaGols ps) ++ "\n"
    , "Campeonato vazio:    maiorGoleada [] = " ++ show (maiorGoleada [])
      ++ ", mediaGols [] = " ++ show (mediaGols []) ++ "\n"
    , "\n"
    ]

-- | Seção 4: todos os erros de uma entrada inválida
relatorioErros :: String -> String
relatorioErros texto = secao "4. TRATAMENTO DE UMA ENTRADA COM ERROS"
    ++ unlines (map (("  - " ++) . descreverErro) (todosOsErros texto))
    ++ "\nPrimeiro erro (lerCampeonato): "
    ++ either descreverErro (const "nenhum") (lerCampeonato texto) ++ "\n"

main :: IO ()
main = do
    putStr (either (\e -> "Erro nos dados do campeonato: " ++ descreverErro e ++ "\n") relatorio
                   (lerCampeonato dadosCampeonato))
    putStr (relatorioErros dadosComErros)
