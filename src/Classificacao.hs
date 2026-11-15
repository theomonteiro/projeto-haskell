{-# OPTIONS_GHC -Wall #-}
-- =============================================================
-- CAMADA 2: Classificação com funções de ordem superior
-- (sem recursão explícita: só map, filter, dobras e composição)
-- =============================================================
module Classificacao
    ( pontos
    , pontosLinha
    , saldo
    , aproveitamento
    ) where

import Tipos

-- | Pontos ganhos por resultado: 3 pela vitória, 1 pelo empate, 0 pela derrota
pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

-- | Total de pontos de uma linha da tabela (reaproveita 'pontos')
pontosLinha :: Linha -> Int
pontosLinha l = vitorias l * pontos Vitoria
              + empates l  * pontos Empate
              + derrotas l * pontos Derrota

-- | Saldo de gols: gols pró menos gols contra
saldo :: Linha -> Int
saldo l = golsPro l - golsContra l

-- | Percentual dos pontos disputados que o time conquistou; 0 se não jogou.
--   fromIntegral faz a ponte entre Int e Double (div truncaria o resultado).
aproveitamento :: Linha -> Double
aproveitamento l
    | jogos l == 0 = 0
    | otherwise    = 100 * fromIntegral (pontosLinha l)
                         / fromIntegral (jogos l * pontos Vitoria)
