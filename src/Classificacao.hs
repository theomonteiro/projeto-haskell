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
    , linhaDoTime
    , classificacao
    , ateRodada
    , diferencaGols
    , maiorGoleada
    , mediaGols
    ) where

import Tipos
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

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

-- | Linha da tabela de um time, acumulada com foldl sobre as partidas dele.
--   foldl percorre as partidas na ordem em que aconteceram (como o extrato da aula 11).
linhaDoTime :: [Partida] -> Time -> Linha
linhaDoTime ps t = foldl acumular (Linha t 0 0 0 0 0 0) (filter (participou t) ps)
  where
    -- o filter já garante que o time jogou; o Nothing só é tratado para a função ser total
    acumular acc p = case (placarPara t p, resultadoPara t p) of
        (Just placar, Just r) -> somar acc placar r
        _                     -> acc

    somar acc (feitos, sofridos) r = acc
        { jogos      = jogos acc + 1
        , vitorias   = vitorias acc + contar Vitoria
        , empates    = empates acc  + contar Empate
        , derrotas   = derrotas acc + contar Derrota
        , golsPro    = golsPro acc + feitos
        , golsContra = golsContra acc + sofridos
        }
      where
        contar x = if r == x then 1 else 0

-- | Tabela completa, ordenada pelos critérios do Brasileirão:
--   pontos > vitórias > saldo de gols > gols pró > ordem alfabética.
--   Uma única função de comparação, combinando critérios com <> (Ordering é Monoid).
classificacao :: [Partida] -> [Linha]
classificacao ps = sortBy criterios (map (linhaDoTime ps) (times ps))
  where
    criterios = comparing (Down . pontosLinha)
             <> comparing (Down . vitorias)
             <> comparing (Down . saldo)
             <> comparing (Down . golsPro)
             <> comparing time

-- | Só as partidas até a rodada informada
ateRodada :: Int -> [Partida] -> [Partida]
ateRodada r = filter ((<= r) . rodada)

-- | Diferença absoluta de gols de uma partida
diferencaGols :: Partida -> Int
diferencaGols p = abs (golsMandante p - golsVisitante p)

-- | Partida com a maior diferença de gols; Nothing para lista vazia.
--   Em caso de empate na diferença, fica a primeira da lista.
maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada = foldr escolher Nothing
  where
    escolher p Nothing = Just p
    escolher p (Just m)
        | diferencaGols p >= diferencaGols m = Just p
        | otherwise                          = Just m

-- | Média de gols por partida; Nothing para lista vazia
mediaGols :: [Partida] -> Maybe Double
mediaGols [] = Nothing
mediaGols ps = Just (fromIntegral totalGols / fromIntegral (length ps))
  where
    totalGols = foldr (\p acc -> golsMandante p + golsVisitante p + acc) 0 ps :: Int
