{-# OPTIONS_GHC -Wall #-}
-- =============================================================
-- CAMADA 1: Tipos e dados do campeonato
-- =============================================================
module Tipos
    ( Time
    , Resultado(..)
    , Partida(..)
    , Linha(..)
    , participou
    , placarPara
    , resultadoPara
    , times
    ) where

import Data.List (nub, sort)

-- | Nome de um time
type Time = String

-- | Resultado de uma partida do ponto de vista de um time
data Resultado = Vitoria | Empate | Derrota
    deriving (Show, Eq)

-- | Uma partida: rodada, mandante, gols do mandante, gols do visitante e visitante
data Partida = Partida
    { rodada        :: Int
    , mandante      :: Time
    , golsMandante  :: Int
    , golsVisitante :: Int
    , visitante     :: Time
    } deriving (Show, Eq)

-- | Uma linha da tabela de classificação
data Linha = Linha
    { time       :: Time
    , jogos      :: Int
    , vitorias   :: Int
    , empates    :: Int
    , derrotas   :: Int
    , golsPro    :: Int
    , golsContra :: Int
    } deriving (Show, Eq)

-- | Verdadeiro se o time jogou a partida (como mandante ou visitante)
participou :: Time -> Partida -> Bool
participou t p = t == mandante p || t == visitante p

-- | Placar do ponto de vista do time: (gols feitos, gols sofridos).
--   Nothing se o time não jogou a partida.
placarPara :: Time -> Partida -> Maybe (Int, Int)
placarPara t p
    | t == mandante p  = Just (golsMandante p, golsVisitante p)
    | t == visitante p = Just (golsVisitante p, golsMandante p)
    | otherwise        = Nothing

-- | Resultado da partida do ponto de vista do time; Nothing se ele não jogou.
--   Usa Maybe como functor (fmap) sobre o placar.
resultadoPara :: Time -> Partida -> Maybe Resultado
resultadoPara t = fmap (uncurry comparar) . placarPara t
  where
    comparar feitos sofridos = case compare feitos sofridos of
        GT -> Vitoria
        EQ -> Empate
        LT -> Derrota

-- | Todos os times do campeonato, sem repetição e em ordem alfabética
times :: [Partida] -> [Time]
times = sort . nub . concatMap (\p -> [mandante p, visitante p])
