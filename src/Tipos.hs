{-# OPTIONS_GHC -Wall #-}
-- =============================================================
-- CAMADA 1: Tipos e dados do campeonato
-- =============================================================
module Tipos
    ( Time
    , Resultado(..)
    , Partida(..)
    , Linha(..)
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
