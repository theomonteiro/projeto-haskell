{-# OPTIONS_GHC -Wall #-}
-- =============================================================
-- CAMADA 3: Leitura, validação (Maybe e Either) e saída
-- =============================================================
module Leitura
    ( Erro(..)
    , descreverErro
    ) where

import Tipos (Time)

-- | Erros possíveis na entrada. Os de linha carregam o número da linha
--   no texto original (contando comentários e linhas em branco).
data Erro
    = CamposInvalidos Int                -- ^ linha sem 5 campos, ou com nome de time vazio
    | NumeroInvalido Int String          -- ^ linha e o campo que não é um número válido
    | PlacarNegativo Int                 -- ^ linha com gols negativos
    | MesmoTime Int                      -- ^ linha em que um time joga contra si mesmo
    | TimeRepetidoNaRodada Int Time       -- ^ rodada e o time escalado duas vezes nela
    deriving (Show, Eq)

-- | Mensagem legível para cada erro
descreverErro :: Erro -> String
descreverErro (CamposInvalidos n)        = "Linha " ++ show n ++ ": esperado 5 campos (rodada;mandante;gols;gols;visitante), sem nomes vazios"
descreverErro (NumeroInvalido n campo)   = "Linha " ++ show n ++ ": \"" ++ campo ++ "\" nao e um numero valido"
descreverErro (PlacarNegativo n)         = "Linha " ++ show n ++ ": placar negativo"
descreverErro (MesmoTime n)              = "Linha " ++ show n ++ ": um time nao pode jogar contra si mesmo"
descreverErro (TimeRepetidoNaRodada r t) = "Rodada " ++ show r ++ ": " ++ t ++ " joga mais de uma vez"