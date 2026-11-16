{-# OPTIONS_GHC -Wall #-}
-- =============================================================
-- DADOS: campeonato de teste, como String (lido pela Camada 3)
-- =============================================================
module Dados
    ( dadosCampeonato
    , dadosComErros
    ) where

-- | Campeonato paranaense fictício: 6 times, 5 rodadas, turno único (15 jogos).
--   Cobre: empate em 0 x 0 (rodada 1), vitórias por 3 gols (rodadas 2 e 3) e
--   Londrina e Operario empatados em pontos e vitórias, decididos pelo saldo.
dadosCampeonato :: String
dadosCampeonato = unlines
    [ "# rodada;mandante;gols mandante;gols visitante;visitante"
    , ""
    , "# Rodada 1"
    , "1;Coritiba;3;2;Athletico"
    , "1;Operario;0;0;Londrina"
    , "1;Maringa;0;2;Parana"
    , ""
    , "# Rodada 2"
    , "2;Athletico;3;0;Parana"
    , "2;Londrina;1;1;Coritiba"
    , "2;Operario;1;0;Maringa"
    , ""
    , "# Rodada 3"
    , "3;Londrina;0;2;Athletico"
    , "3;Parana;0;1;Operario"
    , "3;Coritiba;3;0;Maringa"
    , ""
    , "# Rodada 4"
    , "4;Athletico;2;0;Maringa"
    , "4;Operario;0;2;Coritiba"
    , "4;Parana;0;1;Londrina"
    , ""
    , "# Rodada 5"
    , "5;Operario;1;1;Athletico"
    , "5;Parana;0;2;Coritiba"
    , "5;Londrina;2;0;Maringa"
    ]
