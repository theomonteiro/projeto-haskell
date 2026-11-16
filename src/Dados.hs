{-# OPTIONS_GHC -Wall #-}
module Dados
    ( dadosCampeonato
    ) where

-- | As três primeiras rodadas do campeonato de teste.
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
    ]