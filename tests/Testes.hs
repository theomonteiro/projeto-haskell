{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Tipos
import Classificacao
import Leitura (lerCampeonato)
import Dados (dadosCampeonato)
import Data.Either (fromRight)

testar :: (Eq a, Show a) => String -> a -> a -> IO ()
testar nome obtido esperado
    | obtido == esperado = putStrLn ("OK      " ++ nome)
    | otherwise          = putStrLn ("FALHOU  " ++ nome
                                     ++ ": obtido " ++ show obtido
                                     ++ ", esperado " ++ show esperado)

partidas :: [Partida]
partidas = fromRight [] (lerCampeonato dadosCampeonato)

exemplo :: Partida
exemplo = Partida 1 "Coritiba" 2 1 "Athletico"

camada1 :: IO ()
camada1 = do
    testar "resultadoPara mandante"
        (resultadoPara "Coritiba" exemplo)
        (Just Vitoria)

    testar "resultadoPara visitante"
        (resultadoPara "Athletico" exemplo)
        (Just Derrota)

    testar "resultadoPara time que nao jogou"
        (resultadoPara "Parana" exemplo)
        Nothing

    testar "resultadoPara empate"
        (resultadoPara "A" (Partida 1 "A" 0 0 "B"))
        (Just Empate)

    testar "participou mandante e time ausente"
        (participou "Coritiba" exemplo, participou "Parana" exemplo)
        (True, False)

    testar "times sem repeticao e em ordem"
        (times partidas)
        ["Athletico", "Coritiba", "Londrina", "Maringa", "Operario", "Parana"]

    testar "times de campeonato vazio"
        (times [])
        []

-- | Tabela final esperada, conferida a partir das 15 partidas.
tabelaEsperada :: [Linha]
tabelaEsperada =
    [ Linha "Coritiba"  5 4 1 0 11  3
    , Linha "Athletico" 5 3 1 1 10  4
    , Linha "Londrina"  5 2 2 1  4  3
    , Linha "Operario"  5 2 2 1  3  3
    , Linha "Parana"    5 1 0 4  2  7
    , Linha "Maringa"   5 0 0 5  0 10
    ]

camada2 :: IO ()
camada2 = do
    testar "pontos de vitoria, empate e derrota"
        (map pontos [Vitoria, Empate, Derrota])
        [3, 1, 0]

    testar "classificacao completa"
        (classificacao partidas)
        tabelaEsperada

    testar "pontos da tabela final"
        (map pontosLinha (classificacao partidas))
        [13, 10, 8, 8, 3, 0]

    testar "saldos da tabela final"
        (map saldo (classificacao partidas))
        [8, 6, 1, 0, -5, -10]

    let londrina = linhaDoTime partidas "Londrina"
        operario = linhaDoTime partidas "Operario"

    testar "desempate: ambos com 8 pontos e 2 vitorias"
        ( (pontosLinha londrina, vitorias londrina)
        , (pontosLinha operario, vitorias operario)
        )
        ((8, 2), (8, 2))

    testar "desempate decidido pelo saldo"
        (saldo londrina, saldo operario)
        (1, 0)

    testar "Londrina fica antes de Operario"
        (map time (take 2 (drop 2 (classificacao partidas))))
        ["Londrina", "Operario"]

    let tabelaR3 = classificacao (ateRodada 3 partidas)

    testar "ateRodada 3 tem 9 partidas"
        (length (ateRodada 3 partidas))
        9

    testar "ordem apos a rodada 3"
        (map time tabelaR3)
        ["Coritiba", "Operario", "Athletico", "Parana", "Londrina", "Maringa"]

    testar "classificacao intermediaria diferente da final"
        (tabelaR3 /= classificacao partidas)
        True

    testar "aproveitamento do Coritiba arredondado"
        (round (aproveitamento (linhaDoTime partidas "Coritiba")) :: Int)
        87

    testar "aproveitamento de time sem jogos"
        (aproveitamento (linhaDoTime partidas "Inexistente"))
        0

    testar "maior goleada: primeira com diferenca de 3 gols"
        (maiorGoleada partidas)
        (Just (Partida 2 "Athletico" 3 0 "Parana"))

    testar "media de 30 gols em 15 partidas"
        (mediaGols partidas)
        (Just 2.0)

    testar "maiorGoleada de lista vazia"
        (maiorGoleada [])
        Nothing

    testar "mediaGols de lista vazia"
        (mediaGols [])
        Nothing

    testar "classificacao de lista vazia"
        (classificacao [])
        []

main :: IO ()
main = do
    camada1
    camada2