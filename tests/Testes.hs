{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Tipos
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

main :: IO ()
main = camada1