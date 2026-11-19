{-# OPTIONS_GHC -Wall #-}
-- =============================================================
-- CAMADA 3: Leitura, validação (Maybe e Either) e saída
-- =============================================================
module Leitura
    ( Erro(..)
    , descreverErro
    , separar
    , linhasNumeradas
    , lerPartida
    , lerCampeonato
    , timesRepetidos
    , todosOsErros
    ) where

import Tipos (Time, Partida(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, nub)
import Text.Read (readMaybe)
import Data.Either (lefts, rights)
import Data.Maybe (listToMaybe)

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

-- | Quebra uma linha nos separadores. Mantém campos vazios: separar ';' "a;;b" == ["a","","b"]
separar :: Char -> String -> [String]
separar c s = case break (== c) s of
    (campo, [])       -> [campo]
    (campo, _ : resto) -> campo : separar c resto

-- | Remove espaços (e o \r de arquivos do Windows) do começo e do fim
aparar :: String -> String
aparar = dropWhileEnd isSpace . dropWhile isSpace

-- | Linhas que devem ser lidas, com o número que têm no texto original.
--   Linhas em branco e linhas que começam com # são ignoradas.
linhasNumeradas :: String -> [(Int, String)]
linhasNumeradas = filter (deveLer . snd) . zip [1 ..] . map aparar . lines
  where
    deveLer l = not (null l) && take 1 l /= "#"

-- | Converte um campo em número, usando readMaybe (nunca read)
lerNumero :: Int -> String -> Either Erro Int
lerNumero n campo = maybe (Left (NumeroInvalido n campo)) Right (readMaybe campo)

-- | Devolve o erro se a condição for falsa
exigir :: Bool -> Erro -> Either Erro ()
exigir condicao erro = if condicao then Right () else Left erro

-- | Quebra a linha nos 5 campos esperados
campos :: Int -> String -> Either Erro (String, Time, String, String, Time)
campos n linha = case map aparar (separar ';' linha) of
    [r, m, gm, gv, v] | not (null m) && not (null v) -> Right (r, m, gm, gv, v)
    _                                                -> Left (CamposInvalidos n)

-- | Converte uma linha (com seu número) em partida, com notação do na mônada Either:
--   o primeiro Left interrompe o cálculo, sem "escadinha" de case.
lerPartida :: Int -> String -> Either Erro Partida
lerPartida n linha = do
    (rTxt, mand, gmTxt, gvTxt, vis) <- campos n linha
    r  <- lerNumero n rTxt
    gm <- lerNumero n gmTxt
    gv <- lerNumero n gvTxt
    exigir (r >= 1) (NumeroInvalido n rTxt)
    exigir (gm >= 0 && gv >= 0) (PlacarNegativo n)
    exigir (mand /= vis) (MesmoTime n)
    return (Partida r mand gm gv vis)

-- | Um erro para cada time que aparece mais de uma vez na mesma rodada
timesRepetidos :: [Partida] -> [Erro]
timesRepetidos ps = map (uncurry TimeRepetidoNaRodada) (nub (filter repetido escalacoes))
  where
    escalacoes = concatMap (\p -> [(rodada p, mandante p), (rodada p, visitante p)]) ps
    repetido e = length (filter (== e) escalacoes) > 1

-- | Lê o texto inteiro, parando no primeiro erro (traverse), e depois
--   verifica que nenhum time joga duas vezes na mesma rodada.
lerCampeonato :: String -> Either Erro [Partida]
lerCampeonato texto = do
    partidas <- traverse (uncurry lerPartida) (linhasNumeradas texto)
    maybe (Right partidas) Left (listToMaybe (timesRepetidos partidas))

-- | Relatório com todos os erros do texto, e não só o primeiro
todosOsErros :: String -> [Erro]
todosOsErros texto = lefts resultados ++ timesRepetidos (rights resultados)
  where
    resultados = map (uncurry lerPartida) (linhasNumeradas texto)