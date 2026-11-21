{-# OPTIONS_GHC -Wall #-}
-- Versao em arquivo unico para o play.haskell.org.
-- GERADO a partir dos modulos de src/ (mesma ordem e mesmo codigo).
-- Para rodar: cole tudo no playground e clique em Run.
module Main (main) where

import Data.Char (isSpace)
import Data.Either (lefts, rights)
import Data.List (dropWhileEnd, intercalate, nub, sort, sortBy, transpose)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing, Down(..))
import Text.Read (readMaybe)

-- =============================================================
-- CAMADA 1: Tipos e dados do campeonato
-- =============================================================

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

-- =============================================================
-- CAMADA 2: Classificação com funções de ordem superior
-- (sem recursão explícita: só map, filter, dobras e composição)
-- =============================================================

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

-- =============================================================
-- CAMADA 3: Leitura, validação (Maybe e Either) e saída
-- =============================================================

-- | Erros possíveis na entrada. Os de linha carregam o número da linha
--   no texto original (contando comentários e linhas em branco).
data Erro
    = CamposInvalidos Int                -- ^ linha sem 5 campos, ou com nome de time vazio
    | NumeroInvalido Int String          -- ^ linha e o campo que não é um número válido
    | PlacarNegativo Int                 -- ^ linha com gols negativos
    | MesmoTime Int                      -- ^ linha em que um time joga contra si mesmo
    | TimeRepetidoNaRodada Int Time      -- ^ rodada e o time escalado duas vezes nela
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

-- | Tabela pronta para impressão. As colunas se ajustam ao maior valor,
--   então nada é cortado (nomes longos, saldo -10 etc.).
tabelaFormatada :: [Linha] -> String
tabelaFormatada ls = unlines (formatar cabecalho : separador : map formatar corpo)
  where
    cabecalho = ["Pos", "Time", "Pts", "J", "V", "E", "D", "GP", "GC", "SG", "Aprov"]
    corpo     = zipWith celulas [1 :: Int ..] ls
    celulas i l =
        [ show i, time l, show (pontosLinha l), show (jogos l), show (vitorias l)
        , show (empates l), show (derrotas l), show (golsPro l), show (golsContra l)
        , show (saldo l), show (round (aproveitamento l) :: Int) ++ "%" ]

    larguras  = map (foldr (max . length) 0) (transpose (cabecalho : corpo))
    formatar  = intercalate " | " . zipWith3 alinhar [0 :: Int ..] larguras
    alinhar coluna n s
        | coluna == 1 = s ++ replicate (n - length s) ' '   -- nome do time à esquerda
        | otherwise   = replicate (n - length s) ' ' ++ s   -- números à direita
    separador = replicate (sum larguras + 3 * (length larguras - 1)) '-'

-- =============================================================
-- DADOS: campeonato de teste, como String (lido pela Camada 3)
-- =============================================================

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

-- | Entrada com um erro de cada tipo, para demonstrar a validação
dadosComErros :: String
dadosComErros = unlines
    [ "# entrada com erros de digitacao"
    , "1;Coritiba;dois;1;Athletico"      -- linha 2: NumeroInvalido
    , "1;Parana;1;1;Parana"              -- linha 3: MesmoTime
    , "1;Londrina;-1;0;Operario"         -- linha 4: PlacarNegativo
    , "2;Maringa;0;0;Athletico;Extra"    -- linha 5: CamposInvalidos
    , "2;Coritiba;2;0;Londrina"
    , "2;Coritiba;1;0;Operario"          -- rodada 2: Coritiba escalado duas vezes
    ]

-- =============================================================
-- MAIN: demonstração das três camadas
-- Todo o texto é montado por funções puras; main é a única função com IO.
-- =============================================================

-- | Título de uma seção da saída
secao :: String -> String
secao titulo = unlines [linha, " " ++ titulo, linha]
  where
    linha = replicate 62 '='

-- | Partida em uma linha: "R3  Coritiba 3 x 0 Maringa"
descreverPartida :: Partida -> String
descreverPartida p = "R" ++ show (rodada p) ++ "  " ++ mandante p ++ " " ++ show (golsMandante p)
                  ++ " x " ++ show (golsVisitante p) ++ " " ++ visitante p

-- | Seções 1 a 3: tabela final, tabela intermediária e estatísticas
relatorio :: [Partida] -> String
relatorio ps = concat
    [ secao "1. TABELA FINAL DO CAMPEONATO"
    , tabelaFormatada (classificacao ps)
    , "\n"
    , secao "2. TABELA APOS A 3a RODADA (ateRodada 3)"
    , tabelaFormatada (classificacao (ateRodada 3 ps))
    , "\n"
    , secao "3. ESTATISTICAS"
    , "Partidas lidas:      " ++ show (length ps) ++ "\n"
    , "Maior goleada:       " ++ maybe "nenhuma partida" descreverPartida (maiorGoleada ps) ++ "\n"
    , "Media de gols:       " ++ maybe "sem partidas" show (mediaGols ps) ++ "\n"
    , "Campeonato vazio:    maiorGoleada [] = " ++ show (maiorGoleada [])
      ++ ", mediaGols [] = " ++ show (mediaGols []) ++ "\n"
    , "\n"
    ]

-- | Seção 4: todos os erros de uma entrada inválida
relatorioErros :: String -> String
relatorioErros texto = secao "4. TRATAMENTO DE UMA ENTRADA COM ERROS"
    ++ unlines (map (("  - " ++) . descreverErro) (todosOsErros texto))
    ++ "\nPrimeiro erro (lerCampeonato): "
    ++ either descreverErro (const "nenhum") (lerCampeonato texto) ++ "\n"

main :: IO ()
main = do
    putStr (either (\e -> "Erro nos dados do campeonato: " ++ descreverErro e ++ "\n") relatorio
                   (lerCampeonato dadosCampeonato))
    putStr (relatorioErros dadosComErros)
