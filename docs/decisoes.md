# Decisões de Modelagem e Limitações - PjBL2

Este documento justifica as escolhas técnicas das três camadas do projeto, com ênfase
em tipos que evitam estados inválidos, funções totais e separação entre o núcleo puro e o IO.

## 1. Camada 1: Tipos e Modelagem de Dados

- **Records para `Partida` e `Linha`**: evitamos tuplas como `(Int, String, Int, Int, String)`.
  Numa tupla de `Int`s, trocar os gols do mandante com os do visitante compila sem aviso;
  com records, cada campo tem nome.
- **Tipo soma `Resultado`**: `data Resultado = Vitoria | Empate | Derrota` fecha o conjunto de
  valores possíveis. Com `-Wall`, o compilador avisa se algum caso ficar sem tratamento
  (por exemplo, em `pontos`).
- **`Linha` guarda só contagens**: jogos, vitórias, empates, derrotas, gols pró e contra.
  Pontos, saldo e aproveitamento são **derivados** (`pontosLinha`, `saldo`, `aproveitamento`),
  e não armazenados, para não existir uma linha com pontos incoerentes com as vitórias.
- **`placarPara` e `resultadoPara` devolvem `Maybe`**: se o time não jogou a partida, o resultado
  é `Nothing`, e quem chama é obrigado a tratar esse caso. `resultadoPara` é definida como
  `fmap` sobre `placarPara`, para que as duas não possam discordar.
- **Campeonato de teste**: 6 times em turno único (15 jogos, 5 rodadas, nenhum time duas vezes
  na mesma rodada), com empate em 0 x 0, duas vitórias por 3 gols e Londrina e Operario
  empatados em pontos **e** em vitórias, decididos pelo saldo de gols.

## 2. Camada 2: Ordem Superior, sem Recursão Explícita

- **`map`**: gera uma linha para cada time em `classificacao`.
- **`filter`**: separa as partidas de um time (`linhaDoTime`) e as partidas até uma rodada (`ateRodada`).
- **`foldl` em `linhaDoTime`**: a linha do time é acumulada na ordem em que os jogos aconteceram,
  como o extrato bancário da aula 11. Usamos `foldl` do Prelude, e não `foldl'`, porque
  `foldl'` só faz parte do Prelude a partir do GHC 9.10: em versões anteriores ele precisa
  de `import Data.List (foldl')`, e do 9.10 em diante esse import passa a ser redundante.
  Com `foldl`, o mesmo código compila sem avisos nas duas situações. Como as listas são pequenas (poucas dezenas de partidas), a diferença de memória
  entre os dois não importa aqui.
- **`foldr` em `maiorGoleada` e `mediaGols`**: `maiorGoleada` começa com `Nothing`, então nunca
  quebra com lista vazia (ao contrário de `maximum` ou `foldr1`). Em empate na diferença de
  gols, fica a partida que aparece primeiro.
- **Composição `(.)`**: em `ateRodada` (`(<= r) . rodada`), nos critérios de ordenação
  (`Down . pontosLinha`) e em `times`.
- **Ordenação com uma única função de comparação**: `comparing` combinado com `<>` (`Ordering`
  é um Monoid) e `Down` para ordem decrescente. Critérios, inspirados no Brasileirão:
  pontos > vitórias > saldo de gols > gols pró > ordem alfabética.
- **`aproveitamento` com `fromIntegral` e `/`**: `div` truncaria o percentual. Time sem jogos
  tem aproveitamento 0, sem divisão por zero.

## 3. Camada 3: Leitura, Validação e Saída

- **`readMaybe` em vez de `read`**: `read "dois"` lança exceção; `readMaybe` devolve `Nothing`,
  que `lerNumero` transforma em `Left (NumeroInvalido linha campo)` com `maybe`.
- **`do` na mônada `Either`**: `lerPartida` encadeia separar os campos, converter os números e
  validar o placar. O primeiro `Left` interrompe o cálculo, sem `case` aninhados. A função
  auxiliar `exigir` transforma uma condição em `Right ()` ou `Left erro`.
- **`traverse` em `lerCampeonato`**: aplica `lerPartida` a todas as linhas e para no primeiro erro.
  Depois, `timesRepetidos` verifica se algum time joga duas vezes na mesma rodada.
- **`todosOsErros`**: usa `map` em vez de `traverse` para não parar no primeiro erro, e junta os
  erros das linhas (`lefts`) com os times repetidos entre as linhas válidas (`rights`).
- **Erros indicam onde está o problema**: os erros de linha carregam o **número da linha no texto
  original** (contando comentários e linhas em branco, para bater com o editor), e
  `TimeRepetidoNaRodada` carrega a rodada **e o time** repetido. `descreverErro` gera a mensagem legível.
- **Entrada tolerante a formatação**: espaços em volta dos campos e o `\r` de arquivos do Windows
  são removidos. Nome de time vazio gera `CamposInvalidos`, e rodada menor que 1 gera `NumeroInvalido`.
- **`tabelaFormatada` sem cortes**: a largura de cada coluna é calculada a partir do maior valor
  (`transpose` + `foldr`), então nomes longos e saldos como `-10` aparecem inteiros.
- **Núcleo puro e IO separados**: os dados ficam em `Dados.hs` como `String` (o playground não tem
  entrada pelo teclado, então não usamos `getLine`). Todo o texto da saída é montado por funções
  puras (`relatorio`, `relatorioErros`), e `main` é a única função com IO.

## 4. Limitações Conhecidas

- **Sem confronto direto**: o desempate não considera o resultado entre os times empatados, nem
  cartões ou sorteio. Depois de gols pró, a ordem é alfabética.
- **Nomes são comparados exatamente**: `Parana` e `Paraná` (ou `parana`) são times diferentes.
  Não há normalização de acentos ou maiúsculas.
- **Pontuação fixa 3/1/0**: os pontos por resultado estão em `pontos`; outro esporte exigiria
  mudar essa função e os critérios de desempate.
- **Turno e returno não são verificados**: o programa não confere se cada par de times se
  enfrentou o número certo de vezes, nem se as rodadas são consecutivas.
- **`todosOsErros` só procura times repetidos entre as linhas válidas**: uma linha com erro
  de digitação não entra na verificação de rodada.
- **Desempenho**: `timesRepetidos` e `linhaDoTime` percorrem a lista várias vezes (custo
  quadrático). Para um campeonato estadual isso não faz diferença, mas não escalaria para
  milhares de partidas sem usar `Data.Map` (fora da biblioteca permitida).
