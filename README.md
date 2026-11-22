# PjBL2 - Tabela de Classificação (Haskell)

**Grupo 7 — Programação Lógica e Funcional, PUCPR**

O programa calcula a classificação de um campeonato de futebol a partir dos resultados das partidas. O projeto está organizado em três camadas:

1. **Tipos e dados** — representa os times, as partidas, os resultados e as linhas da tabela.
2. **Classificação** — usa funções de ordem superior para calcular a pontuação, aplicar os critérios de desempate e gerar estatísticas.
3. **Leitura e validação** — usa `Maybe` e `Either` para interpretar a entrada, identificar erros e formatar a tabela.

## Ambiente utilizado

O projeto foi compilado e executado no Windows com **GHC 9.10.3**, instalado via GHCup. Usa apenas a biblioteca padrão (`base`).

O programa principal e os testes compilaram sem avisos com `-Wall -Werror`. Os **40 testes passaram**: 7 da Camada 1, 17 da Camada 2 e 16 da Camada 3.

## Como compilar e executar

Execute os comandos na **raiz do projeto**, onde estão as pastas `src/` e `tests/`. No Windows, use o **Git Bash**.

### Programa principal

```bash
mkdir -p build/app
ghc -Wall -Werror -isrc -outputdir build/app -o build/campeonato src/Main.hs &&
./build/campeonato
```

O programa exibe, nesta ordem:

1. Tabela final do campeonato.
2. Tabela após a 3ª rodada.
3. Estatísticas, incluindo a maior goleada e a média de gols por partida.
4. Demonstração do tratamento de uma entrada inválida, com o relatório completo e o primeiro erro encontrado.

### Testes

```bash
mkdir -p build/test
ghc -Wall -Werror -isrc -outputdir build/test -o build/testes tests/Testes.hs &&
./build/testes
```

Cada teste compara o resultado obtido com o esperado e imprime `OK` ou `FALHOU`. O resultado esperado é **40 linhas com `OK` e nenhuma com `FALHOU`**.

Os arquivos intermediários ficam em `build/app/` e `build/test/` porque o programa e os testes definem um módulo chamado `Main`. No Windows, os executáveis gerados são `build/campeonato.exe` e `build/testes.exe`.

### Execução com `runghc`

Para executar diretamente a partir dos arquivos-fonte:

```bash
runghc -isrc src/Main.hs
runghc -isrc tests/Testes.hs
```

### Execução no Haskell Playground

Para executar a demonstração no navegador, copie o conteúdo de `src/Playground.hs`, cole no [Haskell Playground](https://play.haskell.org) e clique em **Run**. Essa versão reúne as três camadas e os dados do campeonato em um único arquivo.

## Estrutura

```text
projeto/
├── src/
│   ├── Tipos.hs           Tipos do domínio e consultas sobre partidas
│   ├── Classificacao.hs   Classificação, desempates e estatísticas
│   ├── Leitura.hs         Leitura, validação e formatação da tabela
│   ├── Dados.hs           Dados do campeonato e exemplo de entrada inválida
│   ├── Main.hs            Demonstração das três camadas
│   └── Playground.hs      Versão em arquivo único para o playground
├── tests/
│   └── Testes.hs          Testes das três camadas com resultados esperados
├── docs/
│   └── decisoes.md        Decisões de modelagem e limitações conhecidas
└── README.md
```

## Formato da entrada

Os dados de demonstração ficam em `src/Dados.hs`. Cada linha representa uma partida com cinco campos separados por `;`: rodada, mandante, gols do mandante, gols do visitante e visitante. Linhas em branco e linhas iniciadas por `#` são ignoradas.

```text
# rodada;mandante;gols mandante;gols visitante;visitante
1;Coritiba;3;2;Athletico
1;Operario;0;0;Londrina
```
