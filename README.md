# PjBL2 - Tabela de Classificação (Haskell)

**Grupo 7**: Programação Lógica e Funcional, PUCPR

Calcula a tabela de classificação de um campeonato a partir dos resultados das
partidas, em três camadas: tipos (Camada 1), classificação com funções de ordem
superior (Camada 2) e leitura/validação com `Maybe` e `Either` (Camada 3).

## Ambiente utilizado

**GHC local, instalado via GHCup.** Usamos só a biblioteca padrão (`base`), sem
pacotes extras. O código compila sem avisos com `-Wall` no GHC 9.4 e no 9.10.

O arquivo `src/Playground.hs` traz o mesmo código em um arquivo só, para quem
quiser rodar no [play.haskell.org](https://play.haskell.org): é só colar e clicar em **Run**.

## Como compilar e executar

Todos os comandos são executados na raiz do projeto.

**Programa principal** (tabela final, tabela após a 3ª rodada, estatísticas e entrada com erros):

```bash
mkdir -p build
ghc -Wall -isrc -outputdir build -o build/campeonato src/Main.hs
./build/campeonato
```

**Testes** (cada linha mostra `OK` ou `FALHOU`):

```bash
ghc -Wall -isrc -outputdir build -o build/testes tests/Testes.hs
./build/testes
```

No Windows, o executável é `build\campeonato.exe` (e `build\testes.exe`).

Para rodar sem gerar executável:

```bash
runghc -isrc src/Main.hs
runghc -isrc tests/Testes.hs
```

## Estrutura

```
projeto/
├── src/
│   ├── Tipos.hs           Camada 1: tipos e consultas básicas
│   ├── Classificacao.hs   Camada 2: ordem superior
│   ├── Leitura.hs         Camada 3: Maybe, Either e formatação
│   ├── Dados.hs           campeonato de teste, como String
│   ├── Main.hs            demonstração das três camadas
│   └── Playground.hs      tudo em um arquivo, para o play.haskell.org
├── tests/
│   └── Testes.hs          testes com resultado esperado
├── docs/
│   └── decisoes.md        decisões de modelagem e limitações conhecidas
└── README.md
```

## Formato da entrada

Uma partida por linha, com os campos separados por `;`. Linhas em branco e
linhas que começam com `#` são ignoradas.

```
# rodada;mandante;gols mandante;gols visitante;visitante
1;Coritiba;3;2;Athletico
1;Operario;0;0;Londrina
```
