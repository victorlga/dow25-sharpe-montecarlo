#!/bin/bash

# Limpa o arquivo anterior
echo "Resultados dos Experimentos - $(date)" > experiment.txt

echo "Executando com paralelismo (N4)..." >> experiment.txt
for i in {1..5}
do
  echo "Execução $i com -N10" >> experiment.txt
  { time cabal run dow25-sharpe-montecarlo -- +RTS -N10 -RTS; } >> experiment.txt 2>&1
  echo "------------------------" >> experiment.txt
done

echo "Executando sem paralelismo (N1)..." >> experiment.txt
for i in {1..5}
do
  echo "Execução $i com -N1" >> experiment.txt
  { time cabal run dow25-sharpe-montecarlo -- +RTS -N1 -RTS; } >> experiment.txt 2>&1
  echo "------------------------" >> experiment.txt
done
