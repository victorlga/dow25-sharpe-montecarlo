import pandas as pd
import itertools
import math
import numpy as np
import numba
from joblib import Parallel, delayed

CSV_PATH = 'dow_jones_close_prices_aug_dec_2024.csv'
NUM_ATIVOS_TOTAL = 30
NUM_ATIVOS_SELECIONADOS = 25
NUM_CARTEIRAS_POR_COMBINACAO = 1000
MAX_PESO = 0.2
DIAS_NO_ANO = 252

def ler_dados_csv(path):
    df = pd.read_csv(path)
    datas = df['Date'].tolist()
    tickers = df.columns[1:].tolist()
    precos = df.iloc[:, 1:].to_numpy()
    return precos, tickers

def calcular_retornos_diarios(precos):
    return (precos[1:] - precos[:-1]) / precos[:-1]

def gerar_pesos_validos(n, batch_size=100):
    while True:
        pesos_batch = np.random.dirichlet(np.ones(n), size=batch_size)
        validos = np.all(pesos_batch <= MAX_PESO, axis=1)
        if np.any(validos):
            return pesos_batch[validos][0]

@numba.jit(nopython=True)
def calcular_sharpe(retornos, pesos):
    r_p = retornos @ pesos
    media_diaria = np.mean(r_p)
    retorno_anual = media_diaria * DIAS_NO_ANO
    n = retornos.shape[1]
    cov = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            cov[i, j] = np.mean((retornos[:, i] - np.mean(retornos[:, i])) * 
                               (retornos[:, j] - np.mean(retornos[:, j])))
    var = 0.0
    for i in range(n):
        for j in range(n):
            var += pesos[i] * cov[i, j] * pesos[j]
    if var <= 0:
        return -math.inf
    desvio_anual = math.sqrt(var) * math.sqrt(DIAS_NO_ANO)
    return retorno_anual / desvio_anual

def expandir_pesos(pesos_25, tickers_25, todos_tickers):
    w_completo = np.zeros(NUM_ATIVOS_TOTAL)
    for i, ticker in enumerate(tickers_25):
        idx = todos_tickers.index(ticker)
        w_completo[idx] = pesos_25[i]
    return w_completo

def processar_combinacao(indices, tickers, retornos):
    tickers_25 = [tickers[j] for j in indices]
    retornos_25 = retornos[:, indices]
    local_melhor_SR = -math.inf
    local_melhor_pesos = None
    local_melhor_comb = None
    for _ in range(NUM_CARTEIRAS_POR_COMBINACAO):
        pesos = gerar_pesos_validos(NUM_ATIVOS_SELECIONADOS)
        SR = calcular_sharpe(retornos_25, pesos)
        if SR > local_melhor_SR:
            local_melhor_SR = SR
            local_melhor_pesos = pesos
            local_melhor_comb = tickers_25
    return local_melhor_SR, local_melhor_pesos, local_melhor_comb

def main():
    precos, tickers = ler_dados_csv(CSV_PATH)
    retornos = calcular_retornos_diarios(precos)

    melhor_SR = -math.inf
    melhor_pesos = None
    melhor_comb = None

    combinacoes = list(itertools.combinations(range(NUM_ATIVOS_TOTAL), NUM_ATIVOS_SELECIONADOS))
    total = len(combinacoes)

    resultados = Parallel(n_jobs=-1, verbose=10)(
        delayed(processar_combinacao)(indices, tickers, retornos) for indices in combinacoes
    )

    for i, (SR, pesos, comb) in enumerate(resultados):
        if SR > melhor_SR:
            melhor_SR = SR
            melhor_pesos = pesos
            melhor_comb = comb
        if (i + 1) % 1000 == 0 or i == total - 1:
            print(f"Processadas {i+1}/{total} combinações...")
            print(f"Melhor SR: {melhor_SR:.4f}")
            print("Melhor Carteira:")
            print(f"{'Ticker':<10} {'Peso (%)':>10}")
            print("-" * 20)
            for ticker, peso in zip(melhor_comb, melhor_pesos):
                print(f"{ticker:<10} {peso*100:>10.2f}")
            print("-" * 50)

    w_final = expandir_pesos(melhor_pesos, melhor_comb, tickers)

    print("\nMelhor Sharpe Ratio:", melhor_SR)
    print("Ativos selecionados:", melhor_comb)
    print("Pesos correspondentes:", w_final)

if __name__ == "__main__":
    main()