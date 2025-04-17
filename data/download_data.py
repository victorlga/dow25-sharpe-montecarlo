import yfinance as yf

tickers = [
    "UNH", "GS", "MSFT", "HD", "V", "SHW", "MCD", "CAT", "AMGN", "AXP",
    "TRV", "CRM", "IBM", "JPM", "AAPL", "HON", "AMZN", "PG", "BA", "JNJ",
    "CVX", "MMM", "NVDA", "WMT", "DIS", "MRK", "KO", "CSCO", "NKE", "VZ"
]

start_date = "2024-08-01"
end_date = "2024-12-31"

# Baixa todos os dados
raw_data = yf.download(tickers, start=start_date, end=end_date)

# Filtra apenas a parte "Close"
close_data = raw_data["Close"]

# Salva
close_data.to_csv("dow_jones_close_prices_aug_dec_2024.csv")

print("Somente preços de fechamento salvos em 'dow_jones_close_prices_aug_dec_2024.csv'")
