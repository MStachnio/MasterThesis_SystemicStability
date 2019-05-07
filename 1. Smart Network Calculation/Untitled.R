library(quantmod)
library(BatchGetSymbols)

# Tickers as of March 2019
tickerList = c("ABBV","ABT", "ACN", "ADBE", "AGN", "AIG","ALL", "AMGN", "AMZN", "AXP", "BA", "BAC", "BIIB", "BK", "BKNG", "BLK", "BMY", "BRK-B", "C", "CAT", "CELG", "CHTR", "CL", "CMCSA", "COF", "COP", "COST", "CSCO", "CVS", "CVX", "DHR", "DIS", "DUK", "DWDP", "EMR", "EXC", "F", "FB", "FDX", "GD", "GE", "GILD", "GM", "GOOG", "GOOGL", "GS", "HD", "HON", "IBM", "INTC", "JNJ", "JPM", "KHC", "KMI", "KO", "LLY", "LMT", "LOW", "MA", "MCD", "MDLZ", "MDT", "MET", "MMM", "MO", "MRK", "MS", "MSFT", "NEE", "NFLX", "NKE", "NVDA", "ORCL", "OXY", "PEP", "PFE", "PG", "PM", "PYPL", "QCOM", "RTN", "SBUX", "SLB", "SO", "SPG", "T", "TGT", "TXN", "UNH", "UNP", "UPS", "USB", "UTX", "V", "VZ", "WBA", "WFC", "WMT", "XOM")
data = 0
data_total = getSymbols("AAPL", auto.assign = F, src="yahoo", from = "2018-01-01", to = "2018-12-31", silent = TRUE)
data_total = data_total[,4]

for (ticker in tickerList) {
  data = getSymbols(ticker, auto.assign = F, src="yahoo", from = "2018-01-01", to = "2018-12-31", silent = TRUE)
  data = data[,4]
  data_total = merge(data_total, data)
}


write.table(data_total,file="S&P100.csv",append=FALSE,sep=",", 
            quote=TRUE,row.names=TRUE,col.names=TRUE) 
