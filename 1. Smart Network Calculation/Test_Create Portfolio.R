library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
library(foreach)
library(DEoptim)
library(iterators)
library(fGarch)
library(Rglpk)
library(quadprog)
library(ROI)
library(ROI.plugin.glpk) 
library(ROI.plugin.quadprog) 
library(ROI.plugin.symphony) 
library(pso) 
library(GenSA) 
library(corpcor) 
library(testthat)
library(nloptr) 
library(MASS)
library(robustbase) 
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)


start_time <- Sys.time()

n_Banks = 20
m_Assets = 10

SP100_daily = read.table("S&P100.csv" , header = TRUE, sep = ",", row.names = 1)
SP100_returns_daily = (SP100_daily[2:length(SP100_daily[,1]),] -  SP100_daily[1:(length(SP100_daily[,1])-1),])/SP100_daily[1:(length(SP100_daily[,1])-1),]
sample_SP100_returns_daily = SP100_returns_daily[,sample(100, m_Assets, replace = FALSE)]

asset_names = colnames(sample_SP100_returns_daily)

q_t = matrix(data = NA, nrow = n_Banks, ncol = m_Assets)
for (i in 1:n_Banks) {
  pspec = portfolio.spec(assets = asset_names)
  pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum = 0.99, max_sum = 1.01)
  pspec <- add.constraint(portfolio=pspec, type="long_only")
  Target_return = rnorm(1, mean= 3, sd = 1.5)
  pspec <- add.constraint(portfolio=pspec, type="return", return_target = Target_return)
  pspec <- add.objective(pspec, type="risk", name="var")
  opt_minvar = optimize.portfolio(sample_SP100_returns_daily,  portfolio = pspec, optimize_method = "DEoptim", trace = TRUE )
  q_t[i,] = opt_minvar$weights
}

end_time <- Sys.time()
print(end_time - start_time)

