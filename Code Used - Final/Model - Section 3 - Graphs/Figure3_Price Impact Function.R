
library(ggplot2)

# Generating Graph
n_Banks = 20
m_Assets = 10
# Diversification
linkProbability = 0.4
minLinkProbability = 0.1
maxLinkProbability = 0.8
linkProbabilityStep = 0.1

# Generating the balance Sheet 
asset_0 = 80
cash_0 = 20
intial_Price = matrix(data = 1, nrow = m_Assets, ncol = 1)
# Leverage testing factors
leverage = 5
leverage_low_step = 3
leverage_high_step = 8
leverage_test_step = 0.5

# System rules parameters
gamma_factor = 1
gamma = matrix(data = gamma_factor, nrow = n_Banks, ncol = 1)
# Liquidity heterogenity
meanLiquidity = 0.4
liquidity_step = 0.1
max_liquidityStep = 0.4
min_liquidityStep = 0
liquidity_step_sequence_test = 0.1

method_selection = 3
method_selection_list = c(1,2,3)
external_Trade_Dummy = 0

# Shock & Simulation
assetReduction = 0.05
numberIterations = 25
numberSimulations = 500
leverage_warning_factor = 0.95
testing = 0
convergence_index = matrix(data = NA, nrow = numberIterations, ncol =1)
# Set f to 0 if using single simulation
f = 0

# Plot Price Impact


m_Assets = 1
p_0 = 1
daily_market_volume = 0
net_Volume_Traded = 0
external_Trade_Dummy = 0
System_Update_Dummy = 0
System_Q = 100
decision_Volume_Traded_plot = 0:100
decision_Volume_Traded_plot = -decision_Volume_Traded_plot
liquidity_factor_plot = c(0.2, 0.4, 0.8)


Price_Impact(1, 0.4, p_0,System_Q)

Price_Impact = function(decision_Volume_Traded, liquidity_factor, p_0, System_Q) {
  # External trade is the demand for the asset outside of the financial system

    external_Trade = matrix(data = 0, nrow = 1, ncol = 1)
  
  p_t1  = p_0 * exp(liquidity_factor * decision_Volume_Traded/System_Q)  
  

  return(p_t1)
}

a1 = matrix(data = NA, nrow = 101, ncol = 1)
a2 = matrix(data = NA, nrow = 101, ncol = 1)
a3 = matrix(data = NA, nrow = 101, ncol = 1)

for (i in 1:length(decision_Volume_Traded_plot)) {
  a1[i] = Price_Impact(decision_Volume_Traded_plot[i], 0.2, p_0,System_Q)
  a2[i] = Price_Impact(decision_Volume_Traded_plot[i], 0.4, p_0,System_Q)
  a3[i] = Price_Impact(decision_Volume_Traded_plot[i], 0.8, p_0,System_Q)
}


plot(a1,x = -decision_Volume_Traded_plot, type="p", col="blue", pch=0, lty=1, cex = 0.3, ylim=c(0.35,1.1), ylab = "p_t", xlab="Quantity Liquidated" )
points( a2, col="red", pch=1, cex = 0.3)
points(a3, col="green", pch=2, cex = 0.3)
legend(x = 0.1, y = 0.6, legend=c("0.2", "0.4", "0.8"),
       col=c("blue", "red", "green"), lty=1:2, cex=0.8, title = "Liqudity Factor")

price_data = data.frame(a1, a2, a3)


ggplot(test_data, aes(date)) + 
  geom_line(aes(y = var0, colour = "var0")) + 
  geom_line(aes(y = var1, colour = "var1")) 


GraphPriceImpact=ggplot(price_data, aes(0:(length(price_data[,1]) -1)) ) +
  geom_line(aes(y = a1, colour = "a1"), size = 0.7) +
  geom_line(aes(y = a2, colour = "a2"), size = 0.7) +
  geom_line(aes(y = a3, colour = "a3"), size = 0.7) + 
  ggtitle("Price impact depending on quantity") +
  xlab("Quantity liquidated [%]") +
  ylab("Price impact [%]")+
  theme_light() + 
  scale_color_manual(name="Liquidity values",
                     labels = c("0.2", 
                                "0.4", 
                                "0.8"), 
                     values = c("a1"="black", 
                                "a2"="gray45", 
                                "a3"="gray"))+
  theme(
    plot.title = element_text( size=10),
    axis.title.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    legend.title =element_text(size=8),
    legend.text=element_text(size=8)
  )  + 
  scale_fill_grey()
print(GraphPriceImpact) 


ggsave("Price_Impact.pdf", plot = GraphPriceImpact, device = "pdf", path = "/Users/Michal/Dropbox/UNISG/20. Thesis/4. Code/Figures/",
       scale = 1, width = 10, height = 7, units = "cm",
       dpi = 300, limitsize = TRUE)


print(GraphPriceImpact)
ggsave("Graph16.png")

tikz(file = "Graph16.tex", width = 3, height = 2.25)
Graph16
dev.off()
