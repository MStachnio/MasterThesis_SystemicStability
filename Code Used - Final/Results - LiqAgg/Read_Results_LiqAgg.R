
library(igraph)
library(alabama)
library(ggplot2)

setwd("/Users/Michal/Dropbox/UNISG/20. Thesis/4. Code/Code Used - Final/Results - LiqAgg/")  

# Parameters----------------------------------------------------------------------------------------

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
numberSimulations = 100
leverage_warning_factor = 0.95
testing = 0
convergence_index = matrix(data = NA, nrow = numberIterations, ncol =1)
# Set f to 0 if using single simulation
f = 0


index = c(0.5, 0.75, 1)
    


# CONTAGION PROBABILITY ----------------------------------------------------------------------------------------

# Read the results - Contagion Probability
dimB<-scan(file="contagion_probability_simulation.csv",sep=",",nlines=1) 
dataB<-matrix(scan(file="contagion_probability_simulation.csv",sep=",",skip=1), nrow = 3, byrow =  TRUE) 
contagion_probability_simulation_read<-array(t(dataB),dimB) 

# Present in a plot:
a1 = rowSums(contagion_probability_simulation_read[,,1])/ numberSimulations
a2 = rowSums(contagion_probability_simulation_read[,,2])/ numberSimulations
a3 = rowSums(contagion_probability_simulation_read[,,3])/ numberSimulations
x = index
# Note: do box graphs for each of the methods !!
df1 = data.frame(x = index, y = a1, Method = as.factor("Pro-rata"))
df2 = data.frame(x = index, y = a2, Method = as.factor("Bank holding"))
df3 = data.frame(x = index, y = a3, Method = as.factor("System optim"))
info =  rbind(df1, df2, df3)

Graph_title = "Bankruptcy percentage on liquidation aggressivity"
x_lab_title = "Liquidation aggressivity"
y_lab_title = "Bankruptcy percentage"
save_title = "Gamma_Probability"

Contagion_Probability_Leverage = ggplot(info, aes(x,y , group = Method)) +  
  geom_line(aes( linetype = Method, color = Method), size = 0.7) + 
  geom_point(aes(shape = Method, color = Method), size =2.25, stroke = 0.75) +
  ggtitle(Graph_title) +
  xlab(x_lab_title) +
  ylab(y_lab_title) +
  theme_light() +
  scale_x_continuous(breaks = seq(0.5, 1, 0.25), lim = c(0.5, 1)) + 
  scale_fill_grey() +
  scale_color_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size=12),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.title =element_text(size=11),
    legend.text = element_text(size=11)) 
print(Contagion_Probability_Leverage) 
ggsave(paste(save_title, ".pdf", sep =""), plot = Contagion_Probability_Leverage, device = "pdf", path = "/Users/Michal/Dropbox/UNISG/20. Thesis/4. Code/Figures/",
       scale = 1, width = 12, height = 7, units = "cm",
       dpi = 300, limitsize = TRUE)


# System Loss ----------------------------------------------------------------------------------------
# Read the results - System Loss
dimB<-scan(file="system_loss_simulation.csv",sep=",",nlines=1) 
dataB<-matrix(scan(file="system_loss_simulation.csv",sep=",",skip=1), nrow = 3, byrow =  TRUE) 
system_loss_simulation_read<-array(t(dataB),dimB)

a1 = rowSums(system_loss_simulation_read[,,1])/ numberSimulations
a2 = rowSums(system_loss_simulation_read[,,2])/ numberSimulations
a3 = rowSums(system_loss_simulation_read[,,3])/ numberSimulations

df1 = data.frame(x = index, y = -a1, Method = as.factor("Pro-rata"))
df2 = data.frame(x = index, y = -a2, Method = as.factor("Bank holding"))
df3 = data.frame(x = index, y = -a3, Method = as.factor("System optim"))
info =  rbind(df1, df2, df3)

Graph_title = "Asset loss on liquidation aggressivity"
y_lab_title = "Asset loss"
save_title = "Gamma_Loss"


Contagion_Loss_Leverage = ggplot(info, aes(x,y , group = Method)) +  
  geom_line(aes( linetype = Method, color = Method), size = 0.7) + 
  geom_point(aes(shape = Method, color = Method), size =2.25, stroke = 0.75) +
  ggtitle(Graph_title) +
  xlab(x_lab_title) +
  ylab(y_lab_title) +
  theme_light() +
  scale_x_continuous(breaks = seq(0.5, 1, 0.25), lim = c(0.5, 1)) + 
  scale_fill_grey() +
  scale_color_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size=12),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.title =element_text(size=11),
    legend.text = element_text(size=11)) 
print(Contagion_Probability_Leverage) 
ggsave(paste(save_title, ".pdf", sep =""), plot = Contagion_Loss_Leverage, device = "pdf", path = "/Users/Michal/Dropbox/UNISG/20. Thesis/4. Code/Figures/",
       scale = 1, width = 12, height = 7, units = "cm",
       dpi = 300, limitsize = TRUE)




# System Loss Data Points ------------------


y_lab_title = "Asset loss"
Graph_title = "Asset loss outcome distribution for pro-rata"

# Plotting per method type:
# Method 1
b1 = c(system_loss_simulation_read[,,1])
index_values = as.factor(rep(index,numberSimulations))
method1 = rep("pro-Rata", length(c(b1)))
method1_data = data.frame(b1, index_values, method1)


M1 =  ggplot(method1_data, aes(x=index_values, y=b1, )) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.4, stackratio = 1, dotsize =10) + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=0.4), geom="crossbar", width=0.3) +
  ggtitle(Graph_title) +
  theme_light() +
  xlab(x_lab_title) +
  ylab(y_lab_title) +
  scale_fill_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size=12),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.title =element_text(size=11),
    legend.text = element_text(size=11)) 
print(M1)

ggsave(paste(save_title, "_M1", ".pdf", sep =""), plot = M1, device = "pdf", path = "/Users/Michal/Dropbox/UNISG/20. Thesis/4. Code/Figures/",
       scale = 1, width = 20, height = 7, units = "cm",
       dpi = 300, limitsize = TRUE)


# Method 2

Graph_title = "Asset loss outcome distribution for bank holding"


b2 = c(system_loss_simulation_read[,,2])
index_values = as.factor(rep(index,numberSimulations))
method2 = rep("Bank Holding", length(c(b2)))
method2_data = data.frame(b2, index_values, method2)


M2 =  ggplot(method2_data, aes(x=index_values, y=b2, )) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.4, stackratio = 1, dotsize =10) + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=0.4), geom="crossbar", width=0.3) +
  ggtitle(Graph_title) +
  theme_light() +
  xlab(x_lab_title) +
  ylab(y_lab_title) +
  scale_fill_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size=12),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.title =element_text(size=11),
    legend.text = element_text(size=11)) 
print(M2)

ggsave(paste(save_title, "_M2", ".pdf", sep =""), plot = M2, device = "pdf", path = "/Users/Michal/Dropbox/UNISG/20. Thesis/4. Code/Figures/",
       scale = 1, width = 20, height = 7, units = "cm",
       dpi = 300, limitsize = TRUE)


# Method 3
Graph_title = "Asset loss outcome distribution for system optim"

b3 = c(system_loss_simulation_read[,,3])
index_values = as.factor(rep(index,numberSimulations))
method3 = rep("System Optim", length(c(b3)))
method3_data = data.frame(b3, index_values, method3)

M3 =  ggplot(method3_data, aes(x=index_values, y=b3 )) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.4, stackratio = 1, dotsize =10) + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=0.4), geom="crossbar", width=0.3) +
  ggtitle(Graph_title) +
  theme_light() +
  xlab(x_lab_title) +
  ylab(y_lab_title) +
  scale_fill_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size=12),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.title =element_text(size=11),
    legend.text = element_text(size=11)) 
print(M3)

ggsave(paste(save_title, "_M3", ".pdf", sep =""), plot = M3, device = "pdf", path = "/Users/Michal/Dropbox/UNISG/20. Thesis/4. Code/Figures/",
       scale = 1, width = 20, height = 7, units = "cm",
       dpi = 300, limitsize = TRUE)
