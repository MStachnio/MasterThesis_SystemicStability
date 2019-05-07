# Notes --------------------------------------------------------------------------------------------
# Date: 28.12.2018
# Last update: 04.05.2019
# Author: Michal Stachnio

# Important notes:


# Packages & Functions (hide) --------------------------------------------------------------------------------------------

library(igraph)

NamingString = function(String, startingValue, size) {
  nameString = matrix("0", nrow = 1, ncol = size, byrow = TRUE)
  for (i in 1:size) {
    nameString[, i] = paste(String, i - 1 + startingValue, sep = "") # sep="" means that there is no space between w and the number
  }
  return (nameString)
}

NamingRows = function(String, data, startingValue) {
  
  # Input Variables:
  # String        = value in "" that will be the name that will be iterated
  # data          = data structure that will have a new name for rows
  # startingValue = allows to start with 0 or 1 etc...
  
  dataNames = matrix("0", nrow = nrow(data), ncol = 1, byrow = TRUE)
  for (i in 1:nrow(dataNames)) {
    dataNames[i, ] = paste(String, i - 1 + startingValue, sep = "") # sep="" means that there is no space between w and the number
  }
  rownames(data) = c(dataNames)
  return (data)
}

NamingCols = function(String, data, startingValue) {
  dataNames = matrix("0", nrow = 1, ncol = ncol(data), byrow = TRUE)
  for (i in 1:ncol(dataNames)) {
    dataNames[, i] = paste(String, i - 1 + startingValue, sep = "") # sep="" means that there is no space between w and the number
  }
  colnames(data) = c(dataNames)
  return (data)
}

# Parameters ------------------------------------------------------------------------------------------------------------------

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
  leverage_high_step = 9
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

daily_market_volume = 0.01
method_selection = 3
external_Trade_Dummy = 0

# Shock & Simulation
assetReduction = 0.05
numberIterations = 25
numberSimulations = 100
leverage_warning_factor = 0.95
testing = 0
convergence_index = matrix(data = NA, nrow = numberIterations, ncol =1)

# Results Matrices

# Generating the bipartiate graph --------------------------------------------------------------------------------------------

GenerateErdosBipartiiate = function(n_Banks, m_Assets, linkProbability) {
  # Generates the graph
  graph = sample_bipartite(n_Banks, m_Assets, p=linkProbability)
  # Changes the form of the vertex for the graph
  V(graph)$shape <- c("square", "circle")[V(graph)$type+1]
  # Deletes the label
  V(graph)$label <- c(NamingString("bank",1,n_Banks), NamingString("asset",1,m_Assets))
  
  # Making sure each bank has at least one asset:
  for (i in 1:n_Banks) {
    if (sum(graph[i, ]) == 0) {
      assetChoice = sample(assetChoiceVector, 1)
      graph = graph + edge(c(i, assetChoice + n_Banks))
    }
  }
  
  # Making sure each asset has at least one bank:
  bankChoiceVector = 1:n_Banks
  for (i in 1:m_Assets) {
    if (sum(graph[, i + n_Banks]) == 0) {
      bankChoice = sample(bankChoiceVector, 1)
      graph = graph + edge(c(i + n_Banks, assetChoice))
    }
  }
  
  
  return(graph)
}

GenerateScaleFreeBipartite = function(n_Banks, m_Assets, linkProbability) {
  
  
  #Generate incidence matrix
  # Generate graph from incidence matrix
  graph_from_incidence_matrix(incidence, directed = FALSE, mode = c("all", "out", "in", "total"), multiple = FALSE, weighted = NULL,add.names = NULL)
  
  # Generates the graph
  graph = sample_bipartite(n_Banks, m_Assets, p=linkProbability)
  # Changes the form of the vertex for the graph
  V(graph)$shape <- c("square", "circle")[V(graph)$type+1]
  # Deletes the label
  V(graph)$label <- c(NamingString("bank",1,n_Banks), NamingString("asset",1,m_Assets))
  
  # Making sure each bank has at least one asset:
  for (i in 1:n_Banks) {
    if (sum(graph[i, ]) == 0) {
      assetChoice = sample(assetChoiceVector, 1)
      graph = graph + edge(c(i, assetChoice + n_Banks))
    }
  }
  
  # Making sure each asset has at least one bank:
  bankChoiceVector = 1:n_Banks
  for (i in 1:m_Assets) {
    if (sum(graph[, i + n_Banks]) == 0) {
      bankChoice = sample(bankChoiceVector, 1)
      graph = graph + edge(c(i + n_Banks, assetChoice))
    }
  }
  
  
  return(graph)
}


# System Rules ------------------------------------------------------------

# DeltaAsset
Cont_Delta_Asset = function(n_Banks, balanceSheet, gamma, target_Leverage, leverage_warning_factor) {
  
  delta_asset = matrix(0, nrow = n_Banks, ncol = 1)
  
  for (i in 1:n_Banks) {
    # bankruptcy check
    if (balanceSheet[i, 1] + balanceSheet[i, 2] - balanceSheet[i, 3] < 0 ) {
      delta_asset[i] = balanceSheet[i, 1]
    } else if (leverage_warning_factor * balanceSheet[i, 1]/balanceSheet[i, 4] > target_Leverage[i]) {
      delta_asset[i] = gamma[i] * balanceSheet[i, 1] * ((1 - (target_Leverage[i] * balanceSheet[i, 4])/balanceSheet[i, 1]))
    } else {
      delta_asset[i] = 0
    }
  }
  return(delta_asset)
}

# Price Impact
Price_Impact = function(decision_Volume_Traded, liquidity_factor, m_Assets, p_0, System_Q,
                        daily_market_volume, net_Volume_Traded, external_Trade_Dummy, 
                        System_Update_Dummy) {
  # External trade is the demand for the asset outside of the financial system
  if (external_Trade_Dummy == 1) {
    external_Trade = matrix(System_Q * daily_market_volume, nrow = m_Assets, ncol = 1)
    external_Trade = NamingRows("asset", external_Trade, 1) 
  } else {
    external_Trade = matrix(data = 0, nrow = nrow(decision_Volume_Traded), ncol = 1)
  }
  
  
  if (length(decision_Volume_Traded) > m_Assets) {
    totalTrade = rowSums(decision_Volume_Traded)
  } else {
    totalTrade = decision_Volume_Traded
  }
  
  net_Volume_Traded = totalTrade + net_Volume_Traded + external_Trade
  
  if (System_Update_Dummy == 1) {
    assign("net_Volume_Traded", net_Volume_Traded, envir = .GlobalEnv)
  }
  p_t1 = p_0 * exp(liquidity_factor * net_Volume_Traded/System_Q)  
  
  return(p_t1)
}

# Liquidation Schedules Functions ------------------------------------------------------------

Lagrangian_Approach2 = function(parameter_initial_values, i, q_t, p_0, liquidity_factor, net_Volume_Traded, 
                                     m_Assets, System_Q, delta_asset, external_Trade_Dummy) {
  
  decision_Volume_Traded = parameter_initial_values
  
  LagrangianValue = colSums(q_t[i, ] * (p_t - p_0 * exp(liquidity_factor * (decision_Volume_Traded + net_Volume_Traded)/System_Q)))
  
  return(LagrangianValue)
}

Lagrangian_Approach3 = function(parameter_initial_values, q_t, p_0, liquidity_factor, net_Volume_Traded, 
                                m_Assets, System_Q, delta_asset, external_Trade_Dummy) {
  
  decision_Volume_Traded = matrix(data = parameter_initial_values, nrow = m_Assets, ncol = (length(parameter_initial_values)/m_Assets))
  
  totalTrade = matrix(data = 0, nrow = m_Assets, ncol = 1)
  
  if (length(decision_Volume_Traded) == m_Assets) {
    totalTrade =  decision_Volume_Traded
  } else {
    for (i in 1:(length(decision_Volume_Traded[1, ]))) {
      totalTrade = totalTrade + decision_Volume_Traded[,i ]
    }
  }
  
  LagrangianValue = colSums(q_t %*% (p_t - p_0 * exp(liquidity_factor * (totalTrade + net_Volume_Traded)/System_Q))) 

  return(LagrangianValue)
}

  

# System actualisation rules --------------------------------------------------------------
System_Update = function(method_selection, gamma, asset_t, equity_t, target_Leverage, q_t, p_t, n_Banks, m_Assets, liquidity_factor, p_0,
                          balanceSheet, external_Trade_Dummy) {

  # 2) Defines the liquidation schedule
  
  # Approach 1: Pro-Rata Liquidation            -----------
  if (method_selection == 1) {
    
    decision_Volume_Traded = matrix(0, nrow = m_Assets, ncol = n_Banks)
    decision_Volume_Traded = NamingRows("asset", decision_Volume_Traded, 1) 
    decision_Volume_Traded = NamingCols("bank", decision_Volume_Traded, 1)
    q_copy = q_t
    
    # Liquidation Quantity - Calculates delta_asset
    delta_asset = Cont_Delta_Asset(n_Banks, balanceSheet, gamma, target_Leverage, leverage_warning_factor) 
    
    bank_index_array = 1:n_Banks
    for (i in 1:n_Banks) {
      # bankruptcy check
      if (balanceSheet[i, 1] + balanceSheet[i, 2] - balanceSheet[i, 3] < 0 || colSums(p_t * q_t[i, ]) < delta_asset[i]) {
        decision_Volume_Traded[, i] = -t(q_t[i, ])
        bank_index_array = bank_index_array[!bank_index_array %in% i]
      }
    }
    for (i in bank_index_array) {
      leftover_delta_asset = delta_asset[i]
      # counts how many assets are held by each bank
      count_Asset_Held = length(which(q_t[i, ] > 0))
      # If it needs to liquidate:
      if (delta_asset[i] != 0) {
        # per asset monetary volume it needs to liquidate
        per_Asset_Liquidation_Quantity = delta_asset[i] / count_Asset_Held
        
        repeat{
          test = 0
          for (j in which(q_copy[i, ] > 0)) {
            # If a bank does not have enough of one asset to liquidate on a pro-rata basis, 
            # it liquidates all it has and then adapts how much it liquidates of other assets.
            if (q_t[i, j] * p_t[j] < per_Asset_Liquidation_Quantity) {
              # all the position in the asset is liquidated
              decision_Volume_Traded[j, i] = - q_t[i, j]
              q_copy[i, j] = 0
              # The monetary value to be liquidated decreases
              leftover_delta_asset = delta_asset[i] + rowSums(t(decision_Volume_Traded[, i] %*% p_t))
              # Adapts the per_Asset_Liquidation_Quantity to new quantity/ liquidation value
              count_Asset_Held = count_Asset_Held - 1
              per_Asset_Liquidation_Quantity = leftover_delta_asset / count_Asset_Held 
            } else {
              test = test + 1
            }
          }
          if (test == length(which(q_copy[i,] > 0))) {
            break
          }
        }
      }
      # If there are some assets for which the bank can fully liquidate, it liquidates equal amounts:
      if (count_Asset_Held > 0) {
        # Identifies the index of the assets where the bank still holds a position:
        assign_Vector = which((q_copy[i, ] + decision_Volume_Traded[, i]) > 0)
        for (q in assign_Vector) {
          decision_Volume_Traded[assign_Vector, i] = -leftover_delta_asset / count_Asset_Held
        }
      }
    }
     
  } 
  # Approach 2: Bank Holding Liqudiation        ----------- 
  else if (method_selection == 2) {
  
    decision_Volume_Traded = matrix(0, nrow = m_Assets, ncol = n_Banks)
    decision_Volume_Traded = NamingRows("asset", decision_Volume_Traded, 1) 
    decision_Volume_Traded = NamingCols("bank", decision_Volume_Traded, 1)
    
    
    # Liquidation quantity
    delta_asset = Cont_Delta_Asset(n_Banks, balanceSheet, gamma, target_Leverage, leverage_warning_factor) 
    convergence_value = 0
    
    for (i in 1:n_Banks) {
      # Liquidate everything if bankrupt
      if (balanceSheet[i, 1] + balanceSheet[i, 2] - balanceSheet[i, 3] < 0 || colSums(p_t * q_t[i, ]) < delta_asset[i]) {
        decision_Volume_Traded[, i] = -t(q_t[i, ])
        convergence_value = convergence_value + 1
      # if not bankrupt
      } else if (delta_asset[i] == 0) {
        decision_Volume_Traded[, i] = matrix(data = 0, ncol = m_Assets, nrow = 1)
        convergence_value = convergence_value + 1
      } else {
        
        parameter_initial_values = matrix( c(-q_t[i,]), nrow =  m_Assets, ncol = 1)
        A = matrix(c(-p_t, diag(m_Assets), -diag(m_Assets)), nrow = (2*m_Assets +1), ncol = m_Assets, byrow= TRUE) 
        fuzz = -1e-10
        b = c(delta_asset[i], -q_t[i,], rep(0, m_Assets)) + fuzz
        
        OptimalValues = constrOptim(theta = parameter_initial_values, f = Lagrangian_Approach2, grad = NULL, ui = A, b, 
                                              i = i, q_t = q_t, p_0 = p_0, liquidity_factor = liquidity_factor, net_Volume_Traded = net_Volume_Traded,
                                              m_Assets = m_Assets, System_Q = System_Q, delta_asset = delta_asset, external_Trade_Dummy = external_Trade_Dummy)
        
        # TODO !!!# Introduce a correction for values > 0 s.t. it never increases !
        
        decision_Volume_Traded[, i] = OptimalValues$par
        
        if (OptimalValues$convergence == 0) {
          convergence_value = convergence_value + 1  
        }
        
      }
    }
    if (convergence_value == n_Banks) {
      assign("convergence_value", 1, envir = .GlobalEnv)
    } else {
      assign("convergence_value", 0, envir = .GlobalEnv)
    }
  } 
  
  # Approach 3: Perfect Information Liquidation ----------- 
  else {
    
    decision_Volume_Traded = matrix(0, nrow = m_Assets, ncol =  n_Banks)
    decision_Volume_Traded = NamingRows("asset", decision_Volume_Traded, 1) 
    decision_Volume_Traded = NamingCols("bank", decision_Volume_Traded, 1)
    
    initial_bank_index_array = 1:n_Banks
    bank_index_array = initial_bank_index_array
    bank_no_liquidation_array = initial_bank_index_array
    bank_no_bankruptcy_array = initial_bank_index_array
    
    # Liquidation quantity
    delta_asset = Cont_Delta_Asset(n_Banks, balanceSheet, gamma, target_Leverage, leverage_warning_factor) 
    
    for (i in 1:n_Banks) {
      # Liquidate everything if bankrupt
      if (balanceSheet[i, 1] + balanceSheet[i, 2] - balanceSheet[i, 3] <= 0  || colSums(p_t * q_t[i, ]) <= delta_asset[i]) {
        bank_no_bankruptcy_array = bank_no_bankruptcy_array[!bank_no_bankruptcy_array %in% i]
        bank_index_array = bank_index_array[!bank_index_array %in% i]
      } else if (delta_asset[i] == 0) {
        bank_no_liquidation_array = bank_no_liquidation_array[!bank_no_liquidation_array %in% i]
        bank_index_array = bank_index_array[!bank_index_array %in% i]
      }
    }
    
    # Volume traded for bankrupt banks
    bankrupt_bank_array = initial_bank_index_array[!initial_bank_index_array %in% bank_no_bankruptcy_array]
    bankruptcy_Liquidation_Volume = matrix(q_t[bankrupt_bank_array, ], nrow = length(bankrupt_bank_array), ncol = m_Assets)
    
    # Volume traded for no liquidation banks
    bank_no_liquidation_array = initial_bank_index_array[!initial_bank_index_array %in% bank_no_liquidation_array]
    no_Liquidation_Volume = matrix(0, nrow = length(bank_no_liquidation_array), ncol = m_Assets)
    
    # Volume traded for non-bankrupt liquidation banks
    if (length(bank_index_array) > 0 ) {
      parameter_initial_values = matrix( t(-q_t[bank_index_array,]), nrow =  m_Assets* length(bank_index_array), ncol = 1)
      
      # Creating the Constraint Matrix A
      A1 = matrix(c(-p_t, rep(0, (length(bank_index_array)  - 1) * m_Assets)), nrow = 1, ncol = length(bank_index_array) * m_Assets)
      if (length(bank_index_array)> 1) {
        for (n in 2:length(bank_index_array)) {
          A1 = c(A1, c(rep(0, (n - 1)*m_Assets), -p_t, rep(0, (length(bank_index_array) - n)*m_Assets )))
        }
      }
        
      A1 = matrix(A1, nrow = length(bank_index_array), ncol = length(bank_index_array) * m_Assets, byrow = TRUE)
      A2 = diag(length(bank_index_array) * m_Assets)
      A3 = -diag(length(bank_index_array) * m_Assets)
      A = rbind(A1, A2, A3)
      
      # Creating contraints b
      fuzz = -1e-10
      b = c(delta_asset[bank_index_array], -t(q_t[bank_index_array,]), rep(0, m_Assets*length(bank_index_array))) + fuzz
      
      
      parameter_initial_values = matrix(t(- q_t[bank_index_array, ]), nrow = m_Assets * length(bank_index_array), ncol = 1)
      OptimalValues = constrOptim(theta = parameter_initial_values, f = Lagrangian_Approach3, grad = NULL,A, b, 
                                    q_t = q_t, p_0 = p_0, liquidity_factor = liquidity_factor, net_Volume_Traded = (net_Volume_Traded + colSums(bankruptcy_Liquidation_Volume)),
                                    m_Assets = m_Assets, System_Q = System_Q, delta_asset = delta_asset[bank_index_array], external_Trade_Dummy = external_Trade_Dummy)
      
      
      
      # TODO !!!# Introduce a correction for values > 0 s.t. it never increases !

      if (OptimalValues$convergence == 0) {
        assign("convergence_value", 1, envir = .GlobalEnv)
      } else {
        assign("convergence_value", 0, envir = .GlobalEnv)
      }
    } else {
      assign("convergence_value", 1, envir = .GlobalEnv)
    }
    # Updating the volumes traded results
    if (length(bank_index_array) > 0 ) {
      decision_Volume_Traded[ , bank_index_array] = t(matrix(data = OptimalValues$par, nrow = length(OptimalValues$par)/m_Assets, ncol = m_Assets, byrow = TRUE))
    }
    decision_Volume_Traded[ , bankrupt_bank_array] = -t(bankruptcy_Liquidation_Volume)
    decision_Volume_Traded[ , bank_no_liquidation_array] = t(no_Liquidation_Volume)
    
  }
  
  # 3) Calculate the price impact of decision_Volume_Traded
  p_t = Price_Impact(decision_Volume_Traded, liquidity_factor, m_Assets, p_0, System_Q, daily_market_volume, 
                     net_Volume_Traded, external_Trade_Dummy, System_Update_Dummy = 1)
  
  # 4) Settle the cash of the trade
  balanceSheet[, 2] = balanceSheet[, 2] - t(t(p_t) %*% decision_Volume_Traded)
  
  # 5) Reduce the quantities of holdings of banks
  q_t = q_t + t(decision_Volume_Traded)
  
  # 6) reupdate the asset values
  balanceSheet[, 1] = q_t %*% p_t
  
  # 7) Calculate the equity
  balanceSheet[, 4] = balanceSheet[, 1] + balanceSheet[, 2] - balanceSheet[, 3]
  
  # The funciton should return: the prices, the balance sheets, the quantities, 
  
  return(list("balanceSheet" = balanceSheet, "Quantities" = q_t, "Prices" = p_t, "decision_Volume_Traded" = decision_Volume_Traded))
}

# On top of this: 
# - monitoring of the bankruptcy rate, the delta asset sold, the total equity of the sstem
# - do a loop that stops once there are no more bankruptcies
# - make a loop so that it does multiple simulations and collects aggregate data on the simulations
# - 


# Execution Code ------------------------------------------------------------
# If testing, set as comment until "STOP"

# For Leverage
# leverage_test_index = seq(leverage_low_step, leverage_high_step, by = leverage_test_step)
# contagion_probability_simulation = array(data = NA, dim = c(length(leverage_test_index), numberSimulations,3 ))
# system_loss_simulation = array(data = NA, dim = c(length(leverage_test_index), numberSimulations,3 ))
# for (method_selection in c(1, 2, 3)) {
#   f = 0
#   for (leverage in leverage_test_index) {
#   f = f + 1
  
  
  #
  #
# # For Diversification Degrees
# linkProbability_test_index = seq(minLinkProbability, maxLinkProbability, linkProbabilityStep)
# contagion_probability_simulation = array(data = NA, dim = c(length(linkProbability_test_index), numberSimulations,3 ))
# system_loss_simulation = array(data = NA, dim = c(length(linkProbability_test_index), numberSimulations,3 ))
# for (method_selection in c(1, 2, 3)) {
#   f = 0
#   for (linkProbability in linkProbability_test_index) {
#     f = f + 1
  #  
  #
# # For Liquidity Heterogenity
LiquidityHeterogenity_test_index = seq(min_liquidityStep, max_liquidityStep, liquidity_step)
contagion_probability_simulation = array(data = NA, dim = c(length(LiquidityHeterogenity_test_index), numberSimulations,3 ))
system_loss_simulation = array(data = NA, dim = c(length(LiquidityHeterogenity_test_index), numberSimulations,3 ))
  for (method_selection in c(1, 2, 3)) {
  f = 0
  for (liquidity_step_sequence_test in LiquidityHeterogenity_test_index) {
    f = f + 1


# 
#   
#   
# 
# 
   
    contagion_ratio = matrix(data = 0, ncol = numberSimulations, nrow = 1)
    asset_loss = matrix(data = 0, ncol = numberSimulations, nrow = 1)
    for (sim in 1:numberSimulations) {
#       
# STOP
      
      # Part 1: Setting up the system -------------------
        
        # Generates the graph, makes sure it's all connected
        repeat{
          graph1 = GenerateErdosBipartiiate(n_Banks, m_Assets, linkProbability)
          if (vertex_connectivity(graph1, source = NULL, target = NULL, checks = TRUE) > 0) {
            break
          }
        }
        # plot(graph1, vertex.color=c("orange", "green")[1 + (V(graph1)$type == "TRUE")], vertex.size = 5, vertex.label = NA) 
        
        
        # Adapting parameters for simulation
          # Liquidity Heterogenity
          step_liqudity = liquidity_step_sequence_test * 2/m_Assets
          liquidity_factor = matrix(NA, nrow = m_Assets, ncol = 1)
          liquidity_factor_values = seq(meanLiquidity - liquidity_step_sequence_test, meanLiquidity + liquidity_step_sequence_test, by = step_liqudity)
          if (length(liquidity_factor_values) < m_Assets) {
            liquidity_factor = sample(liquidity_factor_values, size = m_Assets, replace = TRUE)
          } else {
            liquidity_factor = sample(liquidity_factor_values, size = m_Assets, replace = FALSE)
          }
          
          # Leverage
          liabilities_0 = asset_0 + cash_0 - asset_0/leverage 
          
        # Create the k_i vector that has the degrees of each bank
        k_i = matrix(data = NA, ncol = 1, nrow = n_Banks)
        for (i in 1:n_Banks) {
          k_i[i] = sum(graph1[i])
        }
        k_i = NamingRows("bank", k_i, 1)
        
        # Create the l_j vector that has the degrees of each asset
        l_j = matrix(data = NA, ncol = 1, nrow = m_Assets)
        for (j in 1:m_Assets) {
          l_j[j] = sum(graph1[j+n_Banks])
        }
        l_j = NamingRows("asset", l_j, 1)
        
        # Populating the balanceSheets
        balanceSheet = matrix(data = NA, nrow = n_Banks, ncol = 4)
        colnames(balanceSheet) = c("Assets", "Cash", "Liabilities", "Equity")
        balanceSheet = NamingRows("bank", balanceSheet, 1) 
        balanceSheet[ ,1] = asset_0
        balanceSheet[ ,2] = cash_0
        balanceSheet[ ,3] = liabilities_0
        equity_0 = asset_0 + cash_0 - liabilities_0
        balanceSheet[ ,4] = equity_0
        
        
        # defining the initial price of assets:
        p_0 = matrix(data = intial_Price, nrow = m_Assets, ncol = 1)
        p_0 = NamingRows("asset", p_0, 1) 
        
        # The matrix of asset quantities owned
        q_0 = matrix(data = 0, nrow = n_Banks, ncol = m_Assets )
        q_0 = NamingRows("bank", q_0, 1)
        q_0 = NamingCols("asset", q_0, 1)
        
        # Assigns the initial quantities owned by each bank of all assets
        for (i in 1:n_Banks) {
          for (j in 1:m_Assets) {
            if (graph1[i, j+ n_Banks] == 1) {
              q_0[i, j] = balanceSheet[ i,1] / (k_i[i] * p_0[j])
            } 
          }
        }
        System_Q = rowSums(t(q_0))
        
        # Defining the termporal variables:
        q_t = q_0
        p_t = p_0
        k_t = k_i
        balanceSheet[, 1] = asset_0
        equity_t = balanceSheet[, 4]
        target_Leverage = asset_0 / balanceSheet[, 4] # Setting the target leverage at the initial leverage level
        net_Volume_Traded = matrix(0, nrow = m_Assets, ncol = 1) 
        bankruptBanks = 0
      
      
      # Part 2: Simulating a system -------------------
      
      # Shocking the system via price reduction
      assetChoice = sample(assetChoiceVector, 1)
      p_t[assetChoice] = (1 - assetReduction) * p_t[assetChoice]
      p_0 = p_t
      
      # Updating asset values
      balanceSheet[, 1] = q_t %*% p_t
      
      # Updating equity values
      balanceSheet[, 4] = balanceSheet[, 1] + balanceSheet[, 2] - balanceSheet[, 3]
      asset_evolution = matrix(0, nrow = numberIterations, ncol =1)
      prices_evolution = matrix(0, nrow = numberIterations, ncol = m_Assets)
      decision_Volume = array(data = NA, dim = c(n_Banks, m_Assets, numberIterations))
      
      # Simulating one system
      for (i in 1:numberIterations) {
        print("iteration:")
        print(i)
        system_Update_Values = System_Update(method_selection, gamma, balanceSheet[, 1], balanceSheet[, 4], target_Leverage, q_t, p_t, n_Banks, m_Assets, liquidity_factor, p_0,
                      balanceSheet, external_Trade_Dummy)
        asset_evolution[i] = sum(balanceSheet[, 1])
        prices_evolution[i,] = t(p_t)
        
        balanceSheet = system_Update_Values$balanceSheet
        # convergence_index[i] = convergence_value
        print("q_t")
        print(q_t)
        q_t = system_Update_Values$Quantities
        p_t = system_Update_Values$Prices
        decision_Volume[,,i] = t(system_Update_Values$decision_Volume_Traded)
        print("asset_evolution:")
        print(asset_evolution[i])
        plot(asset_evolution[1:i] , type="p", col="black") 
      }
      
      # TESTS ---------------
      # Test if it always converges ?
      # if ((min(convergence_index) != 1) && (method_selection != 1)) {
      #   print("There were some non-convergences!!")
      # }
      bankruptBanks = length(which(balanceSheet[,1] == 0))
      contagion_ratio[sim] = bankruptBanks/n_Banks
      asset_loss[sim] = -(asset_0 * n_Banks - sum(balanceSheet[,1]))

      
      
    #   print("sim:")
    #   print(sim)
    #   print("contagion ratio:")
    #   print(contagion_ratio[sim])
    #   print("asset loss:")
    #   print(asset_loss[sim])
    # #   
    # 
    # 
      
# For Testing, put the below as comments Util STOP !!

    }
    contagion_probability_simulation[f, , method_selection] = contagion_ratio
    system_loss_simulation[f, , method_selection] = asset_loss
    print("numberSimulation")
    print(f)
  }

}
# STOP
      
# RESULT SECTION -------------------------
setwd("/Users/Michal/Dropbox/UNISG/20. Thesis/4. Code")
      
index = LiquidityHeterogenity_test_index
      
D = dim(contagion_probability_simulation) 
write.table(t(dim(contagion_probability_simulation)), file="contagion_probability_simulation.csv",sep=",", 
            quote=FALSE,row.names=FALSE,col.names=FALSE) 
Z1 = as.vector(contagion_probability_simulation[,,1]) 
Z2 = as.vector(contagion_probability_simulation[,,2]) 
Z3 = as.vector(contagion_probability_simulation[,,3]) 
Z = rbind(Z1, Z2, Z3)
write.table(Z,file="contagion_probability_simulation.csv",append=TRUE,sep=",", 
            quote=FALSE,row.names=FALSE,col.names=FALSE) 

D = dim(system_loss_simulation) 
write.table(t(dim(system_loss_simulation)), file="system_loss_simulation.csv",sep=",", 
            quote=FALSE,row.names=FALSE,col.names=FALSE) 
Z1 = as.vector(system_loss_simulation[,,1]) 
Z2 = as.vector(system_loss_simulation[,,2]) 
Z3 = as.vector(system_loss_simulation[,,3]) 
Z = rbind(Z1, Z2, Z3)
write.table(Z,file="system_loss_simulation.csv",append=TRUE,sep=",", 
            quote=FALSE,row.names=FALSE,col.names=FALSE) 



# Read the results - Contagion Probability
dimB<-scan(file="contagion_probability_simulation.csv",sep=",",nlines=1) 
dataB<-matrix(scan(file="contagion_probability_simulation.csv",sep=",",skip=1), nrow = 3, byrow =  TRUE) 
contagion_probability_simulation_read<-array(t(dataB),dimB) 

# Present in a plot:
a1 = rowSums(contagion_probability_simulation_read[,,1])/ numberSimulations
a2 = rowSums(contagion_probability_simulation_read[,,2])/ numberSimulations
a3 = rowSums(contagion_probability_simulation_read[,,3])/ numberSimulations

# To change depending on the parameter evaluated !!
x = 1:length(index)
plot(x, a1, type="o", col="blue", pch=0, lty=1, ylim=c(0,1), ylab = "contagion_probability",xaxt = "n", xlab="leverage_test_index" )
points( a2, col="red", pch=1)
lines(a2, col="red", lty=2)
points(a3, col="green", pch=2)
lines(a3, col="green", lty=2)
axis(side = 1, at= seq(1,length(index),1), index)
legend(x = 3, y = 1, legend=c("Pro-rata", "Bank Holding Optim", "System Optim"),
       col=c("blue", "red", "green"), lty=1:2, cex=0.8)
# Note: do box graphs for each of the methods !!


# Read the results - System Loss
dimB<-scan(file="system_loss_simulation.csv",sep=",",nlines=1) 
dataB<-matrix(scan(file="system_loss_simulation.csv",sep=",",skip=1), nrow = 3, byrow =  TRUE) 
system_loss_simulation_read<-array(t(dataB),dimB)

a1 = rowSums(system_loss_simulation_read[,,1])/ numberSimulations
a2 = rowSums(system_loss_simulation_read[,,2])/ numberSimulations
a3 = rowSums(system_loss_simulation_read[,,3])/ numberSimulations


# To change depending on the parameter evaluated !!
x = 1:length(index)
plot(x, a1, type="o", col="blue", pch=0, lty=1, ylim=c(0,-1700), ylab = "system_loss",xaxt = "n", xlab="leverage_test_index" )
points( a2, col="red", pch=1)
lines(a2, col="red", lty=2)
points(a3, col="green", pch=2)
lines(a3, col="green", lty=2)
axis(side = 1, at= seq(1,length(index),1), index)
legend(x = 3, y = -1700, legend=c("Pro-rata", "Bank Holding Optim", "System Optim"),
       col=c("blue", "red", "green"), lty=1:2, cex=0.8)
# Note: do box graphs for each of the methods !!


