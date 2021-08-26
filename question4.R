# Question 4

# ASSUMPTION: We start with $10000 in the beginning
# ASSUMPTION: Risk-free rate is 0%

library(tseries)
library(PerformanceAnalytics)

asset_list = c("AAPL", "IBM", "GOOG", "BP", "XOM", "COST", "GS")
asset_weights = c(0.15,0.2,0.2,0.15,0.1,0.15,0.05)
start_date = "2016-01-01"
end_date = "2016-12-31"
initial_investment = 10000


price_mat = matrix()

for(x in 1:length(asset_list)){
  prices = get.hist.quote(instrument = asset_list[x], start_date, end_date,
                          quote = c("AdjClose"),
                          provider = c("yahoo"), method = NULL,
                          origin = "1899-12-30", compression = "d",
                          retclass = c("zoo"))
  price_mat = cbind(price_mat,prices)
}
price_mat = price_mat[-1,]
price_mat = price_mat[,-1]
colnames(price_mat) = asset_list

log_ret_zoo = diff(log(price_mat))
mean = c()
for(x in 1:ncol(log_ret_zoo)){
  mean = c(mean, mean(log_ret_zoo[,x]))
}

# Question 4a and 4b

portfolio_expected_return = t(as.matrix(asset_weights)) %*% as.matrix(mean)
portfolio_variance = t(as.matrix(asset_weights)) %*% as.matrix(var(log_ret_zoo)) %*% as.matrix(asset_weights)
portfolio_sd = sqrt(portfolio_variance)

# Parametric
five_perc_var = round((-initial_investment * (exp(portfolio_expected_return + (1.645 * portfolio_sd)) - 1)),2)
five_perc_cvar = round((initial_investment * ES(R = log_ret_zoo, p = 0.95, method="gaussian", weights = as.matrix(asset_weights), mu = mean, sigma = as.matrix(var(log_ret_zoo)))),2)
print("As of 2016/12/31")
print(paste("Portfolio 5% Parametric VaR: ", five_perc_var, sep="")) # 156.3
print(paste("Portfolio 5% Parametric CVaR: ", five_perc_cvar, sep="")) # 181.11
print("Assuming the normality of log returns distributions, an investor has a 5% chance of losing the above amounts (or more) per day from an initial $10000 in this portfolio.")

# Historical
five_perc_var = round((-initial_investment * (exp(quantile(log_ret_zoo %*% as.matrix(asset_weights), 0.05)) - 1)),2)
# I'm unable to get historical CVaR - function may be broken
# five_perc_cvar = round((initial_investment * ES(R = log_ret_zoo, p = 0.95, method="gaussian", weights = as.matrix(asset_weights), mu = mean, sigma = as.matrix(var(log_ret_zoo)))),2)

print("As of 2016/12/31")
print(paste("Portfolio 5% Historical VaR: ", five_perc_var, sep="")) # 142.84
# print(paste("Portfolio 5% Historical CVaR: ", five_perc_cvar, sep="")) 
print("Assuming the normality of log returns distributions, an investor has a 5% chance of losing the above amounts (or more) per day from an initial $10000 in this portfolio.")

# It appears that parametric VaR is greater in amplitude than historical VaR

# Question 4c

tangency_mat = matrix(ncol=length(asset_list))
sharpe_mat = matrix(ncol = 2)

for(x in 1:12){
  print(x)
  # temp_mat = matrix(ncol = length(asset_list))
  # colnames(temp_mat) = asset_list
  # rownames(temp_mat) = c('a')
  extent = 0
  for(y in 1:nrow(log_ret_zoo)){
    print(y)
    if(as.numeric(format(index(log_ret_zoo)[y], "%m")) <= x){
      # temp_mat = rbind(temp_mat, log_ret_zoo[1,])
      extent = extent + 1
      
    } else {
      break
    }
  }
  relevant_zoo = log_ret_zoo[(1:extent),]
  
  
  inv_cov = solve(as.matrix(var(relevant_zoo)))
  one_vec = rep(1,length(asset_list))
  top_mat = inv_cov %*% as.matrix(mean)
  bot_val = as.numeric(t(one_vec) %*% top_mat)
  t_vec = top_mat[,1]/bot_val
  tangency_mat = rbind(tangency_mat,t_vec)
  
  # optimized and unoptimized sharpe ratios

  optimized_sharpe = (colMeans(relevant_zoo) %*% t_vec) / sqrt(t(t_vec) %*% var(relevant_zoo) %*% t_vec)
  equal_weights = rep((1/length(asset_list)), length(asset_list))
  unoptimized_sharpe = (colMeans(relevant_zoo) %*% equal_weights) / sqrt(t(equal_weights) %*% var(relevant_zoo) %*% equal_weights)
  sharpe_mat = rbind(sharpe_mat, matrix(c(optimized_sharpe, unoptimized_sharpe),ncol=2))
}

tangency_mat = tangency_mat[-1,]
rownames(tangency_mat) = c(1:12)
print(tangency_mat) 
print("Each row refers to the optimized portfolio weights for the end of each month.")

#           AAPL       IBM        GOOG          BP        XOM       COST        GS
# 1  -0.43536863 3.2796085 -1.82359415  0.05171880 -0.7581224 -3.1509152 3.8366731
# 2  -0.36025464 0.7322928  0.16823404 -0.54546534  0.5023683 -0.6421354 1.1449603
# 3  -0.29418402 0.6333908  0.02375510 -0.36239787  0.4131340 -0.2920627 0.8783647
# 4  -0.19453288 0.5851755 -0.06448141 -0.23420836  0.3974572 -0.2873535 0.7979435
# 5  -0.14237515 0.5694654 -0.15827901 -0.21339928  0.4293112 -0.1606537 0.6759306
# 6  -0.12064342 0.5737869 -0.30565955 -0.27322359  0.5622322 -0.1325678 0.6960753
# 7  -0.08187370 0.5895194 -0.28266210 -0.20664748  0.5066561 -0.1475401 0.6225479
# 8  -0.07341532 0.6101169 -0.29298847 -0.19638455  0.4756965 -0.1311165 0.6080914
# 9  -0.03506871 0.6780492 -0.37428185 -0.18796125  0.4209753 -0.1685409 0.6668282
# 10 -0.02128892 0.6548028 -0.35389800 -0.16815973  0.4035685 -0.1741392 0.6591145
# 11  0.06378765 0.6408777 -0.27324302 -0.10060858  0.3590239 -0.1931171 0.5032794
# 12  0.06904307 0.6474206 -0.28751613 -0.07518275  0.3589300 -0.1987111 0.4860164

sharpe_mat = sharpe_mat[-1,]
rownames(sharpe_mat) = c(1:12)
colnames(sharpe_mat) = c("optimized", "unoptimized")
print(sharpe_mat) 

#      optimized  unoptimized
# 1  -0.34960074 -0.109713212
# 2  -0.10967343 -0.094659136
# 3   0.00158699  0.020075148
# 4   0.04496494  0.000161124
# 5   0.05286521  0.016919705
# 6   0.02743733  0.017700732
# 7   0.03241234  0.042377021
# 8   0.04469004  0.043017467
# 9   0.02641821  0.039015648
# 10  0.03240432  0.037531309
# 11  0.08630164  0.053678750
# 12  0.09517162  0.071259772
