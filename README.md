# Risk-Management-of-Portfolio
# eXtensible Time Series. 
library(xts) 
# Quantitative Financial Modelling Framework. 
library(quantmod) 
# Econometric Tools for Performance and Risk Analysis.  
library(PerformanceAnalytics) 
# Object-oriented modeling language for convex optimization library(CVXR) 
# Graphical display of a correlation matrixlibrary(corrplot) library(corrplot)  
# Plotting package 
library(ggplot2) 
# Reshape package for data  
library(reshape2) 
# Setting up begining date and end date  
begin_date <- "2016-01-01" 
end_date <- "2019-01-01" 
# Selection of stocks 
stock_namelist <- c("ASIANPAINT.NS", "DRREDDY.NS", "EICHERMOT.NS","IBULHS GFIN.NS", "HDFCBANK.NS", "RELIANCE.NS","TECHM.NS","HCLTECH.NS") 
# Downloading data  
prices <- xts() 
for (stock_index in 1:length(stock_namelist)) 
 prices <- cbind(prices, Ad(getSymbols(stock_namelist[stock_index],   from = begin_date, to = end_date,  auto.assign = FALSE))) 
# To set the column names of matrix 
colnames(prices) <- stock_namelist 
# To view and set the class of the time-index of a given xts object indexClass(prices) <- "Date" 
# Omitting NA values 
pricesnew <- na.omit(prices) 
prices <- pricesnew

# Understanding structure of data 
str(prices) 
# To view starting portion of data 
head(prices) 
# To view ending portion of data 
tail(prices) 
#Univariate analysis of Data 
plot(prices$ASIANPAINT.NS,col = "blue") 
plot(prices$DRREDDY.NS,col = "red") 
plot(prices$EICHERMOT.NS,col = "green") 
plot(prices$HDFCBANK.NS,col = "violet") 
plot(prices$RELIANCE.NS,col = "orange") 
plot(prices$TECHM.NS,col = "yellow") 
plot(prices$HCLTECH.NS,col = "darkgoldenrod") 
plot(prices$IBULHSGFIN.NS, col = "darkcyan") 
# Computing return of stocks 
X_log <- CalculateReturns(prices, "log")[-1] 
X_lin <- CalculateReturns(prices)[-1] 
# Correlation between stocks 
coeff_R <- cor(X_lin) 
# correlation matrix 
round (coeff_R,3) 
# correlogram  
corrplot(coeff_R, type = "upper", order = "hclust",  
 tl.col = "black", tl.srt = 45) 
# number of stocks 
N <- ncol(X_log)  
# number of days 
T <- nrow(X_log)  
# split data into training and test data 
T_trn <- round(0.7*T) # 70% of data 
X_log_trn <- X_log[1:T_trn, ] 
X_log_tst <- X_log[(T_trn+1):T, ] 
X_lin_trn <- X_lin[1:T_trn, ] 
X_lin_tst <- X_lin[(T_trn+1):T, ] 
# Computing mean and standard deviation of returns 
mu_lin <- colMeans(X_lin_trn) 
X_ <- X_lin_trn - matrix(mu_lin, T_trn, N, byrow = TRUE) #remove mean
 
Sigma_lin <- 1/(T_trn-1) * t(X_) %*% X_ 
mu_log <- colMeans(X_log_trn) 
Sigma_log <- cov(X_log_trn) 
# creating a function for Global Minimum Variance Portfolio portolioGMVP <- function(Sigma) { 
 ones <- rep(1, nrow(Sigma)) 
 Sigma_inv_1 <- solve(Sigma, ones) #same as: inv(Sigma) %*% ones  w <- (1/as.numeric(ones %*% Sigma_inv_1)) * Sigma_inv_1  return(w) 
} 
w_lin <- portolioGMVP(Sigma_lin) 
w_log <- portolioGMVP(Sigma_log) 
Wa <- cbind(w_lin, w_log) 
# Plotting for comparison between weights for different returns 
barplot(t(Wa), col = c("darkblue","darkcyan"), legend = colnames(Wa),   main = "Portfolio allocation for daily returns", xlab = "stocks",  ylab = "weights", beside = TRUE, ylim = c(0,0.6)) 
ret_all_trn <- xts(X_lin_trn %*% Wa, index(X_lin_trn)) 
StdDev.annualized(ret_all_trn) 
# Computing returns weekly 
periodicity(prices) 
prices_weekly <- xts() 
for (i in 1:ncol(prices)) 
 prices_weekly <- cbind(prices_weekly, Cl(to.weekly(prices[, i]))) colnames(prices_weekly) <- colnames(prices) 
indexClass(prices_weekly) <- "Date" 
periodicity(prices_weekly) 
X_weekly_log <- CalculateReturns(prices_weekly, "log")[-1] X_weekly_lin <- CalculateReturns(prices_weekly)[-1] 
T_weekly <- nrow(X_weekly_log) 
T_weekly_trn <- round(0.7*T_weekly) # 70% of data 
X_weekly_log_trn <- X_weekly_log[1:T_weekly_trn, ] 
X_weekly_log_tst <- X_weekly_log[(T_weekly_trn+1):T_weekly, ] X_weekly_lin_trn <- X_weekly_lin[1:T_weekly_trn, ] 
X_weekly_lin_tst <- X_weekly_lin[(T_weekly_trn+1):T_weekly, ] 
mu_weekly_lin <- colMeans(X_weekly_lin_trn) 
Sigma_weekly_lin <- cov(X_weekly_lin_trn)
 
mu_weekly_log <- colMeans(X_weekly_log_trn) 
Sigma_weekly_log <- cov(X_weekly_log_trn) 
w_weekly_lin <- portolioGMVP(Sigma_weekly_lin) 
w_weekly_log <- portolioGMVP(Sigma_weekly_log) 
week <- cbind(w_weekly_lin,w_weekly_log) 
barplot(t(week), col = c("darkcyan", "darkgoldenrod"), legend = colnames( week),  
 main = "Portfolio allocation for weekly returns",   xlab = "stocks", ylab = "dollars", beside = TRUE, ylim = c(0,0.5) ) 
ret_weekly_all_trn <- xts(X_weekly_lin_trn %*% week, index(X_weekly_lin_t rn)) 
StdDev.annualized(ret_weekly_all_trn) 
# Computing returns monthly 
periodicity(prices) 
prices_monthly <- xts() 
for (i in 1:ncol(prices)) 
 prices_monthly <- cbind(prices_monthly, Cl(to.monthly(prices[, i]))) colnames(prices_monthly) <- colnames(prices) 
indexClass(prices_monthly) <- "Date" 
periodicity(prices_monthly) 
X_monthly_log <- CalculateReturns(prices_monthly, "log")[-1] X_monthly_lin <- CalculateReturns(prices_monthly)[-1] 
T_monthly <- nrow(X_monthly_log) 
T_monthly_trn <- round(0.7*T_monthly) # 70% of data 
X_monthly_log_trn <- X_monthly_log[1:T_monthly_trn, ] 
X_monthly_log_tst <- X_monthly_log[(T_monthly_trn+1):T_monthly, ] X_monthly_lin_trn <- X_monthly_lin[1:T_monthly_trn, ] 
X_monthly_lin_tst <- X_monthly_lin[(T_monthly_trn+1):T_monthly, ] 
mu_monthly_lin <- colMeans(X_monthly_lin_trn) 
Sigma_monthly_lin <- cov(X_monthly_lin_trn) 
mu_monthly_log <- colMeans(X_monthly_log_trn) 
Sigma_monthly_log <- cov(X_monthly_log_trn) 
w_monthly_lin <- portolioGMVP(Sigma_monthly_lin) 
w_monthly_log <- portolioGMVP(Sigma_monthly_log) 
month <- cbind(w_monthly_lin, w_monthly_log) 
barplot(t(month), col = c("darkcyan", "darkgoldenrod"), legend = colnames (month), 
 
 main = "Portfolio allocation for monthly returns",   xlab = "stocks", ylab = "dollars", beside = TRUE, ylim = c(0,0.5) ) 
ret_monthly_all_trn <- xts(X_monthly_lin_trn %*% month, index(X_monthly_l in_trn)) 
StdDev.annualized(ret_monthly_all_trn) 
# Comparison between various Portfolio Selection techniques 
mu_final <- colMeans(X_weekly_log_trn) 
Sigma_final <- cov(X_weekly_log_trn) 
# Uniform portfolio 
w_uniform <- rep(1/N, N) 
# Global Minimum Variance Portfolio 
portfolioGMVP <- function(Sigma_final) { 
 w <- Variable(nrow(Sigma_final)) 
prob <- Problem(Minimize(quad_form(w, Sigma_final)),  
 constraints = list(w >= 0, sum(w) == 1))  result <- solve(prob) 
 return(as.vector(result$getValue(w))) 
} 
w_GMVP <- portfolioGMVP(Sigma_final) 
# Markowitz Mean-Variance portfolio 
portfolioMarkowitz <- function(mu_final, Sigma_final, lmd = 0.5) {  w <- Variable(nrow(Sigma_final)) 
 prob <- Problem(Maximize(t(mu_final) %*% w - lmd*quad_form(w, Sigma_fin al)), 
 constraints = list(w >= 0, sum(w) == 1))  result <- solve(prob) 
 return(as.vector(result$getValue(w))) 
} 
w_Markowitz <- portfolioMarkowitz(mu_final, Sigma_final, lmd = 2) 
# Maximum Sharpe Ratio pportfolio 
portfolioMaxSharpeRatio <- function(mu_final, Sigma_final) {  w_ <- Variable(nrow(Sigma_final)) 
 prob <- Problem(Minimize(quad_form(w_, Sigma_final)),  constraints = list(w_ >= 0, t(mu_final) %*% w_ == 1))  result <- solve(prob) 
 return(as.vector(result$getValue(w_)/sum(result$getValue(w_)))) } 
w_maxSR <- portfolioMaxSharpeRatio(mu_final, Sigma_final)

# Comparing all portfolios 
w_meanvar <- cbind(w_uniform, w_GMVP, w_Markowitz, w_maxSR) rownames(w_meanvar) <- colnames(X_weekly_log) 
colnames(w_meanvar) <- c("uniform", "GMVP", "Markowitz", "maxSR") round(w_meanvar , 3) 
# Plotting comparison of portfolios 
barplot(t(w_meanvar), 
 main = "Portfolio allocation", xlab = "stocks", ylab = "weights",  beside = TRUE,  
 legend = colnames(w_meanvar), 
 col = c( "brown", "gold", "coral", "chartreuse3")) # computing returns of all portfolios 
ret_all <- xts(X_weekly_log %*% w_meanvar, index(X_weekly_log)) ret_all_trn <- ret_all[1:T_weekly_trn, ] 
ret_all_tst <- ret_all[-c(1:T_weekly_trn), ] 
# performance of portfolios 
table.AnnualizedReturns(ret_all_trn) 
table.AnnualizedReturns(ret_all_tst) 
# Plotting performance of portfolios 
{ chart.CumReturns(ret_all[ -c(1:9)], main = "Performance of different po rtfolios",  
 wealth.index = TRUE, legend.loc = "topleft", colorset  = rich8equal) 
 addEventLines(xts("training", index(X_lin[T_trn])), srt=90, pos=2, lwd  = 2, col = "darkblue") } 
# Computing the efficient frontier 
w_frontier_trn <- NULL 
lmd_sweep <- exp(seq(-5, 5, by = 0.5)) 
for (lmd in lmd_sweep) 
 w_frontier_trn <- cbind(w_frontier_trn, portfolioMarkowitz(mu_final, Si gma_final, lmd)) 
ret_frontier_trn <- xts(X_weekly_log_trn %*% w_frontier_trn, index(X_week ly_log_trn)) 
mu_frontier_trn <- table.AnnualizedReturns(ret_frontier_trn)[1, ] sd_frontier_trn <- table.AnnualizedReturns(ret_frontier_trn)[2, ] # Plotting efficient frontier 
maxSR <- table.AnnualizedReturns(ret_all_trn[, "maxSR"])[3, ]
 
chart.RiskReturnScatter(ret_all_trn, 
 main = "Efficient frontier ", 
symbolset = c(rep(21, 9), rep(22, 4+3)),  
 colorset = c(1,2,3,4), 
 #colorset = c(rep("red", 9), rep("blue", 4), rep( "green", 3), rep("brown", 3)), 
 bg = "black", 
add.sharpe= 1,xlab = "Risk",ylab = "Return") 
lines(sd_frontier_trn, mu_frontier_trn) 
write.table(ret_all,file ="jsn1.csv",sep = ",")
