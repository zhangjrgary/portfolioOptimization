library(PerformanceAnalytics)
library(xts)
library(portfolioBacktest)
library(CVXR)

data(prices)

# head(prices)

# No shortsell, leverage =1
# 1yr, reopt month, rebal week
# any(w[i ] + 1e-6 >= 0)
# sum(abs(w[i, ])) <= leverage + 1e-6

# Rebalance one time 
maxSharpe <- function(mu, Sigma) {
    
    w <- Variable(nrow(Sigma))
    constrain1 <- w >= 0
    constrain2 <- t(mu) %*% w == 1
    constrain3 <- w[1:nrow(Sigma), ] > 0
    #constrain4 <- sum(abs(w[1:nrow(Sigma), ])) <= 1
    # sum(abs(w[] <= 1+1e-6))
    
    prob <- Problem(Minimize(quad_form(w, Sigma)),
                    constraints = list(constrain1, 
                                       constrain2,
                                       constrain3
                                       #constrain4
                    )
    )
    result <- solve(prob)
    return(as.vector(result$getValue(w)/sum(result$getValue(w))))
}

portfolio_fun <- function(prices) {
    # initial
    X_log <- diff(log(prices))[-1] # log returns
    T <- nrow(X_log)  # number of days
    
    T_trn <- round(0.4*T)
    X_log_trn <- X_log[1:T_trn, ]
    X_log_tst <- X_log[(T_trn + 1):T, ]
    
    mu <- colMeans(X_log_trn)
    Sigma <- cov(X_log_trn)
    
    w_rolling <- X_log
    w_rolling[] <- NA
    
    # rolling index
    rebal_indices <- T_trn + endpoints(X_log_tst, on = "weeks")
    #print(rebal_indices)
    index(X_log)[rebal_indices]
    
    # lookback <- 10*20  # maximum value is: floor(T_trn/20)*20
    #lookback <-  floor(T_trn/20)*20
    lookback <- 25
    for (i in 1:length(rebal_indices)) {
        X_ <- X_log[(rebal_indices[i] - lookback + 1):rebal_indices[i], ]
        mu <- colMeans(X_)
        Sigma <- cov(X_)
        w_rolling[rebal_indices[i], ] <- maxSharpe(mu, Sigma)
    }
    w_rolling <- na.omit(w_rolling)
    #print(w_rolling)
    return(w_rolling[nrow(w_rolling), ])
}

res <- portfolioBacktest(portfolio_fun, prices[[1]], rebalance_every = 5)
res$performance
res$cpu_time
res$failure_ratio
res$error_message
