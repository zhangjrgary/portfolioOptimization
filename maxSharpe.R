library(xts)
library(portfolioBacktest)
library(CVXR)
data(prices)
# head(prices)

# No shortsell, leverage =1
# 1yr, reopt month, rebal week
# any(w[i ] + 1e-6 >= 0)
# sum(abs(w[i, ])) <= leverage + 1e-6




portfolio_fun <- function(prices) {
    X <- diff(log(prices))[-1] # log returns
    mu <- colMeans(X)  # compute mean vector
    Sigma <- cov(X)  # compute the SCM

    w <- Variable(nrow(Sigma))
    prob <- Problem(Minimize(quad_form(w, Sigma)),
                    constraints = list(w >= 0, 
                                    t(mu) %*% w == 1), 
                                    sum(abs(w[i, ] <= 1+1e-6)), 
                                    any(w[i, ] + 1e-6 >= 0))
    result <- solve(prob)
    return(as.vector(result$getValue(w)/sum(result$getValue(w))))
    
}
# 
res <- portfolioBacktest(portfolio_fun, prices[[1]], rebalance_every = 5)
if(res$error) {
    res$error_message
} else {
    res$performance
}