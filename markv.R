library(PerformanceAnalytics)
library(xts)
library(portfolioBacktest)
library(CVXR)

#data(prices)

portfolio_fun <- function(prices) {
    X <- diff(log(prices))[-1]  # compute log returns
    mu <- colMeans(X)  # compute mean vector
    Sigma <- cov(X)  # compute the SCM
    # design mean-variance portfolio
    w <- Variable(nrow(Sigma))
    prob <- Problem(Maximize(t(mu) %*% w - 3.98*quad_form(w, Sigma)),
                    constraints = list(w >= 0, 
                                       sum(w) == 1))
    result <- solve(prob)
    return(as.vector(result$getValue(w)))
}

res <- portfolioBacktest(portfolio_fun, prices[[1]], rebalance_every = 5)
res$performance
res$cpu_time
