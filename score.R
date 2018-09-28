portfolioLeaderboard <- function(res = NA, weights = c(1, 1, 1, 7)) {
    if (length(weights) != 4) stop("argument \"weights\" must have 4 elements")
    
    # sort the vaild scores
    weights <- weights / (sum(weights))
    mask_valid <- res$failure_ratio != 1
    scores <- cbind(res$performance[1],
                    -res$performance[2],
                    -res$cpu_time_average[mask_valid],
                    -res$failure_ratio[mask_valid])
    final_score <- scores %*% weights
    return(final_score)
}

if(!res$error) {
    score <- portfolioLeaderboard(res)
    score
} else {
    res$error_message
}
