decideCounts <- function (object, p.value=.01, lfc=1) {
    pvals <- as.matrix(object$p.value)
    tstat <- as.matrix(object$t)
    coeff <- as.matrix(object$coefficients)
    results <- sign(tstat) * (pvals <= p.value)
    if (lfc > 0)  results <- results * (abs(coeff) >= lfc)
    results <- results[which(apply(results,1,function(x){sum(abs(x))>0})),]
    results
}

