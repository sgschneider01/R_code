my.decideTests <- function (object, method = "separate", adjust.method = "BH", 
    p.value = 0.01, lfc = 1) {
    if (!is(object, "MArrayLM")) 
        stop("Need MArrayLM object")
    if (is.null(object$t)) 
        object <- eBayes(object)
    method <- match.arg(method, c("separate", "global", "hierarchical", 
        "nestedF"))
    adjust.method <- match.arg(adjust.method, c("none", "bonferroni", 
        "holm", "BH", "fdr", "BY"))
    if (adjust.method == "fdr") 
        adjust.method <- "BH"

    switch(method, separate = {
        p <- as.matrix(object$p.value)
# Since we only need the sign anyway, we use coefficients instead - object$t has not been combined with new ids
#        tstat <- as.matrix(object$t)  
        tstat <- as.matrix(object$coefficients)
        for (j in 1:ncol(p)) {
            o <- !is.na(p[, j])
            p[o, j] <- p.adjust(p[o, j], method = adjust.method)
        }
        results <- new("TestResults", sign(tstat) * (p < p.value))
    }, global = {
        p <- as.matrix(object$p.value)
# Since we only need the sign anyway, we use coefficients instead - object$t has not been combined with new ids
#        tstat <- as.matrix(object$t)  
        tstat <- as.matrix(object$coefficients)
        o <- !is.na(p)
        p[o] <- p.adjust(p[o], method = adjust.method)
        results <- new("TestResults", sign(tstat) * (p < p.value))
    }, hierarchical = {
        if (any(is.na(object$F.p.value))) 
            stop("Can't handle NA p-values yet")
        sel <- p.adjust(object$F.p.value, method = adjust.method) < 
            p.value
        i <- sum(sel, na.rm = TRUE)
        n <- sum(!is.na(sel))
        a <- switch(adjust.method, none = 1, bonferroni = 1/n, 
            holm = 1/(n - i + 1), BH = i/n, BY = i/n/sum(1/(1:n)))
        results <- new("TestResults", array(0, dim(object$t)))
        dimnames(results) <- dimnames(object$coefficients)
        if (any(sel)) 
            results[sel, ] <- classifyTestsP(object[sel, ], p.value = p.value * 
                a, method = adjust.method)
    }, nestedF = {
        if (any(is.na(object$F.p.value))) 
            stop("nestedF method can't handle NA p-values", call. = FALSE)
        sel <- p.adjust(object$F.p.value, method = adjust.method) < 
            p.value
        i <- sum(sel, na.rm = TRUE)
        n <- sum(!is.na(sel))
        a <- switch(adjust.method, none = 1, bonferroni = 1/n, 
            holm = 1/(n - i + 1), BH = i/n, BY = i/n/sum(1/(1:n)))
        results <- new("TestResults", array(0, dim(object$t)))
        dimnames(results) <- dimnames(object$coefficients)
        if (any(sel)) 
            results[sel, ] <- classifyTestsF(object[sel, ], p.value = p.value * 
                a)
    })
    if (lfc > 0) {
        if (is.null(object$coefficients)) 
            warning("lfc ignored because coefficients not found")
        else results@.Data <- results@.Data * (abs(object$coefficients) > 
            lfc)
    }
    results<-subset(results[,],apply(results,1,function(x) any(x!=0)))
    results
}
