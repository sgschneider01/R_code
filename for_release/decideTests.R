decideTests <- function (object, method = "sliding", adjust.method = "BH", 
    p.value = 0.05, lfc = 0, tau=sqrt(2)) 
{
    if (!is(object, "MArrayLM")) 
        stop("Need MArrayLM object")
    if (is.null(object$p.value)) 
        object <- eBayes(object)
    method <- match.arg(method, c("separate", "global", "sliding",
        "hierarchical", "nestedF"))
    adjust.method <- match.arg(adjust.method, c("none", "bonferroni", 
        "holm", "BH", "fdr", "BY"))
    if (adjust.method == "fdr") 
        adjust.method <- "BH"
    switch(method, separate = {
        p <- as.matrix(object$p.value)
        for (j in 1:ncol(p)) {
            o <- !is.na(p[, j])
            p[o, j] <- p.adjust(p[o, j], method = adjust.method)
        }
        s <- sign(as.matrix(object$coefficients))
        results <- new("TestResults", s * (p < p.value))
        if (lfc > 0) 
          results@.Data <- results@.Data * (abs(object$coefficients) > lfc)
    }, global = {
        p <- as.matrix(object$p.value)
        o <- !is.na(p)
        p[o] <- p.adjust(p[o], method = adjust.method)
        s <- sign(as.matrix(object$coefficients))
        results <- new("TestResults", s * (p < p.value))
        if (lfc > 0) 
          results@.Data <- results@.Data * (abs(object$coefficients) > lfc)
    }, sliding = {
        p <- as.matrix(object$p.value)
        for (j in 1:ncol(p)) {
            o <- !is.na(p[, j])
            p[o, j] <- p.adjust(p[o, j], method = adjust.method)
        }
        m <- as.matrix(object$coefficients)
        m.a <- abs(m)
        beta <- log(4)/(tau-1)
        alf <- p.value/(1+exp(beta*(tau-m.a)))  
          # if (p.value=.05 & m.a=1) then alf=.01 regardless of tau
          #   tau defines the midpoint, ie where p=.5*p.value
        s <- sign(m)
        results <- new("TestResults", s * (p < alf))
    }, hierarchical = {
        stop("Can't handle hierarchical method yet")
    }, nestedF = {
        stop("Can't handle nestedF method yet")
    })
    results
}
