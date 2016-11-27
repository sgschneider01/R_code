kendall.w <- function (x, lambda = NULL, descending = TRUE, ranks = FALSE) {
    if (missing(x)) 
        stop("Usage: kendall.w(x,lambda=NULL,descending=TRUE,ranks=FALSE)")
    if (!is.data.frame(x) && !is.matrix(x)) 
        stop("x must be a dataframe or matrix")
    Wcrit01 <- matrix(c(NA, NA, NA, NA, NA, 0.522, 0.469, 0.425, 
        0.392, 0.359, 0.335, 0.311, 0.291, 0.274, 0.26, 0.245, 
        0.233, 0.221, NA, 0.768, 0.644, 0.553, 0.491, 0.429, 
        0.39, 0.351, 0.328, 0.306, 0.284, 0.262, 0.24, 0.227, 
        0.216, 0.204, 0.193, 0.182, 0.84, 0.683, 0.571, 0.489, 
        0.434, 0.379, 0.344, 0.309, 0.289, 0.269, 0.25, 0.23, 
        0.211, 0.2, 0.19, 0.18, 0.17, 0.16, 0.78, 0.629, 0.524, 
        0.448, 0.397, 0.347, 0.314, 0.282, 0.264, 0.246, 0.228, 
        0.21, 0.193, 0.183, 0.173, 0.164, 0.155, 0.146, 0.737, 
        0.592, 0.491, 0.419, 0.371, 0.324, 0.293, 0.263, 0.246, 
        0.229, 0.212, 0.195, 0.179, 0.169, 0.16, 0.152, 0.144, 
        0.136), nrow = 5, byrow = TRUE)
    Wcrit05 <- matrix(c(NA, NA, NA, NA, NA, 0.376, 0.333, 0.3, 
        0.275, 0.25, 0.232, 0.214, 0.2, 0.187, 0.176, 0.166, 
        0.158, 0.15, NA, 0.619, 0.501, 0.421, 0.369, 0.318, 0.287, 
        0.256, 0.232, 0.222, 0.204, 0.188, 0.171, 0.16, 0.151, 
        0.143, 0.136, 0.129, 0.716, 0.552, 0.449, 0.378, 0.333, 
        0.287, 0.259, 0.231, 0.212, 0.195, 0.18, 0.167, 0.155, 
        0.147, 0.139, 0.131, 0.124, 0.117, 0.66, 0.512, 0.417, 
        0.351, 0.305, 0.267, 0.24, 0.215, 0.201, 0.186, 0.172, 
        0.158, 0.145, 0.137, 0.13, 0.123, 0.116, 0.109, 0.624, 
        0.484, 0.395, 0.333, 0.29, 0.253, 0.228, 0.204, 0.19, 
        0.176, 0.163, 0.15, 0.137, 0.13, 0.123, 0.116, 0.109, 
        0.103), nrow = 5, byrow = TRUE)
    datadim <- dim(x)
    if (min(datadim) < 3) 
        stop("x must be at least 3x3")
    if (is.null(colnames(x))) { cnames <- as.character(1:datadim[2])
    } else                    { cnames <- colnames(x) }
    if (!is.null(lambda)) {
        max.lambda.len <- max(nchar(unlist(lambda)))
    } else { 
        max.lambda.len <- 4
    }
    col.width <- max(nchar(cnames))
    if (col.width <= max.lambda.len) 
        col.width <- max.lambda.len + 1
    cnames <- formatC(cnames, width = col.width)
    if (ranks) {
        xsums <- rowSums(x)
        if (!all(xsums == xsums[1])) 
            stop("Differing row sums - will not compute properly.")
        rank.mat <- x
    } else {
        meanscore <- sapply(x, mean)
        rank.mat <- t(as.matrix(x))
        if (descending) 
            rank.mat <- max(rank.mat) - rank.mat
        exist.tie <- 0
        for (i in 1:datadim[1]) rank.mat[, i] <- rank(rank.mat[, 
            i])
        rank.mat <- t(rank.mat)
    }
    exist.tie <- length(unlist(apply(rank.mat, 1, unique))) < 
        length(rank.mat)
    meanranks <- apply(rank.mat, 2, mean)
    grandmean <- mean(meanranks)
    if (exist.tie) {
        Tj <- tiecorr(rank.mat)
        W <- sum((meanranks - grandmean)^2)/((datadim[2] * (datadim[2]^2 - 
            1) - Tj/datadim[1])/12)
    }
    else W <- sum((meanranks - grandmean)^2)/(datadim[2] * (datadim[2]^2 - 
        1)/12)
    if (datadim[2] > 7) {
        p.table <- NA
        x2df <- datadim[2] - 1
        p.chisq <- pchisq(datadim[1] * (datadim[2] - 1) * W, 
            x2df, lower.tail = FALSE)
    }
    else {
        p.table <- ifelse(W > Wcrit01[datadim[2] - 2, datadim[1] - 
            2], "<0.01", ifelse(W > Wcrit05[datadim[2] - 2, datadim[1] - 
            2], "<0.05", ">0.05"))
        x2df <- NA
        p.chisq <- NA
    }
    if (!is.null(lambda)) {
        ldim <- dim(lambda)
        if (is.null(ldim)) 
            zstat <- round(BEZ(rank.mat, lambda), 3)
        else {
            zstat <- vector("numeric", ldim[1])
            for (i in 1:ldim[1]) {
                zstat[i] <- round(BEZ(rank.mat, lambda[i, ]), 
                  3)
            }
        }
    }
    else zstat <- NULL
    k.w <- list(W = W, p.table = p.table, p.chisq = p.chisq, 
        x2df = x2df, rank.mat = rank.mat, cnames = cnames, meanranks = meanranks, 
        lambda = lambda, zstat = zstat)
    class(k.w) <- "kendall.w"
    return(k.w)
}
