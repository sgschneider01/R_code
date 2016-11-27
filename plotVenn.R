#source("~/Desktop/research/notes/R_code/plotVenn.R")
plotVenn <- function(object) {
# object should be a list of length 3, each item in the list should be a vector
#   of items from the same universe.  plotVenn will plot a venn diagram of the 
#   overlaps of the 3 vectors.
  require(limma)

  stopifnot( length(object) == 3)

  # Form universe as union of all three sets
  universe <- unique( c(object[[1]], object[[2]], object[[3]]) )

  counts <- matrix(0, nrow=length(universe), ncol=3)

  colnames(counts) <- names(object)

# universe %in% object[[n]]: creates a list of T/F values whether each item in the universe is in the vector object[[n]]

  counts[,1] <- universe %in% object[[1]]
  counts[,2] <- universe %in% object[[2]]
  counts[,3] <- universe %in% object[[3]]

  myVennDiagram( vennCounts(counts) )

}

myVennDiagram <- function (object, include = "both", names, mar = rep(1, 4), 
   cex = 1.5, lwd = 1, circle.col, counts.col, show.include, ...) {

#same as vennDiagram() except does not report the number of items not in the union of all sets (the zero in the corner) and puts the number of items in each set in the labels

    if (!is(object, "VennCounts")) {
        if (length(include) > 2) 
            stop("Cannot plot Venn diagram for more than 2 sets of counts")
        if (length(include) == 2) 
            object.2 <- vennCounts(object, include = include[2])
        object <- vennCounts(object, include = include[1])
    } else if (length(include == 2)) include <- include[1]
    nsets <- ncol(object) - 1
    if (nsets > 3) 
        stop("Can't plot Venn diagram for more than 3 sets")
    if (missing(names)) 
        names <- colnames(object)[1:nsets]
    counts <- object[, "Counts"]
    if (length(include) == 2) 
        counts.2 <- object.2[, "Counts"]
    if (missing(circle.col)) 
        circle.col <- par("col")
    if (length(circle.col) < nsets) 
        circle.col <- rep(circle.col, length.out = nsets)
    if (missing(counts.col)) 
        counts.col <- par("col")
    if (length(counts.col) < length(include)) 
        counts.col <- rep(counts.col, length.out = length(include))
    if (missing(show.include)) 
        show.include <- as.logical(length(include) - 1)
    theta <- 2 * pi * (1:360)/360
    xcentres <- list(0, c(-1, 1), c(-1, 1, 0))[[nsets]]
    ycentres <- list(0, c(0, 0), c(1/sqrt(3), 1/sqrt(3), -2/sqrt(3)))[[nsets]]
    r <- c(1.5, 1.5, 1.5)[nsets]
    xtext <- list(-1.2, c(-1.2, 1.2), c(-1.2, 1.2, 0))[[nsets]]
    ytext <- list(1.8, c(1.8, 1.8), c(2.4, 2.4, -3))[[nsets]]
    old.par <- par(mar = mar)
    on.exit(par(old.par))
    plot(x = 0, y = 0, type = "n", xlim = c(-4, 4), ylim = c(-4, 
        4), xlab = "", ylab = "", axes = FALSE, ...)
    for (i in 1:nsets) {
        lines(xcentres[i] + r * cos(theta), ycentres[i] + 
            r * sin(theta), lwd = lwd, col = circle.col[i])
        label <- paste(names[i], sum(object[which(object[,i]>0),4]), sep='=')
        text(xtext[i], ytext[i], label, cex = cex)
    }

    switch(nsets, {
        rect(-3, -2.5, 3, 2.5)
        printing <- function(counts, cex, adj, col, leg) {
            text(2.3, -2.1, counts[1], cex = cex, col = col, 
                adj = adj)
            text(0, 0, counts[2], cex = cex, col = col, adj = adj)
            if (show.include) 
                text(-2.3, -2.1, leg, cex = cex, col = col, adj = adj)
        }
    }, {
        rect(-3, -2.5, 3, 2.5)
        printing <- function(counts, cex, adj, col, leg) {
            text(2.3, -2.1, counts[1], cex = cex, col = col, 
                adj = adj)
            text(1.5, 0.1, counts[2], cex = cex, col = col, adj = adj)
            text(-1.5, 0.1, counts[3], cex = cex, col = col, 
                adj = adj)
            text(0, 0.1, counts[4], cex = cex, col = col, adj = adj)
            if (show.include) 
                text(-2.3, -2.1, leg, cex = cex, col = col, adj = adj)
        }
    }, {
        rect(-3, -3.5, 3, 3.3)
        printing <- function(counts, cex, adj, col, leg) {
#            text(2.5, -3, counts[1], cex = cex, col = col, adj = adj)
            text(0, -1.7, counts[2], cex = cex, col = col, adj = adj)
            text(1.5, 1, counts[3], cex = cex, col = col, adj = adj)
            text(0.75, -0.35, counts[4], cex = cex, col = col, 
                adj = adj)
            text(-1.5, 1, counts[5], cex = cex, col = col, adj = adj)
            text(-0.75, -0.35, counts[6], cex = cex, col = col, 
                adj = adj)
            text(0, 0.9, counts[7], cex = cex, col = col, adj = adj)
            text(0, 0, counts[8], cex = cex, col = col, adj = adj)
            if (show.include) 
                text(-2.5, -3, leg, cex = cex, col = col, adj = adj)
        }
    })
    adj <- c(0.5, 0.5)
    if (length(include) == 2) 
        adj <- c(0.5, 0)
    printing(counts, cex, adj, counts.col[1], include[1])
    if (length(include) == 2) 
        printing(counts.2, cex, c(0.5, 1), counts.col[2], include[2])
    invisible()
}
