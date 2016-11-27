connCompAM <- function (object) {
    NL <- nodes(object)
    marked <- rep(0, length(NL))
    names(marked) <- NL
    done <- FALSE
    rval <- vector("list", 1)
    cnode <- 1
    index <- 1
    nused <- numeric(0)
    while (!done) {
        curracc <- accAM(object, NL[cnode])[[1]]
        rval[[index]] <- curracc
        nused <- c(nused, cnode)
        index <- index + 1
        if (length(curracc) > 0) 
            marked[names(curracc)] <- 1
        marked[cnode] <- 1
        cnode <- match(0, marked)
        if (is.na(cnode)) 
            done <- TRUE
    }
    nmR <- NL[nused]
    nc <- length(rval)
    rL <- vector("list", length = nc)
    for (i in 1:nc) rL[[i]] <- c(nmR[[i]], names(rval[[i]]))
    return(rL)
}

accAM <- function (object, index) {
    nN <- numNodes(object)
    nNames <- nodes(object)
    nIndex <- length(index)
    whN <- match(index, nNames)
    if (any(is.na(whN))) 
        stop("unmatched node provided")
    rval <- vector("list", length = nIndex)
    names(rval) <- nNames[whN]
    for (i in 1:nIndex) {
        marked <- rep(0, nN)
        distv <- rep(0, nN)
        names(distv) <- nNames
        distx <- 1
        names(marked) <- nNames
        nmkd <- 0
        marked[index[i]] <- 1
        done <- FALSE
        while (!done) {
            minds <- nNames[marked == 1]
            for (node in minds) {
                avec <- adjAM(object, node)[[1]]
                avec <- avec[marked[avec] == 0]
                marked[avec] <- 1
                distv[avec] <- distx
            }
            marked[minds] <- 2
            distx <- distx + 1
            newmk <- sum(marked == 1)
            if (newmk == 0) 
                done <- TRUE
        }
        marked[index[i]] <- 0
        rval[[i]] <- distv[marked == 2]
    }
    return(rval)
}

adjAM <- function (object, index) {
    nd <- nodes(object)
    if (is.character(index)) 
        index <- match(index, nd)
    if (is.na(index) || index < 0 || index > length(nd)) 
        stop(paste("selected vertex is not in the graph"))
    edges(object)[index]
}
