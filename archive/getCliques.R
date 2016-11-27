#source("getCliques.R")

getCliques <- function (mat,r.crit=.8) {
  require(graph)
  require(RBGL)
  adjmat <- mat>r.crit
  amg <- new("graphAM",adjmat)
  mc <- allCliques(amg)
  mc
}
#can we make this return only cliques of length >1?

allCliques <- function (g) {
    if (class(g)!="graphAM")
        stop("g must be an object of class graphAM")
    if (isDirected(g)) 
        stop("only appropriate for undirected graphs")
    nv <- length(nodes(g))
    em <- getEdgeMatrix(g)
    ne <- ncol(em)
    ans <- .Call("maxClique", as.integer(nv), as.integer(ne), 
        as.integer(em - 1))
    ans_names <- lapply(ans, function(x) { nodes(g)[x] } )
    ans_names
}


getEdgeMatrix <- function (object) {
  to <- apply(object@adjMat, 1, function(x) which(x != 0))
  if (is.list(to)) {
    from <- rep(1:numNodes(object), listLen(to))
    to <- unlist(to, use.names = FALSE)
  } else if (has.dim(to)) {
    from <- rep(1:numNodes(object), nrow(to))
    to <- unlist(list(t(to)))
  } else {
    from <- 1:nrow(object@adjMat)
  }
  ans <- rbind(from = from, to = to)
  swap <- from > to
  ans[1, swap] <- to[swap]
  ans[2, swap] <- from[swap]
  ans <- ans[,!duplicated(t(ans))]
  ans
}


drawCliques <- function (egid,fit,r.crit=.5) {
  require(graph)
  rows <- get.rows.as.cols(egid,fit)
  mat <- get.pairwise.matrix(rows,method="spearman")
  colnames(mat) <- colnames(rows)
  rownames(mat) <- colnames(rows)
  adjmat <- mat>r.crit
  for (i in 1:nrow(adjmat)) adjmat[i,i]<-FALSE  # for drawing purposes

  amg <- new("graphAM",adjmat)
  l1<-layoutGraph(amg)
  renderGraph(l1)
}
