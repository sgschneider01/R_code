source("~/Desktop/research/notes/R_code/getExons.R")
source("~/Desktop/research/notes/R_code/getExonsIntrons.R")
source("~/Desktop/research/notes/R_code/parseExonTable.R")

plotExons <- function (egid,sp) {
  exons <- getExons(egid,sp)
  if (!is.null(exons)) {
    sap <- apply(exons[seq(1,nrow(exons),by=2),], 1, 
                 function (x) {
                   lines(c(x[1],x[2]),c(.01,.01),type="l",lwd=5) } )
    sap <- apply(exons[seq(2,nrow(exons),by=2),], 1, 
                 function (x)  {
                   lines(c(x[1],x[2]),c(-0.01,-.01),type="l",lwd=5) } )
  } else {
    cat("No exon or sts information available.  Bummer.\n")
  }
}

plot.Exons <- function (exons) {
  if (!is.null(exons)) {
    sap <- apply(exons[seq(1,nrow(exons),by=2),], 1, 
                 function (x) {
                   lines(c(x[1],x[2]),c(.01,.01),type="l",lwd=5) } )
    sap <- apply(exons[seq(2,nrow(exons),by=2),], 1, 
                 function (x)  {
                   lines(c(x[1],x[2]),c(-0.01,-.01),type="l",lwd=5) } )
  } else {
    cat("No exon or sts information available.  Bummer.\n")
  }
}

plotExonsIntrons <- function (egid,sp) {
  exons <- getExonTable(egid,sp)
  if (!is.null(exons)) {
    for (e in 1:nrow(exons)) {
       lines(exons[e,],c(0,0),type="l",lwd=5) 
    }
  } else {
    cat("No exon or sts information available.  Bummer.\n")
  }
}


