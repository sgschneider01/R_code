getW <- function (old.rows) {
   mat <- getPairwiseMatrix(old.rows,method="spearman")
   mean(mat,na.rm=T)
}

getW2 <- function (id,fit) {
   mat <- getPairwiseMatrix(id,fit,method="spearman")
   mean(mat,na.rm=T)
}

getWs <- function (fit) {
   u <- unique(fit$egid)
   ws <- sapply(u, function(x) {
        old.rows <- fit[which(fit$egid==x),]
        mat <- getPairwiseMatrix(old.rows,method="spearman")
        mean(mat,na.rm=T)
      })
   ws
}




source("~/Desktop/research/notes/R_code/prepFit.R")
source("~/Desktop/research/notes/R_code/getPairwiseMatrix.R")
# source("~/Desktop/research/notes/R_code/getRows.R") # in getPairwiseMatrix

get.w <- function (id,exp.name,exp.platform,ss=NULL) {
   fit <- prepFit(exp.name,exp.platform)
   mat <- getPairwiseMatrix(id,fit,method="spearman",ss=ss)
   mean(mat,na.rm=T)
}
