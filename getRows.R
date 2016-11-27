# called by getPairwiseMatrix()

get.rows.as.cols <- function (x,fit,ss=NULL) {  #fit from prepFit
  df <- get.rows(x,fit,ss)
  t(df)
}
get.rows <- function (x,fit,ss=NULL) {
  old.rows <- subset(fit,fit$egid==x)
  if (!is.null(ss)) old.rows <- old.rows[ss,]
  y <- which(colnames(old.rows)=="egid")
  z <- which(colnames(old.rows)=="affy.id")
  df <- old.rows[,c(-y,-z)]
  rownames(df) <- old.rows$affy.id
  df
}