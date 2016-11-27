#source poolGSE

poolGSE <- function (g) {
  # g is a list of lists, each sublist is a list of GSE ids that are poolable 
  #   each pooled set must have its own list of conditions and contrasts
  #   each set in the pool must be available as RAW data 
  n <- names(g)[1]
  g <- g[[1]]


}