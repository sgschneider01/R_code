fisher.p <- function(p.list) {
  chi.sq <- (-2)*sum(log(p.list))
  comb.p <- 1-pchisq(chi.sq,2*length(p.list))
  return(comb.p)
}