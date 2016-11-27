filterGenes <- function (eset, g2c, p=NULL, A=NULL, s=0.1) {
   require(genefilter) # for pOverA

   ee <- exprs(eset)
   rng <- range(ee)

   # A is 10% of range above minimum
   A <- diff(rng)/10 + rng[1]

   # p = 75% of smallest number of chips in a condition
   p <- (round(.75*min(table(g2c))))/nrow(g2c)

   f1 <- pOverA(p, A)  
   f2 <- sdOverS(s)
   ff <- filterfun(f1,f2)
   gf <- genefilter(ee,ff)
   exprs(eset) <- ee[gf,]

   eset

}