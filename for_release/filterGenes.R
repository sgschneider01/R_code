filterGenes <- function (eset, s=0.1) {
   ee <- exprs(eset)
   f1 <- sdOverS(s)
   ff <- filterfun(f1)
   gf <- genefilter(ee,ff)
   exprs(eset) <- ee[gf,]
   eset
}