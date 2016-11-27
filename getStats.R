library(GOstats)

# see notes-2011-03-16 for usage

getKEGG <- function(univ,ids,plat) {
   kegg.params <- new("KEGGHyperGParams", 
                  geneIds         = ids, 
                  universeGeneIds = univ, 
                  annotation      = plat,
                  pvalueCutoff    = .05, 
                  testDirection   = "over")
   return(summary(hyperGTest(kegg.params)))
}
getGO <- function(univ,ids,plat) {
    go.params <- new("GOHyperGParams", 
                 geneIds         = ids, 
                 universeGeneIds = univ, 
                 annotation      = plat,
                 ontology        = "BP", 
                 pvalueCutoff    = .25, 
                 conditional     = F, 
                 testDirection   = "over")
   return(summary(hyperGTest(go.params)))
}