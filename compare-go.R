compare.go <- function(exp.name,exp.plat){
   exp.egid <- paste(exp.platform,"ENTREZID",sep="")
   exp.plat <- paste(exp.platform,"db",sep=".")

   dt.old  <- do.eval(paste(exp.name,"dt.old",sep='.'))
   if (has.dim(dt.old))  old.ids <- rownames(dt.old)
    else                 old.ids <- names(dt.old)
   old.ids <- mget(old.ids, do.eval(exp.egid) )
   old.ids <- unique(unlist(old.ids))
   old.ids <- subset(old.ids,!is.na(old.ids))
   
   dt.new  <- do.eval(paste(exp.name,"dt.new",sep='.'))
   new.ids <- neweg2old(rownames(dt.new))
   new.ids <- unique(new.ids)
   
   fit3 <- do.eval(paste(exp.name,"fit3$coefficients",sep='.'))
   my.universe <- mget(rownames(fit3),do.eval(exp.egid))
   my.universe <- unique(unlist(my.universe))
   my.universe <- subset(my.universe,!is.na(my.universe))
   
   go.params.1 <- new("GOHyperGParams", 
                   geneIds         = unique(old.ids), 
                   universeGeneIds = my.universe, 
                   annotation      = exp.plat,
                   ontology        = "BP", 
                   pvalueCutoff    = .25, 
                   conditional     = F, 
                   testDirection   = "over")
   go.params.2 <- new("GOHyperGParams", 
                   geneIds         = unique(new.ids), 
                   universeGeneIds = my.universe, 
                   annotation      = exp.plat,
                   ontology        = "BP", 
                   pvalueCutoff    = .25, 
                   conditional     = F, 
                   testDirection   = "over")

   go.1<-summary(hyperGTest(go.params.1))
   go.2<-summary(hyperGTest(go.params.2))
   go<-merge(go.1,go.2,by=c("GOBPID","Term","Size"),all=T)
   go<-go[order(go$Pvalue.y),]

}
