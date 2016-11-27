compare.kegg <- function(exp.name,exp.platform){
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
   
   kg.params.1 <- new("KEGGHyperGParams", 
                      geneIds         = old.ids, 
                      universeGeneIds = my.universe, 
                      annotation      = exp.plat,
                      pvalueCutoff    = .05, 
                      testDirection   = "over")
   
   kg.params.2 <- new("KEGGHyperGParams", 
                      geneIds         = new.ids, 
                      universeGeneIds = my.universe, 
                      annotation      = exp.plat,
                      pvalueCutoff    = .05, 
                      testDirection   = "over")
   
   kg.1<-summary(hyperGTest(kg.params.1))
   kg.2<-summary(hyperGTest(kg.params.2))
   
   kg<-merge(kg.1,kg.2,by=c("KEGGID","Term","Size"),all=T)
   kg<-kg[order(kg$Pvalue.y),]
   
}