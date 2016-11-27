loadAnnotations <- function() {
   
   require (hgu133a2.db)  # use this for hgu133plus2 also for now
   require (mouse4302.db)
   
   p<-as.list(hgu133a2SYMBOL)
   assign("hgu133a2.sym",data.frame(affy.id=names(p),symb=as.character(p)),pos=1)
   p<-as.list(hgu133a2ENTREZID)   
   assign("hgu133a2.etg",data.frame(affy.id=names(p),egid=as.character(p)),pos=1)
   p<-as.list(hgu133a2REFSEQ)
   assign("hgu133a2.rfs",data.frame(affy.id=names(p),refs=as.character(p)),pos=1)

   p<-as.list(hgu133a2PATH)  #length is 22,277
   assign("hgu133a2.keg",data.frame(affy.id=names(p),kegg=as.character(p)),pos=1)
   p<-p[which(!is.na(p))]     #length is 6,542  (9525 for hgu133plus2)
   assign("hgu133a2.kt",length(p), pos=1)
   assign("hgu133a2.keg.tab", data.frame(table(unlist(p))),pos=1) 


   p<-as.list(mouse4302SYMBOL)
   assign("mouse4302.sym",data.frame(affy.id=names(p),symb=as.character(p)),pos=1)
   p<-as.list(mouse4302ENTREZID)   
   assign("mouse4302.etg",data.frame(affy.id=names(p),egid=as.character(p)),pos=1)
   p<-as.list(mouse4302REFSEQ)
   assign("mouse4302.rfs",data.frame(affy.id=names(p),refs=as.character(p)),pos=1)
   p<-as.list(mouse4302PATH)
   assign("mouse4302.keg",data.frame(affy.id=names(p),kegg=as.character(p)),pos=1)
   p<-p[which(!is.na(p))]    
   assign("mouse4302.kt",length(p), pos=1)
   assign("mouse4302.keg.tab", data.frame(table(unlist(p))),pos=1)

}