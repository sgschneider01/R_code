#source("~/Desktop/research/notes/R_code/addEGID.R")

addEGID <- function (df,chip) {
# df:  a dataframe to add the annotations to
# chip:  what chip to use 
require(mouse4302.db)
require(mouse430a2.db)
require(hgu133a.db)
require(hgu133a2.db)
require(hgu133plus2.db)

   switch(chip,
     mouse4302 =   p<-as.list(mouse4302ENTREZID),
     mouse430a2 =  p<-as.list(mouse430a2ENTREZID),
     hgu133a =     p<-as.list(hgu133aENTREZID),
     hgu133a2 =    p<-as.list(hgu133a2ENTREZID),
     hgu133plus2 = p<-as.list(hgu133plus2ENTREZID)
   )
   p <- data.frame(affy.id=names(p),egid=as.character(p))

   if("affy.id" %in%  colnames(df)) {
     df <- merge(p, df, all.y=TRUE)
   } else {
     df <- merge(p, df, all.y=TRUE, by.x="affy.id", by.y=0)
   }

   cat("Annotation done.\n")
   return(df)
}

