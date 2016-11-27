#source("~/Desktop/research/notes/R_code/addAnnotation.R")

addAnnotation <- function (df,chip,field) {
# df:  a dataframe to add the annotations to
# chip:  what chip to use 

require(paste(chip,"db",sep='.'),character.only=T)

switch(field,
   symbol = {
      p <- as.list(do.eval(paste(chip,"SYMBOL",sep="")))
      p <- data.frame(affy.id=names(p),sym=as.character(p))
   },
   name = {
      p <- as.list(do.eval(paste(chip,"GENENAME",sep="")))
      p <- data.frame(affy.id=names(p),name=as.character(p))
   },
   egid = {
      p <- as.list(do.eval(paste(chip,"ENTREZID",sep="")))
      p <- data.frame(affy.id=names(p),egid=as.character(p))
   },
   {stop("you must specify a valid field")})


   if("affy.id" %in%  colnames(df)) {
     df <- merge(p, df, all.y=TRUE)
   } else {
     df <- merge(p, df, all.y=TRUE, by.x="affy.id", by.y=0)
   }

   return(df)
}



#source("~/Desktop/research/notes/R_code/addAnnotation.R")

addAnnotation <- function (df,chip,field) {
# df:  a dataframe to add the annotations to
# chip:  what chip to use 
library(mouse4302.db)
library(mouse430a2.db)
library(hgu133a.db)
library(hgu133a2.db)
library(hgu133plus2.db)

switch(field,
   symbol = {
     switch(chip,
       mouse4302 =   p<-as.list(mouse4302SYMBOL),
       mouse430a2 =  p<-as.list(mouse430a2SYMBOL),
       hgu133a =     p<-as.list(hgu133aSYMBOL),
       hgu133a2 =    p<-as.list(hgu133a2SYMBOL),
       hgu133plus2 = p<-as.list(hgu133plus2SYMBOL) )
     p <- data.frame(affy.id=names(p),sym=as.character(p))
   },
   name = {
     switch(chip,
       mouse4302 =   p<-as.list(mouse4302GENENAME),
       mouse430a2 =  p<-as.list(mouse430a2GENENAME),
       hgu133a =     p<-as.list(hgu133aGENENAME),
       hgu133a2 =    p<-as.list(hgu133a2GENENAME),
       hgu133plus2 = p<-as.list(hgu133plus2GENENAME))
     p <- data.frame(affy.id=names(p),name=as.character(p))
   },
   egid = {
     switch(chip,
       mouse4302 =   p<-as.list(mouse4302ENTREZID),
       mouse430a2 =  p<-as.list(mouse430a2ENTREZID),
       hgu133a =     p<-as.list(hgu133aENTREZID),
       hgu133a2 =    p<-as.list(hgu133a2ENTREZID),
       hgu133plus2 = p<-as.list(hgu133plus2ENTREZID) )
     p <- data.frame(affy.id=names(p),egid=as.character(p))
   },
   {stop("you must specify a valid field")})


   if("affy.id" %in%  colnames(df)) {
     df <- merge(p, df, all.y=TRUE)
   } else {
     df <- merge(p, df, all.y=TRUE, by.x="affy.id", by.y=0)
   }

   return(df)
}