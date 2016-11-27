library(hgu133a.db)
hgu133a.sym<-ann.o.maker("hgu133aSYMBOL")
hgu133a.eg<-ann.o.maker("hgu133aENTREZID")
hgu133a.keg<-ann.o.maker("hgu133aPATH")

library(hgu133a2.db)
hgu133a2.sym<-ann.o.maker("hgu133a2SYMBOL")
hgu133a2.eg<-ann.o.maker("hgu133a2ENTREZID")
hgu133a2.keg<-ann.o.maker("hgu133a2PATH")

library(hgu133plus2.db)
hgu133pl2.sym<-ann.o.maker("hgu133plus2SYMBOL")
hgu133pl2.eg<-ann.o.maker("hgu133plus2ENTREZID")
hgu133pl2.keg<-ann.o.maker("hgu133plus2PATH")
hgu133pl2.rfs<-ann.o.maker("hgu133plus2REFSEQ")

library(mouse4302.db)
mouse4302.sym<-ann.o.maker("mouse4302SYMBOL")
mouse4302.eg<-ann.o.maker("mouse4302ENTREZID")
mouse4302.keg<-ann.o.maker("mouse4302PATH")



hgu133a.gse <- c("gse3bsk.dt","gse2251.dt", "gse3529.dt", "gse3834_4m.dt", "gse3834_24m.dt", "gse4006.dt", "gse4025.dt", "gse9936_4.dt", "gse9936_24.dt")
hgu133a2.gse <- c("gse9253.dt")
hgu133plus2.gse <- c("gse3834_4b.dt",  "gse3834_4t.dt", "gse3834_24b.dt", "gse3834_24t.dt")


for (i in length(gpl.names)) {
   #repeat for each chip type
   pkg<-gpl.names[[i]]                                       #eg hgu133a
   # these data.frames were created by ann.o.maker:
   chip.sym<-eval(parse(text=(paste(pkg,".sym",sep=''))))
   chip.eg<-eval(parse(text=(paste(pkg,".eg",sep=''))))
   chip.keg<-eval(parse(text=(paste(pkg,".keg",sep=''))))

   chip.list<-eval(parse(text=(paste(pkg,".gse",sep=''))))   #eg hgu133a.gse
   #the list of gses that use this chip

   for (j in 1:length(chip.list)) {
      n<-chip.list[j]                                        #eg gse2251.dt
      x <- eval(parse(text=n))                 
      x <- merge(x, chip.sym, all.x=TRUE)
      x <- merge(x, chip.eg, all.x=TRUE)
      x <- merge(x, chip.keg, all.x=TRUE)
      y <- paste(n,".ann",sep='')
      assign(y,x)
   cat("Success for ",y,'\n')
  }
}