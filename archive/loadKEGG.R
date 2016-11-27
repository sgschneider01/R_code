loadKEGG <- function () {
   p<-as.list(mouse4302PATH)
   p<-p[which(!is.na(p))]    
   assign("mouse4302.kt",length(p), pos=1)
   assign("mouse4302.keg.tab", data.frame(table(unlist(p))),pos=1)

   p<-as.list(hgu133a2PATH)  #length is 22,277
   p<-p[which(!is.na(p))]     #length is 6,542  (9525 for hgu133plus2)
   assign("hgu133a2.kt",length(p), pos=1)
   assign("hgu133a2.keg.tab", data.frame(table(unlist(p))),pos=1) 
}