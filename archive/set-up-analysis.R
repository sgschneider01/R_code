library(GEOquery)
library(limma)
source("~/Desktop/research/code/R scripts/makeGSE.R")
source("~/Desktop/research/code/R scripts/analyzeGEO.R")

gpl.names<- list (
   gpl96="hgu133a",
   gpl570="hgu133plus2",
   gpl571="hgu133a2",
   gpl91="hgu95a",
   gpl1261="mouse4302",
   gpl81="mgu74a"
)

gse.96 <- list (
   gse2251=gpl.names$gpl96,
   gse3529=gpl.names$gpl96,
   gse3834_4m=gpl.names$gpl96,
   gse3834_24m=gpl.names$gpl96,
   gse4006=gpl.names$gpl96,
   gse4025=gpl.names$gpl96,
   gse9936_4=gpl.names$gpl96,
   gse9936_24=gpl.names$gpl96,
   gse3bsk=gpl.names$gpl96
)
gse.570 <- list (
   gse3834_4b=gpl.names$gpl570,
   gse3834_4t=gpl.names$gpl570,
   gse3834_24b=gpl.names$gpl570,
   gse3834_24t=gpl.names$gpl570,
   gse9253=gpl.names$gpl571,
)

gse.contrs <- list (
   gse3bsk=c("Ad_E2-Ad_veh","ERb_E2-ERb_veh","ERb_E2-Ad_E2"),
   gse2251=c("ERa_E2-ERa_veh","Ad_E2-Ad_veh","ERa_E2-Ad_E2","ERa_veh-Ad_veh"),
   gse3529=c("B_E2-B_veh","T_E2-T_veh","M_E2-M_veh","B_E2-T_E2", "B_E2-M_E2","M_E2-T_E2"),
   gse3834_4m=c("M_e4-M_e0"),
   gse3834_4t=c("T_e4-T_e0"),
   gse3834_4b=c("B_e4-B_e0"),
   gse3834_24b=c("B_e24-B_e0"),
   gse3834_24t=c("T_e24-T_e0"),
   gse3834_24m=c("M_e24-M_e0"),
   gse4006=c("Ad_E2-Ad_veh","ERb_E2-ERb_veh","ERb_E2-Ad_E2"),
   gse4025=c("Ad_E2-Ad_veh","ERb_E2-ERb_veh","ERb_E2-Ad_E2"),
   gse9936_4=c("Ad_E2-Ad_veh","ERb_E2-ERb_veh","ERb_E2-Ad_E2"),
   gse9936_24=c("Ad_E2-Ad_veh","ERb_E2-ERb_veh","ERb_E2-Ad_E2"),
   gse9253=c("E2-PL")
)

for (i in 1:length(names(gse.contrs))) {
   gn <- names(gse.contrs)[i]   #eg "gse2251"
   fn <- paste("~/Desktop/research/geo-analysis/",gn,".txt",sep='')
   gsm2cond <- read.table(file=fn) 
   assign(paste(gn,".g2c",sep=''),gsm2cond)

   gsmlist <- rownames(gsm2cond)
   for (i in 1:length(gsmlist)) { assign(gsmlist[i],getGEO(gsmlist[i])) }
   gse <- makeGSE(gsmlist)
   assign(gn,gse)   #store the makeGSE object under the name gn (eg 2251)

   x <- analyzeGEO(gn,gse.contrs[i])

   if (ncol(x)>2) {
      x <- cbind(x,diff.num=apply(apply(x[,-1],2,abs),1,sum))
   } else {
      x <- cbind(x,diff.num=abs(x[,2]))
   }
   #adds a column of the # of conditions in which each probeset was diff exp
   #  (ignore the first column of x, which is affy.ids)

   x <- x[which(x$diff.num>0),]
   #keep only the rows which have at least one sig value

   y <- paste(n,".dt",sep='')
   assign(y,x)

   cat("Success for ",n,'\n')
}

