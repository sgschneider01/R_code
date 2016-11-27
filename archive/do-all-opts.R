#source("~/Desktop/research/notes/R_code/do-all-opts.R")
source("~/Desktop/research/notes/R_code/analyzeGEO.R")
source("~/Desktop/research/notes/R_code/analyzeGEOpool.R")
source("~/Desktop/research/notes/R_code/getKEGGchisq.R")
source("~/Desktop/research/notes/R_code/handyFunctions.R")
load("/Users/arwen/Desktop/research/notes/R_workspaces/wkspc-bcpool.R")

pool.list <- list( c("M25", "bcpool", "hgu133a2", "blue"),
                   c("T6", "bcpool", "hgu133a2", "green"),
                   c("B6", "bcpool", "hgu133a2", "purple"),
                   c("D4", "bcpool", "hgu133a2", "teal"),
                   c("RO", "ROonly", "mouse4302", "pink"),
                   c("KK", "KKonly", "mouse4302", "red")
                 )

options.list <- list( list(2, 1, "BH"),
                      list(2, 5, "BH"),
                      list(2, 1, "none"),
                      list(2, 5, "none"),
                      list(1.5, 1, "BH"),
                      list(1.5, 5, "BH"),
                      list(1.5, 1, "none"),
                      list(1.5, 5, "none") )
 


for (pl in pool.list[5]) {
#  for (l in c(TRUE,FALSE)) {
#    fit3.RO.F <- analyzeGEO(ctr.list=pl[1], gse=pl[2], gf=FALSE)
    x <- fit3.RO.F
    for (i in options.list) {
      y <- analyzeGEOpool(x, aff.chip=pl[3], mval=log2(i[[1]]), pval=(i[[2]]/100),  adjm=i[[3]])
      gn2 <- paste(pl[1],"FALSE",i[[1]],i[[2]],i[[3]],sep='.')
      assign(gn2,y)
    }
    x <- fit3.RO.T
    for (i in options.list) {
      y <- analyzeGEOpool(x, aff.chip=pl[3], mval=log2(i[[1]]), pval=(i[[2]]/100),  adjm=i[[3]])
      gn2 <- paste(pl[1],"TRUE",i[[1]],i[[2]],i[[3]],sep='.')
      assign(gn2,y)
    }
#  }
}


