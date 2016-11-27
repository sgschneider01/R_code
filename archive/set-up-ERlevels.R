library(GEOquery)
library(limma)
source("~/Desktop/research/geo-analysis/geo-lists.txt")
source("~/Desktop/research/geo-analysis/esr-lists.txt")

source("~/Desktop/research/code/R scripts/analyzeGEO.R")
source("~/Desktop/research/code/R scripts/annoMaker.R")

source("~/Desktop/research/code/R scripts/makeERfile.R")
source("~/Desktop/research/code/R scripts/getERlevels.R")

source("~/Desktop/research/code/R scripts/summarizeChips.R")
source("~/Desktop/research/code/R scripts/makeGSE.R")

# Analyze all the GC/RMA sets, and the MAS5 sets that do not provide raw data
#   (makeGSE() will log2 transform the MAS5 data)

er.table <- NULL
for (i in 1:length(gsm.lists)) { # for each 1st level (cell type) list, eg mcf7
  x <- gsm.lists[[i]]  
  for (j in 1:length(x)) {  # for each 2nd level (gse) sublist, eg gse3529
     g <- x[j]
     g2 <- paste(names(g),".er",sep='')
     if(!exists(g2))  makeERfile(g)
     ge <- eval(parse(text=g2))
#print(ge)
     er.table <- merge(er.table,ge,by=0,all=TRUE)
     row.names(er.table) <- er.table[,1]
     er.table <- er.table[,-1]

  }
}

er.table <- do.call("cbind",lapply(er.list,
                    function(x) {
                       g <-  eval(parse(text=x)) 
                       mymatch<-match(esr.hgu,rownames(g))
                       return(g[mymatch,]) 
                    } ) )


# Analyze the MAS5 files with raw data available

for (i in 1:length(raw.lists)) {
  x <- raw.lists[[i]]
  for (j in 1:length(x)) {
     g <- x[j]
     g2 <- paste(g,".er",sep='')
     if(!exists(g2)) {     }
  }
}


