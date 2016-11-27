getORKCp <- function (gpl, deg, alf) {

# Takes:    gpl = a string indicating the name of the chip used
#           deg = a list of kegg annotations for a set of DEGs
#           alf = the p value cut-off to use (alpha)
# Returns:  a table of KEGG ids, their freq, and p-values for overrep in "deg"
#           (only those with p values < alpha)

source("~/Desktop/research/notes/R_code/loadKEGG.R")

# (1) Get table of freq of kegg cats on the whole chip
# Tables were created by loadAnnotations() <- addAnnot() <- analyzeGEOpool()
   akt <- paste(gpl,".keg.tab",sep='')
   if (!exists(akt)) loadKEGG()
   all.tab <- do.eval(akt)
   ak <- do.eval(paste(gpl,".kt",sep=''))   
   # ak is the number of probe sets on the chip that have a KEGG annotation:
   #   for hgu133a2 ak=6542, for hgu133plus2 ak=9525


# (2) Get a count of the number of genes with pathways in the DEG list
   deg.k <- deg[which(deg!="NA")]
   dk <- length(deg.k)   

# (3) Make a table of the observed frequencies of each pathway in the DEG list
   #  first must convert all the c("") entries to an actual list
   deg.k <- as.character(deg.k)
   ll <- grep("c",deg.k)
   deg.k[ll] <- list(do.eval(deg.k[ll]))

   deg.tab <- data.frame(table(unlist(deg.k)))
#  deg.tab[,1] is the kegg id, keg.tab[,2] is the frequency (from table())


# (4) For each pathway (kegg id), calculate chi-sq in the given DEG list

#a = number of "successes" in sample      (O = observed freq)
#b = size of sample                       (dk)
#c = number of "successes" in population  (E = expected freq)
#d = size of population                   (ak)

   deg.chi <- NULL
   for ( i in 1:nrow(deg.tab) ) {
     O <- deg.tab[i,2]                  # obs freq of this pathway in this list
     a <- which(as.character(all.tab[,1])==as.character(deg.tab[i,1]))
     E <- all.tab[a,2]                  # freq of the pathway on the whole chip
     m <- matrix(c(O,E,dk-O,ak-E),nrow=2)  
     p <- fisher.test(m,alternative="greater")$p.value
     deg.chi <- c(deg.chi, p)
   }
   deg.tab <- cbind(deg.tab, p=deg.chi)

# (5) Return just the kegg ids which are over-represented (ie are less than the p-value cut-off "alpha")
#   keg.or <- deg.tab[which(deg.tab$p < alf),1]
#   return(keg.or)
}
