library(annotate)       # getGI()
library(Biostrings)     # matchPattern(), pairwiseAlignment(), etc.

source("~/Desktop/research/notes/R_code/getSeq.R")
source("~/Desktop/research/notes/R_code/parseExonTable.R")

getTxLocs <- function (egid,sp=c("Mm","Hs")) {
# this function should take an egid and get the transcript sequence
# and return a matrix of probe locations

  sp <- match.arg(sp)
  if (!is.character(egid)) egid <- as.character(egid)

# (1) Get the affy probe sequences
  affx <- switch(sp, "Mm" = as.data.frame(mouse4302probe) ,
                     "Hs" = as.data.frame(hgu133plus2probe)  )
  ps   <- switch(sp, 
                "Mm" = toTable(subset(mouse4302ENTREZID,Rkeys=egid))[,1],
                "Hs" = toTable(subset(hgu133plus2ENTREZID,Rkeys=egid))[,1] )
  ps <- subset(ps, grepl("^[0-9]+_",ps))  # eliminate control probes
  seqs <- subset(affx[,1],affx$Probe.Set.Name %in% ps)

  accnums <- getAccNums(egid)

# (2) Get the transcript (mRNA) sequence
  rnas     <- getRNASeq(accnums) # returns a vector
  rna.seqs <- sapply(rnas,DNAString)
  
# (3) Get the probe interrogation positions by matching the actual seq

  match.list <- sapply(rna.seqs, function(rna) {
     matches <- lapply(seqs, function(x){gregexpr(x,rna)[[1]][1]})
     matches <- matrix(unlist(matches),nrow=length(ps),byrow=T)
     rownames(matches) <- ps
     matches
  },simplify=F)

  return(match.list)
}
