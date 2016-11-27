library(annotate)       # getGI()
library(Biostrings)     # matchPattern(), pairwiseAlignment(), etc.

getCustomLocs <- function(egid,sp) {

# (1) Get the CUSTOM CDF probe sequences
  cust.id<-paste(egid,"_at",sep='')
  switch(sp,
    "Mm" =   { eg <- as.list(mouse4302ENTREZID)
               brain.probe <- as.data.frame(mouse4302mmentrezgprobe) },
    "Hs" =   { eg <- as.list(hgu133plus2ENTREZID)
               brain.probe <- as.data.frame(hgu133plus2hsentrezgprobe) }
  )

  seqs<-subset(brain.probe$sequence,brain.probe$Probe.Set.Name==cust.id)

# (2) Get the probe interrogation positions by matching the actual seq
  if (!is.character(egid)) egid <- as.character(egid)

  rna     <- getRNASeq(egid,sp)
  rna.seq <- DNAString(rna)
  matches <- lapply(seqs, function(x){gregexpr(x,rna.seq)[[1]][1]})
  unlist(matches)
}
