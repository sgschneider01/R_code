library(annotate)       # getGI()
library(Biostrings)     # matchPattern(), pairwiseAlignment(), etc.
library(XML)            # xmlParse(), etc.
#library(hgu133plus2.db) # auto calls library(org.Hs.eg.db)
#library(mouse4302.db)   # auto calls library(org.Mm.eg.db)
#library(mouse4302probe)
source("~/Desktop/research/notes/R_code/getExons.R")

mapProbes <- function (gene_id=NULL,symbol=NULL,platform) {

  if (is.null(gene_id) & is.null(symbol)) {
     stop("You must specify either a gene id or symbol.")
  }
  if (is.null(platform)) {
     stop("You must specify an Affymetrix platform.")
  }

probe.table <- do.eval(paste(platform,"probe",sep=''))
paste("org",sp,"egREFSEQ",sep='.')

  accnum <- get(gene_id,REFSEQ)[1]
  gi <- suppressWarnings(getGI(accnum))

# (1) Get the affy probe sequences
  if (!is.null(gene_id)){
    ps <- toTable(subset(do.eval(paste(platform,"ENTREZID",sep='')), Rkeys=gene_id))$probe_id
  } else {
    ps <- toTable(subset(do.eval(paste(platform,"SYMBOL",sep='')), Rkeys=gene_id))$probe_id
  }
  probe.seqs <- subset(probe.table$sequence,probe.table$Probe.Set.Name %in% ps)

# (2) Get the exon boundaries from NCBI
  exons <- getExons(gi)

# (3) Get the probe interrogation positions by matching the actual seq
  rna     <- getSEQ(gi)
  rna.seq <- DNAString(rna)
  matches <- lapply(probe.seqs,function(x) {gregexpr(x,rna.seq)[[1]][1]})
  matches <- matrix(unlist(matches),ncol=length(ps))
  
  colnames(matches) <- probe.nms #different order than ps
  matches<- t(matches)
}