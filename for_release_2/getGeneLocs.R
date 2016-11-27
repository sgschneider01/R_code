getGeneLocs <- function (gene_id,platform) {
# this function should take an gene_id and get the genomic sequence
# and return a matrix of probe locations

  if (!is.character(gene_id)) gene_id <- as.character(gene_id)

# (1) Get the affy probe sequences
  probesets <- toTable(subset(do.eval(paste(platform,"ENTREZID",sep='')), Rkeys=gene_id))$probe_id
  probes <- as.data.frame(do.eval(paste(platform,"probe",sep='')))
  seqs <- subset(probes$sequence,probes$Probe.Set.Name %in% probesets)

# (2) Get the full genomic sequence
  dna <- getDNASeq(gene_id)

# (3) Get the probe interrogation positions by matching the DNA seq
  matches <- lapply(seqs, function(x){gregexpr(x,dna)[[1]][1]})
  matches[which(matches>0)] <- unlist(matches[which(matches>0)]) + 13
  matches <- matrix(unlist(matches),nrow=length(probesets),byrow=T)
  rownames(matches) <- probesets

  return(matches)
}