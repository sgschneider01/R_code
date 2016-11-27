#what do we do when getExons fails to return a table but parseExonTable does?
#eg Phf14 (75725) - neither mRNA nucleotide page has any exon information, but the gene page does

genom2tx <- function (exon.tab) {

  exons <- exon.tab$exons
  for (i in 2:nrow(exons)) {
    prev <- exons[i-1,]
    curr <- exons[i,]
    exons[i,1] <- prev[2] + 1
    exons[i,2] <- curr[2] - curr[1] + prev[2] + 1
  }

  frst <- which(apply(exon.tab$exons,1,in.between,x=exon.tab$cds[1]))
  last <- which(apply(exon.tab$exons,1,in.between,x=exon.tab$cds[2]))

  st <- exon.tab$cds[1] - exon.tab$exons[frst,1] + exons[frst,1] 
  nd <- exon.tab$cds[2] - exon.tab$exons[last,1] + exons[last,1] +1


  exon.tab$exons <- exons
  exon.tab$cds <- c(st,nd)
  
  exon.tab

}

