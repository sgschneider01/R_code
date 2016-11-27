getProbeSetLocs <- function (gene_id=NULL,symbol=NULL,platform) {
  if (is.null(gene_id) & is.null(symbol)) {
     stop("You must specify either a gene id or symbol.")
  }
  if (is.null(platform)) {
     stop("You must specify an Affymetrix platform.")
  }

  org.pkg <- do.eval(paste(platform,"ORGPKG",sep=''))
  org.SYMBOL <- do.eval(paste(org.pkg,"SYMBOL",sep=''))
  if (is.null(gene_id)) {
    gene_id <- mappedLkeys(subset(org.SYMBOL,Rkeys=symbol))
  }
  if (is.null(symbol)) {
    symbol <- get(gene_id,org.SYMBOL)
  }

  if (is.numeric(gene_id)) gene_id <- as.character(gene_id)

  exon.tab <- getExonTable(gene_id)
  if (is.null(exon.tab)) stop("No exon information available")
  gl <- getGeneLocs(gene_id,platform)  

  ex.int.tabs <- lapply(exon.tab,function(p) {
    exs <- p$exons
    ints <- NULL
    for (j in 2:nrow(exs)) { 
       intron <- c(exs[j-1,2]+1, exs[j,1]-1)
       ints <- rbind(ints,intron)
    }
    ei.tab <- rbind(exs,ints)
    rownames(ei.tab) <- c(paste("exon",1:nrow(exs),sep="."), 
                          paste("intron",1:nrow(ints),sep="."))
    ei.tab
  })
  names(ex.int.tabs) <- gsub(" .*","",names(exon.tab))

  exon.matches <- lapply(ex.int.tabs,function(x) {
     apply(x,1,function(st.nd) {rowSums(between(gl,st.nd))})
  })

  lapply(exon.matches,function(exm){  apply(exm,1,function(ps)colnames(exm)[which(ps>0)])})
}

