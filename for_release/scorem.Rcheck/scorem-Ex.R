pkgname <- "scorem"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('scorem')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("consolidateGroups")
### * consolidateGroups

flush(stderr()); flush(stdout())

### Name: consolidateGroups
### Title: Consolidation of Concordant Probe Set Groups
### Aliases: consolidateGroups
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, new.ids)
{
  m <- object$coefficients  
  m <- subset(m,rownames(m) %in% new.ids$probe_id)  
  new.ids <- subset(new.ids, new.ids$probe_id %in% rownames(m))
  m <- data.frame(m[as.character(new.ids$probe_id),])
  m <- apply(m,2,function(x) tapply(x,as.character(new.ids$new.gene_id),mean))

  p <- object$p.value
  p <- subset(p,rownames(p) %in% new.ids$probe_id)  
  p <- data.frame(p[as.character(new.ids$probe_id),])
  p <- apply(p,2,function(x) tapply(x,as.character(new.ids$new.gene_id), fisher.p))

  object$coefficients <- m
  object$p.value <- p
  return(object)
  }



cleanEx()
nameEx("decideTests")
### * decideTests

flush(stderr()); flush(stdout())

### Name: decideTests
### Title: Multiple Testing Across Genes and Contrasts
### Aliases: decideTests
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, method = "sliding", adjust.method = "BH", 
    p.value = 0.05, lfc = 0, tau=sqrt(2)) 
{
    if (!is(object, "MArrayLM")) 
        stop("Need MArrayLM object")
    if (is.null(object$p.value)) 
        object <- eBayes(object)
    method <- match.arg(method, c("separate", "global", "sliding",
        "hierarchical", "nestedF"))
    adjust.method <- match.arg(adjust.method, c("none", "bonferroni", 
        "holm", "BH", "fdr", "BY"))
    if (adjust.method == "fdr") 
        adjust.method <- "BH"
    switch(method, separate = {
        p <- as.matrix(object$p.value)
        for (j in 1:ncol(p)) {
            o <- !is.na(p[, j])
            p[o, j] <- p.adjust(p[o, j], method = adjust.method)
        }
        s <- sign(as.matrix(object$coefficients))
        results <- new("TestResults", s * (p < p.value))
        if (lfc > 0) 
          results@.Data <- results@.Data * (abs(object$coefficients) > lfc)
    }, global = {
        p <- as.matrix(object$p.value)
        o <- !is.na(p)
        p[o] <- p.adjust(p[o], method = adjust.method)
        s <- sign(as.matrix(object$coefficients))
        results <- new("TestResults", s * (p < p.value))
        if (lfc > 0) 
          results@.Data <- results@.Data * (abs(object$coefficients) > lfc)
    }, sliding = {
        p <- as.matrix(object$p.value)
        for (j in 1:ncol(p)) {
            o <- !is.na(p[, j])
            p[o, j] <- p.adjust(p[o, j], method = adjust.method)
        }
        m <- as.matrix(object$coefficients)
        m.a <- abs(m)
        beta <- log(4)/(tau-1)
        alf <- p.value/(1+exp(beta*(tau-m.a)))  
          # if (p.value=.05 & m.a=1) then alf=.01 regardless of tau
          #   tau defines the midpoint, ie where p=.5*p.value
        s <- sign(m)
        results <- new("TestResults", s * (p < alf))
    }, hierarchical = {
        stop("Can't handle hierarchical method yet")
    }, nestedF = {
        stop("Can't handle nestedF method yet")
    })
    results
  }



cleanEx()
nameEx("findDiscords")
### * findDiscords

flush(stderr()); flush(stdout())

### Name: findDiscords
### Title: Detection of Redundant Groups Differentially Expressed in
###   Opposite Directions
### Aliases: findDiscords
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(object) { 
  egs <- neweg2old(rownames(object))
  object <- subset(object, egs %in% get.duped(egs))
  if (nrow(object)==0) return ()

  egs <- neweg2old(rownames(object))  # reset after subsetting
  x <- tapply(rownames(object),egs,
              function(y) subset(object,rownames(object) %in% y),
              simplify=F)
  # creates a list of arrays, one for each egid

  if (ncol(object)>1) { 
    u <- sapply(x,function(y) {
      t(subset(t(y),apply(y,2,function(z) {
         length(unique(sign(subset(z,z!=0)))) > 1
      })))
    })
  } else {
     # when there is only one contrast
     u <- sapply(x,function(y) {
        length(unique(sign(subset(y[,1],y[,1]!=0)))) > 1
     })
  }
  u <- subset(u, sapply(u,function(v)ncol(v)>0))
  return (u)
  }



cleanEx()
nameEx("makeGroups")
### * makeGroups

flush(stderr()); flush(stdout())

### Name: makeGroups
### Title: Make Groups of Concordant Probe Sets for Consolidation
### Aliases: makeGroups
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (eset, ann.table=NULL, filtered=TRUE, alpha=.01) {
cat("Please be patient, this make take a few minutes.\n")

  if (!filtered)  eset <- filterGenes(eset)
  if (is.null(ann.table))  ann.table <- makeAnnTable(annotation(eset))
 
  exprs.eset <- exprs(eset)
  nc <- ncol(exprs.eset)
  r.crit <- getRcrit(alpha,nc)
  w.crit <- (1+r.crit)/2

  ann.table <- subset(ann.table, ann.table$probe_id %in% rownames(exprs.eset))
  multiples <- subset(ann.table,ann.table$Freq>1)
  new.ps.num <- by(multiples,multiples$gene_id,function(ss) {
    eset.rows <- subset(exprs.eset, rownames(exprs.eset) %in% ss$probe_id)
    if (nrow(eset.rows)>1) {
      mat <- cor(t(eset.rows),method="spearman")
      w   <- mean(mat,na.rm=TRUE) 
      if (w >= w.crit) {
        ss$ps.num <- paste(ss$ps.num,collapse='_')
      } else {
        sg <- getSubGraphs(mat,alpha,nc,w.crit)  # returns names not indices
        for (s in sg) {
          if (!is.numeric(s))  s <- sort(match(s,ss$probe_id))
          ss$ps.num[s] <- paste(ss$ps.num[s],collapse="_")
        }
      } 
    }
    ss$ps.num
  })
  multiples$ps.num <- unlist(new.ps.num,use.names=F)
  singles <- subset(ann.table,ann.table$Freq==1)
  multiples <- rbind(multiples,singles)
  new.eg.ids <- paste(multiples$gene_id,multiples$ps.num,sep='.')
  new.eg.ids <- paste(new.eg.ids,multiples$Freq,sep='.')  
  new.ids <- data.frame(probe_id=multiples$probe_id, new.gene_id=new.eg.ids)
  new.ids
  }



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
