\name{makeGroups}
\alias{makeGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Make Groups of Concordant Probe Sets for Consolidation
}
\description{
Given an \code{ExpressionSet} object of normalized data, determines the concordance of redundant probe sets (representing the same gene), producing a data frame of old probe set ids with new ids indicating which can be consolidated.
}
\usage{
makeGroups(eset, ann.table = NULL, filtered = TRUE, alpha = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eset}{
    \code{\link[Biobase]{ExpressionSet}} object output by \code{\link{gcrma}} or other normalization algorithm.
}
  \item{ann.table}{
    data frame output from makeAnnTable.
}
  \item{filtered}{
    logical value indicating whether or not the data has already been filtered. If FALSE, \code{genefilter} is performed to remove probe sets with extremely low variance.
}
  \item{alpha}{
    numerical value between 0 and 1, indicating the level of significance required for correlation to be considered significant.
}
}
\details{

}
\value{
A data frame object mapping probe set ids to group ids.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Stephanie G. Schneider
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[gcrma]{gcrma}}
}
\examples{
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

  ann.table <- subset(ann.table, ann.table$probe_id \%in\% rownames(exprs.eset))
  multiples <- subset(ann.table,ann.table$Freq>1)
  new.ps.num <- by(multiples,multiples$gene_id,function(ss) {
    eset.rows <- subset(exprs.eset, rownames(exprs.eset) \%in\% ss$probe_id)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
