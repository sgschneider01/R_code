\name{decideTests}
\alias{decideTests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Multiple Testing Across Genes and Contrasts
}
\description{
  A modified version of the limma package method.  Classify a series of related t-statistics as up, down or not significant. A number of different multiple testing schemes are offered which adjust for multiple testing down the genes as well as across contrasts for each gene.
}
\usage{
decideTests(object, method = "sliding", adjust.method = "BH", p.value = 0.05, lfc = 0, tau = sqrt(2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
   \code{MArrayLM} object output from \code{\link{consolidateGroups}} from which the t-statistics may be extracted.
}
  \item{method}{
   character string specify how probes and contrasts are to be combined in the multiple testing strategy. Choices are "separate", "global", "hierarchical", "nestedF", "sliding", or any partial string.
}
  \item{adjust.method}{
   character string specifying p-value adjustment method. Possible values are "none", "BH", "fdr" (equivalent to "BH"), "BY" and "holm". See p.adjust for details.
}
  \item{p.value}{
   numeric value between 0 and 1 giving the desired size of the test
}
  \item{lfc}{
   minimum log2-fold-change required
}
  \item{tau}{
  numeric value between 1 and 2 setting the steepness of the sigmoid function used by the "sliding" method.  As tau approaches 1, the curve approaches the hard cutoff produced by method "separate".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of class \code{TestResults}. This is essentially a numeric matrix with elements -1, 0 or 1 depending on whether each t-statistic is classified as significantly negative, not significant or significantly positive respectively.
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
\seealso{
\code{\link[decideTests]{limma}},\code{\link{consolidateGroups}}

}
\examples{
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
