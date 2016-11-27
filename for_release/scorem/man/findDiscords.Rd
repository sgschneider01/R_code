\name{findDiscords}
\alias{findDiscords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Detection of Redundant Groups Differentially Expressed in Opposite Directions
}
\description{
Given an \code{TestResults} object output by \code{\link{decideTests}}, returns a list of matrices for all genes with discordant groups, i.e., groups that are differentially expressed in opposite directions.  
}
\usage{
findDiscords(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
   \code{TestResults} object output by \code{decideTests}.
  }
}
\details{
Identifies genes with with discordant groups, i.e., groups that are differentially expressed in opposite directions, and for each extracts a subset of the \code{TestResults} matrix containing the discordant values.
}
\value{
A list containing subset of the \code{TestResults} matrix where discordant expression patterns are observed.
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
\code{\link{decideTests}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(object) { 
  egs <- neweg2old(rownames(object))
  object <- subset(object, egs \%in\% get.duped(egs))
  if (nrow(object)==0) return ()

  egs <- neweg2old(rownames(object))  # reset after subsetting
  x <- tapply(rownames(object),egs,
              function(y) subset(object,rownames(object) \%in\% y),
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
