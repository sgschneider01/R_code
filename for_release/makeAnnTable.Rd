\name{makeAnnTable}
\alias{makeAnnTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeAnnTable(exp.plat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{exp.plat}{
%%     ~~Describe \code{exp.plat} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (exp.plat) {
  require(paste(exp.plat,"db",sep='.'),character.only=T)
   xx <- toTable(do.eval(paste(exp.plat,"ENTREZID",sep="")))
   xx <- cbind(xx, ps.num=1)
   xy <- subset(xx,!duplicated(xx$gene_id))
   dups <- subset(xx,duplicated(xx$gene_id))
   for (i in 2:max(table(xx$gene_id))) { 
      ones <- subset(dups,!duplicated(dups$gene_id))
      ones$ps.num <- i
      dups <- subset(dups,duplicated(dups$gene_id))
      xy <- rbind(xy,ones)
   }
   xy <- xy[order(xy$gene_id),]
   tab <- as.data.frame(table(xy$gene_id))
   xy <- merge(xy, tab, by.x="gene_id", by.y=1)
   return(xy)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
