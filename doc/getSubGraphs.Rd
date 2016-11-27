\name{getSubGraphs}
\alias{getSubGraphs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSubGraphs(object, alpha, nc, w.crit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{nc}{
%%     ~~Describe \code{nc} here~~
}
  \item{w.crit}{
%%     ~~Describe \code{w.crit} here~~
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
function (object,alpha,nc,w.crit) {

   r.crit <- getRcrit(alpha,nc) 
   # w.crit stays the same, r.crit increases with recursion

   cc <- connComp(as(new("graphAM",object>r.crit),"graphNEL"))  
   sg<-NULL
   for (x in cc) {
      mx <- object[x,x]
      w  <- mean(mx)
      n  <- length(x)

      if (n==2) {
        if (w < w.crit) { x <- as.list(x) } 
        # otherwise, leave cc intact
      } else if (n==3) { 
        if (w < w.crit) { 
          rs <- c(mx[1,2],mx[1,3],mx[2,3])
          if (max(rs)>r.crit) {             # consolidate two
            pairs  <- list(c(1,2),c(1,3),c(2,3))
            tog <- pairs[[which.max(rs)]]   # pair to consolidate
            sep <- setdiff(1:3,tog)         # single to leave out
            x <- list(x[tog],x[sep])
          } else {                          # don't consolidate any
            x <- as.list(x) 
          }
        } 
      } else { 
        if (w < w.crit) { x <- getSubGraphs(mx,alpha,nc-2,w.crit) } 
        # otherwise, leave cc intact
      }

     if (!is.list(x))  { x <- list(x) }  # NOTE: different than as.list(x)!!
     sg <- c(sg,x)
   }
   sg
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
