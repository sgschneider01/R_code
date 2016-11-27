\name{scorem-package}
\alias{scorem-package}
\alias{scorem}
\docType{package}
\title{
SCOREM: Statistical Consolidation of Redundant Expression Measures
}
\description{
SCOREM is a tool for the consolidation of Affymetrix probe set expression values into gene- or transcript-specific expression values. SCOREM is intended to be used with the affy, gcrma, and limma packages, to produce an MArrayLM object with consolidated coefficients and p values, and a TestResults object of classified results.
}
\details{
\tabular{ll}{
Package: \tab scorem\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-06-30\cr
License: \tab LGPL\cr
LazyLoad: \tab yes\cr
}
Given an \code{ExpressionSet} object of normalized expression data (such as output by \code{\link[gcrma]{gcrma}}) and an \code{MArrayLM} object output by \code{\link[limma]{eBayes}}, \code{\link{makeGroups}} determines which probe sets are behaving concordantly and can be consolidated, \code{\link{consolidateGroups}} consolidates the values in the \code{MArrayLM} object, and \code{\link{decideTests}} produces the \code{TestResults} object. \code{\link{findDiscords}} can be performed on a \code{TestResults} object to locate discordant probe sets or groups for the same gene, i.e., groups that are indicate differential expression in opposite directions under the same test conditions (time point, treatment, etc.)
}
\author{
Stephanie G. Schneider
}
\references{
SCOREM: Statistical Consolidation of Redundant Expression Measures.  NAR 2011.
}
\keyword{ package }
\seealso{
 \code{\link[affy]{affy}}
 \code{\link[gcrma]{gcrma}}
 \code{\link[limma]{limma}}
}
\examples{
# my.eset is an ExpressionSet object output by gcrma
# my.fit is an MArrayLM object output by eBayes

new.ids <- makGroups(my.eset)
new.fit <- consolidateGroups(my.fit,new.ids)
dt <- decideTests(new.fit)
fd <- findDiscords(dt)

}
