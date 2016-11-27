#source("~/Desktop/research/notes/R_code/load-all.R")

library(affy)
library(gcrma)
library(genefilter)
library(limma)
library(RBGL)
library(graph)
library(GOstats)
library(XML)

# affymetrix annotation packages
library(hgu133a.db)
library(hgu133plus2.db)
library(mouse4302.db)
library(mouse430a2.db)

library(hgu133aprobe)
library(hgu133plus2probe)
library(mouse4302probe)

data(hgu133aprobe)
data(hgu133plus2probe)
data(mouse4302probe)

# brainarray annotation packages
library(hgu133ahsentrezg.db)
library(hgu133ahsentrezgprobe)
library(hgu133plus2hsentrezg.db)
library(hgu133plus2hsentrezgprobe)
library(mouse4302mmentrezg.db)
library(mouse4302mmentrezgprobe)

data(hgu133ahsentrezgprobe)
data(hgu133plus2hsentrezgprobe)
data(mouse4302mmentrezgprobe)

setwd("~/Desktop/research/notes/R_objects")

source("~/Desktop/research/notes/R_code/do-all-kendall.R")
source("~/Desktop/research/notes/R_code/handyFunctions.R")

source("~/Desktop/research/notes/R_code/addAnnotation.R")
source("~/Desktop/research/notes/R_code/addAnnot.R")
source("~/Desktop/research/notes/R_code/analyzeChips.R")
source("~/Desktop/research/notes/R_code/analyzeGEO.R")
source("~/Desktop/research/notes/R_code/byGeneW.R")
source("~/Desktop/research/notes/R_code/consolidatePS.R")
source("~/Desktop/research/notes/R_code/new_decide_test.R")
source("~/Desktop/research/notes/R_code/myDecideTests.R")
source("~/Desktop/research/notes/R_code/filterGenes.R")
source("~/Desktop/research/notes/R_code/getCC.R")
source("~/Desktop/research/notes/R_code/getPairwiseMatrix.R")
source("~/Desktop/research/notes/R_code/getPsig.R")
source("~/Desktop/research/notes/R_code/getR.R")
source("~/Desktop/research/notes/R_code/getRcrit.R")
source("~/Desktop/research/notes/R_code/getRows.R")
source("~/Desktop/research/notes/R_code/getSubGraphs.R")
source("~/Desktop/research/notes/R_code/makeDesign.R")
source("~/Desktop/research/notes/R_code/makeGSE.R")

source("~/Desktop/research/notes/R_code/plotGenomic.R")
source("~/Desktop/research/notes/R_code/plotGenDouble.R")
source("~/Desktop/research/notes/R_code/getVenn.R")

to.do <- list( c("GSE4799","mouse4302"),
               c("GSE4051","mouse4302"),
               c("GSE3678","hgu133plus2"),
               c("RO","mouse4302"),
               c("KK","mouse430a2"),
               c("katz","hgu133a"),
               c("isis_if","mouse4302"),
               c("isis_gf","mouse4302")#,
#               c("isis_lv","mouse4302")
#               c("GSE974","hgu133a")
)


# LOAD ALL THE SAVED DATA FILES
#for (td in to.do) {

#   exp.name     <- td[[1]]
#   exp.platform <- td[[2]]
#setwd(paste("~/Desktop/research/notes/R_objects/",exp.name,sep=''))
#
#load(paste(exp.name,"fit3","RData",sep='.'))
#
#}

