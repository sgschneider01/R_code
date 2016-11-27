#source("~/Desktop/research/notes/R_code/do-all.R")
source("~/Desktop/research/notes/R_code/handyFunctions.R")
source("~/Desktop/research/notes/R_code/analyzeGEO.R")
source("~/Desktop/research/notes/R_code/analyzeResults.R")
#source("~/Desktop/research/notes/R_code/getORKCp.R")
source("~/Desktop/research/notes/R_code/addEGID.R")
#load("/Users/arwen/Desktop/research/notes/R_workspaces/wkspc-3pools.R")
#if (!exists("GSM92160"))  load("/Users/arwen/Desktop/research/notes/R_workspaces/wkspc-gsm216.R")

pool.list <- list(
#                   c("T6", "bcpool", "hgu133a2", "teal"),
#                   c("B6", "bcpool", "hgu133a2", "purple"),
#                   c("D4", "bcpool", "hgu133a2", "green"),
#                   c("BC6", "bcpool", "hgu133a2", "blue"),
#                   c("BC12", "bcpool", "hgu133a2", "teal"),
#                   c("BC24", "bcpool", "hgu133a2", "purple"),
#                   c("nodiff3","nodiff","hgu133a2","orange"),
#                   c("M116","M116", "hgu133a2", "orange"),
#                   c("BC24", "bcpool", "hgu133a2", "purple"),
#                   c("KK", "KKonly", "mouse4302", "red"),
#                   c("RO9", "ROonly", "mouse4302", "pink"),
#                   c("M125", "M125", "hgu133plus2", "blue")#,
#                   c("katz26", "katz26", "hgu133plus2", "blue")#,
#                   c("isis", "isis", "hgu133a2", "pink")#,
                   c("isis_gf", "isis_gf", "hgu133a2", "pink"),                   
                   c("isis_if", "isis_if", "hgu133a2", "pink"),                   
                   c("isis_lv", "isis_lv", "hgu133a2", "pink")#,                   
#                   c("rat","rat",NULL,"teal")
                 )

options.list <- cbind( pval = c(.05,.01),
                       mval = c(1.5,2),
                       genf = c(FALSE,TRUE),
                       adjm = c("none","BH") )

#  gf=TRUE, pval=.01, mval=log2(1.5), adjm="none"


for (pl in pool.list) {

 # Step 1:  Get matrix (m and values for each probe set in each contrast)
    f3 <- paste(pl[1],"fit3",sep='.')
    if (!exists(f3)) assign(f3, analyzeGEO(ctr.list=pl[1],gse=pl[2],gf=TRUE))
cat("analyzeGEO done for",pl[1],"\n")

    f4 <- do.eval(f3)
    f4 <- addEGID(f4,pl[3])
    f4 <- f4[which(f4$egid!="NA"),]
    f4 <- do.kendall(f4)
    assign(paste(pl[1],"fit4",sep='.'), f4)

next

# Step 2: Get direction in each contrast where m, p are sig; add counts, annot
#f3.mp<-merge(f3$coefficients,f3$p.value,by=0,suffixes=c(".m",".p"))
#colnames(f3.mp)[1]<-"affy.id"   #merge() makes column [,1] "Row.names"

#    f4 <- paste("fit4.",pl[1],sep='')
#    if (!exists(f4))  assign(f4, analyzeResults(f3, aff.chip=pl[3], 
#                             pval=.01, mval=log2(1.5), adjm="none") )
#assign(f4,addEGID(f3.mp,aff.chip=pl[3]))
#    f4 <- do.eval(f4)

#write.table(f4,paste("results/",pl[2],"-fit4.txt",sep=''),sep='\t',quote=FALSE)

# cat("step 2 analyzeResults done\n")



 # Step 3:  Get a list of the over-represented KEGG categories in this matrix
 #            ie ALL contrasts in this contrast SET (eg M25, R3)
    kegg.or <- getORKCp (pl[3], f4$kegg, .05)
    kegg.or <- as.character(kegg.or)
    assign (paste(pl[1],"orkcs",sep="."), kegg.or, pos=1)
 cat("step 3 done\n")

# next # breaks the loop before Step 4


 # Step 4:  Make a subset of the matrix for each KEGG category.
    for (k.id in kegg.or) {
       gr <- grep(k.id,f4$kegg)
#       f5 <- data.frame ( affy.id = I(as.character(f4$affy.id[gr])),
#                          kegg.id = I(as.character(f4$egid[gr])),
#                          direction = sign(f4$sum[gr]) )
f5<-f4[gr,]
       fn <- paste(pl[1],k.id,sep="_")
       assign(fn,f5)
#      write.table(f5,file=paste("R_output",fn,sep="/"))
   }

#   Step 5:  Generate a file of the coloring instructions for KEGG coloring maps

}