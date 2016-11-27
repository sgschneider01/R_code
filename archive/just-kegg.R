pool.list <- list( c("t.3.4","hgu133a2"),
                   c("t.3.4","hgu133a2"),
                   c("t.3.4","hgu133a2") )


for (pl in pool.list) {
   f4 <- do.eval(pl[1])

 # Step 3:  Get a list of the over-represented KEGG categories in this matrix
 #            ie ALL contrasts in this contrast SET (eg M25, R3)
    kegg.or <- getORKCp (pl[2], f4$kegg, .05)
    kegg.or <- as.character(kegg.or)
    assign (paste(pl[1],"orkcs",sep="."), kegg.or, pos=1)
 cat("step 3 getKEGGchisq done\n")
    kegg.or <- getKEGGchisq (pl[2], f4$kegg, .05)
    kegg.or <- as.character(kegg.or)
    assign (paste(pl[1],"chi",sep="."), kegg.or, pos=1)
next

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


}