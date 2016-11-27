#####################################################################################
##  perGene.r is the R program for combined sibling probe sets analysis
## will take a data matrix for affymetrix array and an
## annotation file, fit ANOVA model, and then report p-values of 
## interested factors based on consolidated probe set whenever 
## applicable based on interaction detection.
##    object - a matrix of expression values, first column=ID (affy id)
##    anno   - a matrix of two columns, ID (affy id) and GENE (entrez gene or symbol)
#####################################################################################

perGene <- function( object, annofile=anno, design, adj.meth="BH", alpha=.01)  {
   uniqueGenes<-unique(anno$GENE)
   ngenes<-length(uniqueGenes)
   comP<-matrix(0, ncol=3, nrow=ngenes)
   narrays<-dim(object)[2]   #except that ID is the first column

   for (i in 1:ngenes) {
      d <- 1
      tmp<-object[which(anno$GENE == uniqueGenes[i]),]
      if (is.matrix(tmp)) { d<-dim(tmp)[1] }

      if (d == 1) {
        trt<-as.factor(design)
        tmp<-as.numeric(t(tmp))
        singleGene<-as.data.frame(cbind(tmp, trt))
        res<-anova(lm(tmp~as.factor(trt), data=singleGene))
        comP[i,]<-c(res$Pr[1],NA, NA)
      }
      if (d > 1) {
        trt<-rep(design, each=d)
        v<-rep(seq(1:d), narrays)
        tmp<-as.matrix(tmp)
        tmp<-as.vector(tmp)
        tmp<-as.numeric(tmp)
        v<-as.factor(v)
        trt<-as.factor(trt)
        singleGene<-as.data.frame(cbind(tmp, trt, v))
        res <- anova(lm(tmp~as.factor(trt)+as.factor(v)+as.factor(trt)*as.factor(v), data=singleGene))
        comP[i,]<-res$Pr[1:3]
      }
    }
    colnames(comP) <- c( "trt", "v", "trt*v")
    rownames(comP) <- uniqueGenes
return(comP)

    comP<-comP[order(comP[,3]),]
    nsingles<-as.numeric(summary(comP[,3]=="NA")[2])
    inter<-comP[1:nsingles,3]

    interadj<-mt.rawp2adjp(inter, adj.meth)
    interadj<-interadj$adjp[order(interadj$index),]
return(interadj)


    nInterSig<-as.numeric(summary(interadj[,2]<=alpha)[3])

    genes_sig<-rownames(comP)[1:nInterSig]

    genenames_sig<-anno[which(anno$GENE %in% genes_sig),]
    genes_nosig<-rownames(comP)[(nInterSig+1):ngenes]

    uniqueGenesUpdate<-c(genes_nosig, as.vector(genes_sig))

    genes_rerun<-as.vector(genenames_sig$ID)
    genesNames_rerun<- paste(genenames_sig[,1], genenames_sig[,2], sep="$")  #### gene names in new list

    ngenes_rerun<-length(genes_rerun)

    p_rerun<-rep(0, ngenes_rerun )

    for (i in 1: ngenes_rerun) {
       tmp<-object[which(anno$ID == genes_rerun[i]),]
       trt<-as.factor(design)
       tmp<-as.numeric(t(tmp))
       singleGene<-as.data.frame(cbind(tmp, trt))
       res<-anova(lm(tmp~as.factor(trt), data=singleGene))
       p_rerun[i]<-c(res$Pr[1])
    }

    p_org<-comP[(nInterSig+1):ngenes,1]
    pfinal<-c(p_org, p_rerun)
    names(pfinal)<-c(genes_nosig, genesNames_rerun)
    return (pfinal)
}