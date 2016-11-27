#save each comparison result list in a separate object (using only those diff reg in that column)

#erb vs ad, +/- e2
gse4006.ade2.adveh<-gse4006.dt.ann[which(gse4006.dt.ann[,2]!=0),c(1,2,6,7,8)]
gse4006.erbe2.erbveh<-gse4006.dt.ann[which(gse4006.dt.ann[,3]!=0),c(1,3,6,7,8)]
gse4006.erbe2.ade2<-gse4006.dt.ann[which(gse4006.dt.ann[,4]!=0),c(1,4,6,7,8)]
gse4025.ade2.adveh<-gse4025.dt.ann[which(gse4025.dt.ann[,2]!=0),c(1,2,6,7,8)]
gse4025.erbe2.erbveh<-gse4025.dt.ann[which(gse4025.dt.ann[,3]!=0),c(1,3,6,7,8)]
gse4025.erbe2.ade2<-gse4025.dt.ann[which(gse4025.dt.ann[,4]!=0),c(1,4,6,7,8)]
gse9936_4.ade2.adveh<-gse9936_4.dt.ann[which(gse9936_4.dt.ann[,2]!=0),c(1,2,6,7,8)]
gse9936_4.erbe2.erbveh<-gse9936_4.dt.ann[which(gse9936_4.dt.ann[,3]!=0),c(1,3,6,7,8)]
gse9936_4.erbe2.ade2<-gse9936_4.dt.ann[which(gse9936_4.dt.ann[,4]!=0),c(1,4,6,7,8)]
gse9936_24.ade2.adveh<-gse9936_24.dt.ann[which(gse9936_24.dt.ann[,2]!=0),c(1,2,6,7,8)]
gse9936_24.erbe2.erbveh<-gse9936_24.dt.ann[which(gse9936_24.dt.ann[,3]!=0),c(1,3,6,7,8)]
gse9936_24.erbe2.ade2<-gse9936_24.dt.ann[which(gse9936_24.dt.ann[,4]!=0),c(1,4,6,7,8)]

gse4006.erb.ad.both<-intersect(gse4006.erbe2.erbveh$hgu133aSYMBOL,gse4006.ade2.adveh$hgu133aSYMBOL)
gse4006.erb.ad.eronly<-setdiff(gse4006.erbe2.erbveh$hgu133aSYMBOL,gse4006.ade2.adveh$hgu133aSYMBOL)

gse4025.erb.ad.both<-intersect(gse4025.erbe2.erbveh$hgu133aSYMBOL,gse4025.ade2.adveh$hgu133aSYMBOL)
gse4025.erb.ad.eronly<-setdiff(gse4025.erbe2.erbveh$hgu133aSYMBOL,gse4025.ade2.adveh$hgu133aSYMBOL)

gse9936_24.erb.ad.both<-intersect(gse9936_24.erbe2.erbveh$hgu133aSYMBOL,gse9936_24.ade2.adveh$hgu133aSYMBOL)
gse9936_24.erb.ad.eronly<-setdiff(gse9936_24.erbe2.erbveh$hgu133aSYMBOL,gse9936_24.ade2.adveh$hgu133aSYMBOL)


#era vs ad, +/- e2
gse2251.ade2.adveh<-gse2251.dt.ann[which(gse2251.dt.ann[,3]!=0),c(1,3,6,7,8)]
gse2251.erae2.eraveh<-gse2251.dt.ann[which(gse2251.dt.ann[,2]!=0),c(1,2,6,7,8)]
gse2251.erae2.ade2<-gse2251.dt.ann[which(gse2251.dt.ann[,4]!=0),c(1,4,6,7,8)]


length(intersect(gse2251.ade2.adveh$hgu133aSYMBOL,gse4006.ade2.adveh$hgu133aSYMBOL))
length(intersect(gse2251.ade2.adveh$hgu133aSYMBOL,gse4025.ade2.adveh$hgu133aSYMBOL))
length(intersect(gse2251.ade2.adveh$hgu133aSYMBOL,gse9936_24.ade2.adveh$hgu133aSYMBOL))
length(intersect(gse4006.ade2.adveh$hgu133aSYMBOL,gse4025.ade2.adveh$hgu133aSYMBOL))
length(intersect(gse4006.ade2.adveh$hgu133aSYMBOL,gse9936_24.ade2.adveh$hgu133aSYMBOL))
length(intersect(gse4025.ade2.adveh$hgu133aSYMBOL,gse9936_24.ade2.adveh$hgu133aSYMBOL))
