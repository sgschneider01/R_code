getAffyLocs <- function(egid,sp) {
  switch(sp,
    "Mm" =   { eg <- as.list(mouse4302ENTREZID)
               affx.probe<-as.data.frame(mouse4302probe) },
    "Hs" =   { eg <- as.list(hgu133plus2ENTREZID)
               affx.probe<-as.data.frame(hgu133plus2probe) }
  )
  ps <- subset(names(eg),eg==egid)
  locs <- subset(affx.probe[,5],affx.probe$Probe.Set.Name %in% ps)
  probe.locs <- matrix(locs,ncol=11,byrow=T)
  rownames(probe.locs) <- ps
  probe.locs
}
