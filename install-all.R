source("http://bioconductor.org/biocLite.R")
# biocLite("pkg_name") # to install new bioconductor packages
# install.packages("pkg_name") # to install new CRAN packages


biocLite()

#Using R version 2.13.0, biocinstall version 2.8.4.
#Installing Bioconductor version 2.8 packages:
# "affy"          "affydata"      "affyPLM"       "affyQCReport"  "annaffy"       
# "annotate"      "Biobase"       "biomaRt"       "Biostrings"    "DynDoc"        
# "gcrma"         "genefilter"    "geneplotter"   "GenomicRanges" "hgu95av2.db"   
# "limma"         "marray"        "multtest"      "vsn"           "xtable"       

# also installing the dependencies ‘RSQLite’, ‘bitops’, ‘affyio’, 
# ‘preprocessCore’, ‘RColorBrewer’, ‘simpleaffy’, ‘GO.db’, ‘KEGG.db’, 
# ‘AnnotationDbi’, ‘DBI’, ‘XML’, ‘RCurl’, ‘IRanges’, ‘org.Hs.eg.db’


biocLite(c("RBGL","graph","GOstats"))

#also installing the dependencies ‘GSEABase’, ‘Category’


biocLite(c("hgu133a.db",   "hgu133plus2.db",   "mouse4302.db", "mouse430a2.db", 
           "hgu133aprobe", "hgu133plus2probe", "mouse4302probe"))

# also installing the dependency ‘org.Mm.eg.db’

biocLite(c("hgu133ahsentrezg.db", "hgu133ahsentrezgprobe", "hgu133plus2hsentrezg.db", "hgu133plus2hsentrezgprobe", "mouse4302mmentrezg.db", "mouse4302mmentrezgprobe"),type='source')  

