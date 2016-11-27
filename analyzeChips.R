analyzeChips <- function (x,y="gcrma") {

require(affy)
require(gcrma)

if (length(x)>1) { chips <- ReadAffy(filenames=x) 
} else           { chips <- ReadAffy() }

switch (y,
     rma =   { chips.norm <- rma(chips) },
     gcrma = { chips.norm <- gcrma(chips) },
     mas5 =  { chips.norm <- mas5(chips) }
)

cat("CEL files loaded and normalized, returning eset.\n")


return(chips.norm)
}