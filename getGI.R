get.gi <- function (accnum) {
  my.url <- paste("http://www.ncbi.nlm.nih.gov/nuccore/", accnum,
                  "?report=gilist", sep='')
#  x <- scan(my.url,what="character",sep="\n",quiet=TRUE)
  x <- readLines(my.url,warn=FALSE)
  x <- subset(x, grepl("^[0-9]+$",x))
  x
}

#[1] "<?xml version=\"1.0\" encoding=\"utf-8\"?>"                                                                                   
#[2] "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
#[3] "<pre>"                                                                                                                        
#[4] "284005175"                                                                                                                    
#[5] "</pre>"                                                                                                                       



    if (length(gi) == 0) {
        stop(paste("Can't obtain a gi number for", accNum))
    }
    else {
        return(gi)
    }

  x <- tryCatch(readLines(my.url,warn=FALSE))
