#!/usr/bin/Rscript
require(roxygen2)
roxygenise(clean=T)

setwd("..")
install.packages("miscfun", lib=.libPaths()[1], repos=NULL)
