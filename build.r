#!/usr/bin/Rscript
require(roxygen2)
roxygenise()
setwd("..")
install.packages("miscfun", lib=.libPaths()[1], repos=NULL)
