library(e1071)
library(doParallel)
library(quantmod)
library(zoo)

dir      <- "SVRGARCHKDE_Functions"
files    <- list.files(dir)
files    <- files[-which(files %in% c("Archiv", "Dev", "Source_File","Source_SVRGARCHKDE.R"))]
filepath <- paste(dir, files, sep = "/")
sapply(filepath, source)
rm(dir, files, filepath)