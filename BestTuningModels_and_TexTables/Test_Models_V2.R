###############################################################################
# Description: Load data with true value and VaR-Forecast to analyze and test
# results. The results are combined in a table. This table can be saved to the
# TEX-table used in the thesis by uncommenting the last part of the code. The
# name of the saved table is 'df_sorted' in the script.
# 
###############################################################################
# Dependencies: Fuchtions in 'Result2Table_Functions.R' which is located under 
# the path 'SVRGARCHKDE_Functions/Dev'.
#
###############################################################################
# Author: Marius Lux 2017-03-26
#
###############################################################################

rm(list = ls())
graphics.off()

setwd(paste0(Sys.getenv("USERPROFILE"), "/Desktop/Master/Master_Thesis/Code_Abgabe"))

library(data.table)
library(xtable)

source("SVRGARCHKDE_Functions/Dev/Result2Table_Functions.R")


# Get path to rds-files
result_files       <- file.path("Results/Tuning_2006to2011_FCast_2011to2016_MeanZero", list.files("Results/Tuning_2006to2011_FCast_2011to2016_MeanZero", pattern = ".rds"))

# Aggregate data in list
result_data        <- lapply(result_files, readRDS)

# Specify name of model for corresponding rds-file
n                  <- length(result_files)
elements_n         <- length(strsplit(result_files, "_")[[1]])
mdl                <- unlist(lapply(1:n, function(x) strsplit(result_files, "_")[[x]][(elements_n-2)]))
series             <- unlist(lapply(1:n, function(x) strsplit(result_files, "_")[[x]][(elements_n-1)]))
names(result_data) <- mdl

# Get results of tests
probs              <- c(0.01, 0.025, 0.05)  # x%-Quantiles used in results table
list_table         <- lapply(c(1:n), function(x) arrange_results_df(result_data[[x]], probs, mdl[x], series[x]))

# Convert list to data frame (use df_sorted in thesis)
df                 <- Reduce("rbind", list_table)
df_sorted          <- df[order(-df$Quantile, df$Series, df$CC, df$Model, decreasing = TRUE),]


# Analyze mean ranks
test_quant     <- c(1, 2.5,5)  # Specify quantiles for mean rank analysis
df_sub         <- df_sorted[df_sorted$Quantile %in% test_quant,]
df_sub$Model   <- as.character(df_sub$Model)
df_sub$Model[df_sub$Model == "SVM-KDE"] <- "SVR-KDE"

dt             <- as.data.table(df_sub)
dt[, rank := frankv(CC, ties.method = "min", order = -1), by = list(Quantile, Series)]
mean_dt <- dt[, mean(rank), by = Model]
mean_dt[order(V1, decreasing = F)]


###############################################################################
# Make tex table
###############################################################################

# Define function to save results as tex table
save_xtable <- function(dataset){
  
  df_sub             <- df_sorted[df_sorted$Quantile == dataset, ]
  df_sub_sorted      <- df_sub #df_sub[order(-df_sub$Quantile, df_sub$Order, decreasing = TRUE),]
  df_sub_sorted$Model <- toupper(df_sub_sorted$Model)
  
  df_sub_sorted <- df_sub_sorted[,c("Model", "Series","Quantile", "Exceedances", "UC", "ID", "CC")]
  names(df_sub_sorted)[c(2,4)] <- c("Index", "Violations")
  
  df_sub_sorted$Model[substr(df_sub_sorted$Model, 1, 10) == "SGARCH-NOR"] <- "GARCH-NORM"
  df_sub_sorted$Model[substr(df_sub_sorted$Model, 1, 10) == "SGARCH-STD"] <- "GARCH-STD"
  df_sub_sorted$Model[substr(df_sub_sorted$Model, 1, 10) == "SGARCH-SST"] <- "GARCH-SSTD"
  
  
  df_sub_sorted <- sapply(df_sub_sorted, as.character)
  
  #file_title <- ifelse(dataset == "S&P500", "SP500", dataset)
  file_title <- gsub("[.]", "", as.character(dataset))
  savePath <- paste0("C:/Users/Marius/Desktop/Master/Master_Thesis/Text/Paper_Latex/thesis_complete/Tables/Case_1/"
                     , "Results_Case1_", file_title, ".tex")
  
  
  tex_caption <- paste0("Results for the VaR forecasts from July 1, 2011 to June 30, 2016 for $\\alpha =  ",
                        ifelse(dataset == "S&P500", "Standard and Poors 500", dataset)/100,
                        "$. UC, ID and CC indicate the p-value of the corresponding LR test. The results are in descending order with respect to CC for each index. All values in the columns Quantile, Violations, UC, ID and CC are given in percent.")
  
  
  
  bold <- function(x) {paste('\\textbf{',x,'}', sep ='')}
  
  tex_raw <- xtable(df_sub_sorted, caption = tex_caption, label = paste0("Tab:case1_", dataset))
  align(tex_raw) <- "rl|c|c|c|c|c|c"
  invisible(print(tex_raw
               , include.rownames = FALSE
               , hline.after = c(-1, 0, 0, 10, 10, 20, 20, 30, 30)
               , sanitize.colnames.function=bold
               , type = "latex"
               , size="\\fontsize{11pt}{12pt}\\selectfont"
               , file = savePath
               ))

}

# Uncomment to save as tex-table
# lapply(probs*100, save_xtable)

