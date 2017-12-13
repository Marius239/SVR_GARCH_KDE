# SVR_GARCH_KDE
This repo containes alle the code for using SVR-GARCH-KDE hybrid to forecast Value-at-Risk.

###############################################################################
# Manual for using the codes
###############################################################################

Remark 1: Note that the following packages are used:
beepr, rugarch, e1071, quantmod, zoo, doParallel, R.utils, xtable, plotly
and data.table

Remark 2: The paths in the script 'SVM_KDE_Functions_V6.R' need to be adjusted
manually. This needs to be done for the functions that go parallel. Otherwise
R does not know the correct environment. Hence, it is not enough to set the
working directory.

Remark 3: The folder contain already the results. So, code execution is not
necessary.

*1. Data*
Open the folder 'Data' and go to 'R_Code'. Adjust the path in 'setwd(...)' and
run the code. The data sets will be loaded and constructed. Everything is saved
under the folder 'Data'.

*2. Benchmark Methods'
Open the folder 'Benchmark_Methods' and run 'Apply_GARCH_Models_V2.R'
The resulting RDS-files are saved under the 'Results'-folder.
The file 'GARCH_Models_Functions.R' contains the relevant functions.

*3. SVR-GARCH-KDE Functions*
Functions for estimating the SVR-GARCH-KDE hybrid are contained in the folder 
'SVRGARCHKDE_Functions'

*4. Tuning*
The folder 'Tuning' contains the code 'Tune_VaR_Model.R' which is used to apply
grid search for the SVR-GARCH-KDE hybrid. The time series needs to be set. All
results are saved under the corresponding 'Results'-folder in 'Tuning'

*5. Track_Parallel*
When going parallel, it is not possible to just track the the computation steps by
printing to the console. Hence, CSV-files are saved to this folder during tuning
to monitor the process.

*6. Final SVR-GARCH-KDE Analysis*  
To estimate the final model based on the tuning results use the code 'Apply_SVM_KDE_V6.R'
in 'SVRGARCHKDE_Final_Analysis'. Here, the code 'Create_Best_List' from the folder 
'BestTuningModels_and_TexTables' is used to find the model parameters of the best
models from the tuning process. All results are saved in the results folder.

*7. Compute LR test statistics*
Under the path 'SVRGARCHKDE_Functions\Dev' is the script 'Result2Table_Functions.R'.
This script computes Christoffersen's LR test statistics. This is based on the
rugarch package. However, the modification mentioned at the 
end of Section 2.4 is not implemented. Hence, this modification was added by myself.

*8. Thesis Tables*
In the folder 'BestTuningModels_and_TexTables' are the codes 'Create_Best_List_TexTable.R'
and 'Test_Models_V2'. These produce the tables for the tuning results and model
comparison in the empirical study, respectively. 

*9. Thesis Figures*
In the folder 'Figures' are the codes that produce and save the figures in for 
the thesis. The code 'Feature_Mapping_SVR.R' does not produce the exact figure as
in the thesis. This is due to the limited capabilities of plotly to make 
appropriate axes. This was done by using paint.
