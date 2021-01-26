
#Helloooooooooooo

#Will have to change working directory and data locations: 130 and 173 lines.
#Run until line 163.
#For a summary of the results check: .html report output

rm(list = ls(all = TRUE))
#devtools::session_info()

######## To rerun a new drake pipeline
#clean(destroy = TRUE)
#cached()
########

#setwd("~/Desktop/COVID2021/CovidSurg")

tic()


WorkingDir <- getwd()
source(paste0(WorkingDir,"/CovidSurgPackages.R")) #all libraries needed are here - check they are installed.
source(paste0(WorkingDir,"/FunctionsCovidSurg.R"))

registerDoFuture() #make sure parallelization is working
future::plan(multisession, workers = availableCores())

#check future is working fine

x <- list(1, 10, 100)
y <- list(1, 2, 3)
z <- list(5, 50, 500)

future_map2(x, y, ~ .x + .y)


library(drake) # before problem with workflow now no - nio verlap of functions: so add this later

##### function for MICE or no imputation - interchange
source(paste0(WorkingDir,"/CovidSurgNoImputationMissingVals.R")) 
#source(paste0(WorkingDir,"/CovidSurgMissingValuesImputationDistribution.R"))
#####
source(paste0(WorkingDir,"/CovidSurgFeatureSelection.R"))
source(paste0(WorkingDir,"/CovidSurgSplit.R"))
source(paste0(WorkingDir,"/CovidSurgVariables.R"))
source(paste0(WorkingDir,"/CovidSurgBuildResults.R"))
source(paste0(WorkingDir,"/CovidSurgHeatmapPlots.R"))
source(paste0(WorkingDir,"/CovidSurgCorrelation.R"))
source(paste0(WorkingDir,"/CovidSurgSecondPartCoeffs.R")) # only because chosen model was logit
source(paste0(WorkingDir,"/CovidSurgCalibration.R"))    
source(paste0(WorkingDir,"/CovidSurgAUC.R"))

## ---------------------User Input  --------------------------------------------------------------------------------

# These are the three parameteres that you can change - with this set up 5 mins computation. Also hh in 85 can be changed
# for function FeatureSelectionUserInput - so function outside. 

Bootstraps <- 2

# Models chosen to work

# 1 -logit
# 2 -rf
# 3 -dt
# 4 -bt
# 5 - dtR
#SelectedModels <- c(1, 2, 3)
SelectedModels <- c(1,2,3)

ChosenRun <- 12 #this is the run with the final model 


## ---------------------User Input Functions --------------------------------------------------------------------------------


FeatureSelectionUserInput <- function(PlotsNames) {
 
   KeyFeats <- c(PlotsNames[[1]]$`Variable Name`[c(1:5)])
  #KeyFeats <- c("age", "asa3","cardiacrisk","RespPreop", "spec2")

  lst2 <-
    sapply(seq_along(KeyFeats), function(j) {
      combn(KeyFeats, j, simplify = FALSE)
    })

  hh <- unlist(lst2, recursive = FALSE) # all combinations

  Stop <- sum(choose(length(KeyFeats), 1:1)) # Just combinations with a minimum of 3 interactions
  hh <- hh[-c(1:Stop)] # Take out less than 3 combinations
  hh <- hh[10:26]

  save(hh, file = "FeatureSelect3.RData")
  save(KeyFeats, file = "FeatureSelect4.RData")

  Variables(hh)

  return(hh)
}


ParamsModeling <- function(hh, data, Bootstraps, SelectedModels) {
  
  method <- 1 #default here

  set.seed(132)

  seed <- vector(mode = "list", length = Bootstraps)

  for (i in 1:Bootstraps) {
    seed[[i]] <- sample(100:1000, 1, replace = FALSE)
  }

  set.seed(132)

  tic()
  FeatureSelectRes <- furrr::future_map(.x = c(1:length(hh)), ~ iter_parallelFinal(.x, data, hh, method, Bootstraps, SelectedModels, seed), future.seed = TRUE)
  
  #p <- progressor(along = xs)
  #FeatureSelectRes <- furrr::future_map(c(1:length(hh)), function(x) ~ iter_parallelFinal(.x, data, hh, method, Bootstraps, SelectedModels, seed), future.seed = TRUE)
  
  toc()

  return(FeatureSelectRes)
}


save(
  list = c("Bootstraps", "SelectedModels","ChosenRun"),
  file = paste0("ExternalParams.RData")
)

## ---------------------Pipeline --------------------------------------------------------------------------------


simple_plan <- drake_plan(
  data1 = target(readxl::read_xlsx(file_in("CovidSurgData.xlsx"))), #######change this
  data2 = target(PreProc2(data1)), # full_data, dataA1Old,dataA1Labs, dataA1ImpLabs
  data3 = target(DataSplit(data2[[1]])), # Validation,data
  PlotsNames = target(FeatureSelection(data3[[2]])),
  hh = target(FeatureSelectionUserInput(PlotsNames)),
  g1 = ParamsModeling(hh, data3[[2]],Bootstraps, SelectedModels),
  Coeffs = target(CoeffsLogit(g1,ChosenRun)), #eq, Coefs2
  AUCPlots = target(AUCPlot(data3[[2]], hh, ChosenRun,data3[[1]], Coeffs[[1]])), #DerivationWPred,ValidationWPred, Plot
  CalibrationPlots = target(CalibrationPipe(AUCPlots[[1]],"Title", "xaxis", "yaxis", AUCPlots[[2]])), #CalibrationVals
  TopModels = target(DrakeBuildResults(g1, hh, SelectedModels,Bootstraps)), # q3,IPP
  Logitplot = target(HeatmapPlots(TopModels[[2]], PlotsNames)),
  MatrixCorr = target(mixed_assoc(
    data2[[3]] %>%
      dplyr::rename(
        "Haemoglobin" = "Haemoglobin (g/dl)",
        "White cell count" = "White cell count (10^9/L)",
        "Preop. Respiratory Support" = "Preoperative respiratory support",
        "Rev. Cardiac Risk Index" = "Revised Cardiac Risk Index"
      ) %>%
      select_if(~ !is.ordered(.))
  )), 
  
  report = (rmarkdown::render(
    knitr_in("CovidSurgReport.Rmd"),
    output_file = file_out("CovidSurgReport.html"),
    quiet = TRUE
  )
  )
  
  )


make(simple_plan) #https://github.com/rstudio/gt/issues/297 - appeared when sourcing functions from report # lock_envir = FALSE

toc()



## ---------------------End --------------------------------------------------------------------------------

## ---------------------Run with finalized results and send to folder --------------------------------------------------------------------------------

#Repeat until happy and then send to folder
toMatch <- c(".csv", ".pdf", ".RData", ".html",".docx",".RDS")
matches <- unique (grep(paste(toMatch,collapse="|"), 
                        list.files("/Users/lxb732/Desktop/ParallelizeCOVIDPipelineNoSlurm/DrakePipeline15Jan"), value=TRUE))

NameRun <- "GitResults"
subDir <- paste0(sub("\\..*", "", NameRun), format(Sys.time(), "_%Y%m%d_%H%M"))
dir.create(file.path(subDir))

file.copy(from= WorkingDir, to= file.path(subDir), 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(file.path(WorkingDir,matches), file.path(subDir))
file.remove(matches)



