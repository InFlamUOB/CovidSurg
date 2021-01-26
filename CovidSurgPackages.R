library(doFuture)
library(tictoc)
library(furrr)
library(dplyr)
library(tidymodels)
library(future.apply)
library(tidyverse)
library(kableExtra)
library(data.table)
require(gridExtra)
require(grid)
require(progressr)
require(naniar)
require(UpSetR)
require(mice)
require(vip)
require(magrittr)
require(rcompanion)
require(corrr)
require(readr)
require(DescTools)
require(dummies)
require(plotROC)
require(pROC)
require(ggbeeswarm)
require(rms)
require(CORElearn)
# library(drake) # will need this package but not load it yet 


if (getNamespaceName(environment(select)) != "dplyr") {
  print("Hey") # Goes into global Environment
  select <- dplyr::select
}

if (getNamespaceName(environment(select)) != "workflow") {
  print("Hey") # Goes into global Environment
  workflow <- workflows::workflow
}

rename <- dplyr::rename

if (getNamespaceName(environment(tune)) != "tune") {
  print("Hey3") # Goes into global Environment
  tune <- tune::tune
}
