require(doFuture)
require(ggplot2)
require(tictoc)
require(furrr)
require(dplyr)
require(tidymodels)
require(future.apply)
require(tidyverse)
require(kableExtra)
require(data.table)
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

########################
#required for .Rmd plots

#install.packages("flextable")
require(flextable)
require(knitr)
#install.packages("cowplot")
require(cowplot)
#install.packages("webshot")
#install.packages("magick")
require(webshot)
require(magick)
require(officer)
require(magrittr)
#library(gtsummary); 
require(gt)
require(forcats)
require(compareGroups)
require(officer)
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
