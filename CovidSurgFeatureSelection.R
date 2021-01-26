
PlotsFS <- function(FinWorkFeatures, rankFilt,best_params) {
  # Get feature importance LASSO

  s <- FinWorkFeatures[[1]] %>%
    vi(lambda = best_params[[1]]$penalty) %>%
    mutate(
      Importance = abs(Importance),
      Variable = fct_reorder(Variable, Importance)
    )

  s <- s %>%
    mutate(rank = dense_rank(desc(Importance))) %>%
    mutate(Model = "LASSO") %>%
    select(Sign, Variable, Importance, rank, Model)

  p <- s

  p$Variable <- p$Variable %>%
    str_replace_all(c(
      "X" = "",
      # ":" = "",
      "_x_" = " & ",
      "_" = ":",
      "Abdominal" = "Abdominal surgery",
      "Head and neck" = "Head and neck surgery",
      "Limb" = "Orthopaedic surgery",
      "obstetric" = "Obstetrics",
      "other" = "Other surgery",
      "Thorax" = "Cardiothoracic surgery",
      "none" = "None",
      "oxygen" = "Supplemental oxygen",
      "ventilated" = "Ventilation",
      "Postop" = "Postoperative",
      "Benign/Obs" = "Benign",
      "major" = "Major",
      "minor" = "Minor",
      "grade998" = "Grade of surgery",
      "spec2" = "Speciality",
      "hb" = "Haemoglobin (g/dl)",
      "wcc" = "White cell count (10^9/L)",
      "RespPreop" = "Preoperative respiratory support",
      "indication" = "Indication for surgery",
      "urgency4" = "Urgency of surgery",
      "time" = "Timing of SARS-CoV-2 diagnosis",
      "respdisease" = "Respiratory comorbidity",
      "cardiacrisk" = "Revised Cardiac Risk Index",
      "asa3" = "ASA grade",
      # "Preop" = 'Preoperative',
      "\\." = "-",
      "-years" = "",
      ":-" = ":<",
      "0-" = "0>"
    )) %>%
    str_replace_all(c(
      "sex" = "Sex", 
      "age" = "Age",
      "\\." = "-", 
      "-years" = "",
      "90>" = "90+", 
      ">" = "-"
    )
    ) 



  PlotLASSO <- p %>%
    mutate(rank = dense_rank(desc(Importance))) %>%
    filter(rank < 20) %>%
    ggplot(aes(x = Importance, y = reorder(Variable, Importance), fill = Sign)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0)) +
    labs(subtitle = "Glmnet: LASSO and Interactions") +
    xlab("Importance") +
    ylab("Variable") +
    theme_light() +
    scale_fill_manual(
      name = "Sign",
      values = c("springgreen3", "red3"),
      labels = c("Low Risk", "High Risk")
    ) +
    theme(text = element_text(size = 8))

  # Lasso 2

  s2 <- FinWorkFeatures[[3]] %>%
    vi(lambda = best_params[[3]]$penalty) %>%
    mutate(
      Importance = abs(Importance),
      Variable = fct_reorder(Variable, Importance)
    )

  s2 <- s2 %>%
    mutate(rank = dense_rank(desc(Importance))) %>%
    mutate(Model = "LASSOEN") %>%
    select(Sign, Variable, Importance, rank, Model)

  p2 <- s2

  p2$Variable <- p2$Variable %>%
    str_replace_all(c(
      "X" = "",
      # ":" = "",
      "_x_" = " & ",
      "_" = ":",
      "Abdominal" = "Abdominal surgery",
      "Head and neck" = "Head and neck surgery",
      "Limb" = "Orthopaedic surgery",
      "obstetric" = "Obstetrics",
      "other" = "Other surgery",
      "Thorax" = "Cardiothoracic surgery",
      "none" = "None",
      "oxygen" = "Supplemental oxygen",
      "ventilated" = "Ventilation",
      "Postop" = "Postoperative",
      "Benign/Obs" = "Benign",
      "major" = "Major",
      "minor" = "Minor",
      "grade998" =  "Grade of surgery",
      "spec2" = "Speciality",
      "hb" = "Haemoglobin (g/dl)",
      "wcc" = "White cell count (10^9/L)",
      "RespPreop" = "Preoperative respiratory support",
      "indication" = "Indication for surgery",
      "urgency4" = "Urgency of surgery",
      "time" = "Timing of SARS-CoV-2 diagnosis",
      "respdisease" = "Respiratory comorbidity",
      "cardiacrisk" = "Revised Cardiac Risk Index",
      "asa3" = "ASA grade",
      # "Preop" = 'Preoperative',
      "\\." = "-",
      "-years" = "",
      ":-" = ":<",
      "0-" = "0>"
    )) %>% 
    str_replace_all(c(
      "sex" = "Sex", 
      "age" = "Age",
      "\\." = "-", 
      "-years" = "",
      "90>" = "90+", 
      ">" = "-"
    )
    ) %>%
    as.factor()

  PlotLASSO2 <- p2 %>%
    mutate(rank = dense_rank(desc((Importance)))) %>%
    filter(rank < 20) %>%
    ggplot(aes(x = Importance, y = reorder(Variable, Importance), fill = Sign)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0)) +
    labs(subtitle = "Glmnet: EN") +
    xlab("Importance") +
    ylab("Variable") +
    theme_light() +
    scale_fill_manual(
      name = "Sign",
      values = c("springgreen3", "red3"),
      labels = c("Low Risk", "High Risk")
    ) +
    theme(text = element_text(size = 8))


  # Get feature importance RF

  qF <- FinWorkFeatures[[2]]$fit$variable.importance %>%
    enframe("Variable", "Importance")

  pqF <- qF

  pqF$Variable <- pqF$Variable %>%
    str_replace_all(c(
      "X" = "",
      # ":" = "",
      "_x_" = " & ",
      "_" = ":",
      "Abdominal" = "Abdominal surgery",
      "Head and neck" = "Head and neck surgery",
      "Limb" = "Orthopaedic surgery",
      "obstetric" = "Obstetrics",
      "other" = "Other surgery",
      "Thorax" = "Cardiothoracic surgery",
      "none" = "None",
      "oxygen" = "Supplemental oxygen",
      "ventilated" = "Ventilation",
      "Postop" = "Postoperative",
      "Benign/Obs" = "Benign",
      "major" = "Major",
      "minor" = "Minor",
      "grade998" =  "Grade of surgery",
      "spec2" = "Speciality",
      "hb" = "Haemoglobin (g/dl)",
      "wcc" = "White cell count (10^9/L)",
      "RespPreop" = "Preoperative respiratory support",
      "indication" = "Indication for surgery",
      "urgency4" = "Urgency of surgery",
      "time" = "Timing of SARS-CoV-2 diagnosis",
      "respdisease" = "Respiratory comorbidity",
      "cardiacrisk" = "Revised Cardiac Risk Index",
      "asa3" = "ASA grade",
      # "Preop" = 'Preoperative',
      "\\." = "-",
      "-years" = "",
      ":-" = ":<",
      "0-" = "0>"
    )) %>% 
    str_replace_all(c(
      "sex" = "Sex", 
      "age" = "Age",
      "\\." = "-", 
      "-years" = "",
      "90>" = "90+", 
      ">" = "-"
    )
    )

  PlotRF <- pqF %>%
    mutate(rank = dense_rank(desc(Importance))) %>%
    filter(rank < 20) %>%
    ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_d(end = .7) +
    labs(subtitle = "Random Forest: Ranger") +
    xlab("Variables") +
    ylab("Importance") +
    theme_light()

  qF <- qF %>%
    mutate(rank = dense_rank(desc(Importance))) %>%
    mutate(Model = "RF")

  # Merge both LASSO and RF

  FinalFS1 <- rbind(s %>% select(-Sign), qF)
  FinalFS1 <- rbind(s2 %>% select(-Sign), FinalFS1)
  FinalFS <- FinalFS1 %>%
    filter(rank <= rankFilt) %>%
    mutate(rankMod = paste0(rank, "-", Model))


  FinalFS2 <- FinalFS %>%
    separate(Variable, c("first", "second", "third", "fourth"), sep = "_") %>%
    select(c(1, 4, 6, 7, 8))

  # FinalFS2 <- FinalFS2 %>%
  #  filter(Model != "LASSO")

  FinalFS2 <- rbind(FinalFS2[, c(1, 3, 4, 5)], data.frame(first = FinalFS2$fourth, rank = FinalFS2$rank, Model = FinalFS2$Model, rankMod = FinalFS2$rankMod)) %>%
    na.omit() %>%
    add_count(first) %>%
    filter(Model != "LASSO") %>%
    arrange(desc(n), rank) %>%
    rename("Variable Name" = first) %>%
    rename("Freq of appearance" = n) %>%
    rename("Top rank in model" = rankMod) %>%
    select(-c(rank, Model))

  FinalFS2 <- FinalFS2[!duplicated(FinalFS2$"Variable Name"), ]


  return(list(FinalFS2, PlotRF, PlotLASSO, PlotLASSO2))
}


FeatureSelection <- function(data) {

  ################# PreProcessing



  PreProcL <- recipe(Label ~ ., data = data) %>%
    # step_downsample(Label, seed = 132, under_ratio = 2) %>%
    step_normalize(all_numeric()) %>%
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
    step_interact(terms = ~ all_predictors() * all_predictors()) %>%
    step_nzv(all_predictors()) %>%
    step_corr(all_predictors()) %>%
    step_lincomb(all_predictors())


  PreProcL2 <- recipe(Label ~ ., data = data) %>%
    # step_downsample(Label, seed = 132, under_ratio = 2) %>%
    step_normalize(all_numeric()) %>%
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
    # step_interact(terms = ~ all_predictors() * all_predictors()) %>%
    step_nzv(all_predictors()) # %>%
  # step_corr(all_predictors()) %>%
  # step_lincomb(all_predictors())



  PreProcRF <- recipe(Label ~ ., data = data) %>%
    step_downsample(Label, seed = 132, under_ratio = 2) %>%
    step_normalize(all_numeric()) %>%
    # step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
    # step_interact(terms = ~ all_predictors() * all_predictors()) %>%
    step_nzv(all_predictors()) # %>%
  # step_corr(all_predictors()) %>%
  # step_lincomb(all_predictors())




  ################# Modeling and parameter setting

  set.seed(132)
  office_bootLASSO <- bootstraps(data, strata = Label, times = 10)
  set.seed(132)
  office_bootRF <- bootstraps(data, strata = Label, times = 5)

  tune_spec <-
    logistic_reg(
      mode = "classification",
      penalty = tune(),
      mixture = 1
    ) %>%
    set_engine("glmnet")

  tune_spec2 <-
    logistic_reg(
      mode = "classification",
      penalty = tune(),
      mixture = 0.5
    ) %>%
    set_engine("glmnet")

  lambda_grid <- grid_regular(penalty(), levels = 30)

  lasso_wflow <- workflow() %>%
    add_recipe(PreProcL) %>%
    add_model(tune_spec)

  lasso_wflow2 <- workflow() %>%
    add_recipe(PreProcL2) %>%
    add_model(tune_spec2)


  RF_tune_spec <-
    rand_forest(
      mode = "classification",
      trees = tune(),
      min_n = tune()
    ) %>%
    set_engine("ranger", importance = "impurity")
  # set_engine("randomForest", importance = "")


  rf_workflow <- workflow() %>%
    add_recipe(PreProcRF) %>%
    add_model(RF_tune_spec)

  set.seed(132)
  rf_grid <- grid_regular(parameters(RF_tune_spec), levels = 3)


  wf <- list(lasso_wflow, rf_workflow, lasso_wflow2)
  grid <- list(lambda_grid, rf_grid, lambda_grid)
  boost <- list(office_bootLASSO, office_bootRF, office_bootLASSO)

  metrics <- metric_set(roc_auc)

  ################# Parallelization function


  iter_parallel2 <- function(i) {
    print(i)
    set.seed(132)
    Dome <- tune_grid(
      wf[[i]],
      resamples = boost[[i]],
      grid = grid[[i]],
      metrics = metrics
    )

    print("tune done")

    return(Dome)
  }


  tic()
  #set.seed(132)
  jobid <- future_lapply(
    seq_along(1:3),
    function(i) {
      iter_parallel2(i)
    },
    future.seed = TRUE
  )
  toc()



  RFLASSO <- jobid
 # save(RFLASSO, paste0("RFLASSO.RDS")) # time was still included here - suspicious
  
  best_params <- RFLASSO %>%
    purrr::map(tune::select_best, metric = "roc_auc") #  maximize = FALSE
  
  print("best_params")
  
  save(
    list = c("RFLASSO",  "wf"),
    file = paste0("RFLASSOW.RData")
  )
  

  FinWork <- purrr::map2(.x = wf, .y = best_params, ~ finalize_workflow(.x, .y))

  print("FinWork")
  FinWorkFeatures <- purrr::map(.x = FinWork, ~ pull_workflow_fit(fit(.x, data)))
  print("FinWorkFeatures")
  rankFilt <- 7 ###################

  Plots <- PlotsFS(FinWorkFeatures, rankFilt,best_params) # rank 5
  # took out LASSO EN - if in cardiacrisk comes out as important.
  print("Plots")
  
  PlotsNames <- Plots

  pdf("FeatureSelection.pdf", 12, 5)


  grid.arrange(
    Plots[[2]] +
      theme(text = element_text(size = 9)),
    Plots[[4]] +
      theme(text = element_text(size = 10)),
    # Plots[[3]] +
    #  theme(text = element_text(size=10)),
    # ncol = 3,
    ncol = 2,
    top = "Variable Importance",
    # widths = c(0.7, 1,0.8)
    widths = c(1, 1)
  )


  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = 0.5)),
    colhead = list(fg_params = list(cex = 1.0)),
    rowhead = list(fg_params = list(cex = 1.0))
  )

  Plots[[1]]$`Variable Name` <- Plots[[1]]$`Variable Name` %>%
    str_replace_all(c(
      "sex" = "Sex",
      "age" = "Age",
      "grade998" =  "Grade of surgery",
      "spec2" = "Speciality",
      "hb" = "Haemoglobin (g/dl)",
      "wcc" = "White cell count (10^9/L)",
      "RespPreop" = "Preoperative respiratory support",
      "indication" = "Indication for surgery",
      "urgency4" = "Urgency of surgery",
      "time" = "Timing of SARS-CoV-2 diagnosis",
      "respdisease" = "Respiratory comorbidity",
      "cardiacrisk" = "Revised Cardiac Risk Index",
      "asa3" = "ASA grade",
      # "Preop" = 'Preoperative',
      "\\." = "-",
      "-years" = ""
    ))


  grid.arrange(tableGrob(Plots[[1]], rows = NULL, theme = mytheme))

  write.csv(Plots[[1]], "FeatureSelectionTable.csv")

  # Plots[[1]]
  # kable() %>%
  # kable_styling("striped", position = "float_left", font_size = 12, full_width = FALSE) %>%
  # as_image()

  dev.off()

  save(Plots, file = "FeatureSelect1.RData")


  return(PlotsNames)
}
