
## ----Auxiliary -------------------------------------------------------------------------------


hp.detach <- function(package_name) {
  search_item <- paste("package", package_name, sep = ":")

  while (search_item %in% search()) {
    unload_package <- tryCatch(
      {
        detach(search_item,
          character.only = TRUE,
          unload = TRUE
        )
      },
      error = function(msg) {
        cat(
          paste0(
            "===> ERROR: (unload_package) ",
            match.call()[[1]],
            " (error): ",
            msg,
            "\r\n"
          )
        )
        return(NULL)
      },
      warning = function(msg) {
        cat(
          paste0(
            "===> ERROR: (unload_package) ",
            match.call()[[1]],
            " (warning): ",
            msg,
            "\r\n"
          )
        )
        return(NULL)
      },
      finally = {

      }
    )
  }
}




## ----Data_Distribution -------------------------------------------------------------------------------

Plot1a <- function(FinalData) {
  data2 <- FinalData

  # Label <- data.frame(Label=FinalData$Label)

  FinalData <- FinalData %>%
    dplyr::select(-Label)

  FinalDataNumeric <- FinalData %>%
    select_if(is.numeric) %>%
    add_column(Label = data2$Label)

  FinalDataChar <- FinalData %>%
    select_if(~ !is.numeric(.)) %>%
    mutate_if(~ !is.character(.), as.character) %>%
    add_column(Label = data2$Label)

  Model1aPivot <- FinalDataChar %>%

    pivot_longer(-Label, names_to = "Features", values_to = "Value")

  CharPlots <- Model1aPivot %>%
    group_by(Features) %>%
    dplyr::count(Value, Label, sort = TRUE) %>%
    ungroup() %>%
    mutate(Value = fct_reorder(Value, n, sum)) %>%
    ggplot(aes(n, Value, fill = Label)) +
    geom_col() +
    facet_wrap(~Features, scales = "free") +
    labs(title = "Relationship of Variables and Outcome", x = "Number of Patients", y = "Levels of Feature")


  return(CharPlots)
}

Plot1b <- function(FinalData) {
  data2 <- FinalData

  # Label <- data.frame(Label=FinalData$Label)

  FinalData <- FinalData %>%
    dplyr::select(-Label)

  FinalDataNumeric <- FinalData %>%
    select_if(is.numeric) %>%
    add_column(Label = data2$Label)

  FinalDataChar <- FinalData %>%
    select_if(~ !is.numeric(.)) %>%
    mutate_if(~ !is.character(.), as.character) %>%
    add_column(Label = data2$Label)

  Model2aPivot <- FinalDataNumeric %>%
    pivot_longer(-Label, names_to = "Features", values_to = "Value")

  NumPlots <- Model2aPivot %>%
    ggplot(aes(Value, fill = Label)) +
    geom_density(alpha = .5) +
    facet_wrap(~Features, scales = "free") +
    labs(title = "Relationship of Variables and Outcome", x = "Density", y = "Value")

  return(NumPlots)
}

## ----Feature Selection -------------------------------------------------------------------------------

# Other

## ----Modeling -------------------------------------------------------------------------------

iter_parallelFinal <- function(i, data, hh, method, Bootstraps, SelectedModels, seed) {
  
  message("Run: ", i)
  Preop <- data

  dataFS <- Preop %>%
    dplyr::select(hh[[i]], Label)


  SelectedModels2 <- SelectedModels


  Pipeline <- function(j, dataFS, method, SelectedModels2, seed) {
    set.seed(seed[[j]])
    message("Bootstrap: ", j)
    data <- dataFS

    TrackSplit <- initial_split(data, strata = Label)
    uni_split <- TrackSplit
    uni_train <- training(uni_split)
    uni_test <- testing(uni_split)



    # why no downsample or dummy in previous?? - This is why 0.7 ??

    if (length(select_if(uni_train, is.numeric)) == 0) {
      PreProc <- recipe(Label ~ ., data = uni_train) %>%
        # step_downsample(Label, seed = 132, under_ratio = 2) %>%
        # step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
        # step_interact(~ has_role("Combine"):has_role("Combine")) %>%
        step_nzv(all_predictors()) # %>%
      # step_corr(all_predictors()) %>%
      # step_lincomb(all_predictors())
    } else {
      PreProc <- recipe(Label ~ ., data = uni_train) %>%
        # step_downsample(Label, seed = 132, under_ratio = 2) %>%
        step_normalize(all_numeric()) %>%
        # step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
        # step_interact(~ has_role("Combine"):has_role("Combine")) %>%
        step_nzv(all_predictors()) # %>%
      # step_corr(all_predictors()) %>%
      # step_lincomb(all_predictors())
    }


    PreparedPreProc <- PreProc %>% prep()

    f <- juice(PreparedPreProc)


    logit_tune_pra <- logistic_reg() %>%
      set_engine("glm")

    # Hyperparameter grid
    logit_grid <- logit_tune_pra %>%
      parameters()

    # Workflow bundling every step
    logit_wflow <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(logit_tune_pra)

    # problem with mtry
    rf_tune_pra <- rand_forest(trees = tune(), min_n = tune()) %>%
      set_engine("randomForest") %>%
      set_mode("classification")

    rf_grid <- rf_tune_pra %>%
      parameters() %>%
      grid_max_entropy(size = 5)

    rf_wflow <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(rf_tune_pra)

    dt <-
      decision_tree(
        tree_depth = tune(),
        min_n = tune(),
        cost_complexity = tune()
      ) %>%
      set_engine("rpart") %>%
      set_mode("classification")

    # Hyperparameter grid
    dt_grid <- dt %>%
      parameters() %>%
      grid_max_entropy(size = 20)

    # Workflow bundling every step
    dt_wflow <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(dt)

    bt <- boost_tree(trees = tune(), learn_rate = tune()) %>%
      set_engine("xgboost") %>%
      set_mode("classification")

    bt_grid <- bt %>%
      parameters() %>%
      grid_max_entropy(size = 3)

    # Workflow bundling every step
    bt_wflow <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(bt)


    dtr <-
      parsnip::rand_forest(
        mtry = tune(),
        trees = tune()
      ) %>%
      parsnip::set_engine(
        "ranger",
        regularization.factor = 0.3,
        regularization.usedepth = FALSE
      ) %>%
      set_mode("classification")


    dt_gridr <- grid_regular(
      mtry() %>% range_set(c(1, 15)),
      trees() %>% range_set(c(200, 300)),
      min_n() %>% range_set(c(2, 10)),
      levels = 3
    )

    dt_wflowr <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(dtr)


    metrics <- metric_set(roc_auc) # spec does not work

    mdls <- list(
      "logit" = list(logit_wflow, logit_grid, "logit"),
      "rf" = list(rf_wflow, rf_grid, "rf"),
      "dt" = list(dt_wflow, dt_grid, "dt"),
      "bt" = list(bt_wflow, bt_grid, "bt"),
      "dtR" = list(dt_wflowr, dt_gridr, "dtR")
    )


    mdls <- mdls[c(SelectedModels2)]
    NumModels <- length(mdls)
    wflow_list <- mdls %>% map(c(1))
    grid_list <- mdls %>% map(c(2))
    namesModels <- mdls %>% map(c(3))

    set.seed(132) # new

    folds <- vfold_cv(uni_train, v = 10, strata = Label)

    trained_models_list <- future_map2(
      .x = wflow_list,
      .y = grid_list,
      ~ tune_grid(
        .x,
        resamples = folds,
        grid = .y,
        metrics = metrics
      )
    )

    gg <- trained_models_list %>% map_dfr(function(dat) {
      dat %>%
        collect_metrics()
    })

    best_params <- trained_models_list %>%
      map(select_best, metric = "roc_auc")

    trained_models_list2 <- Map(function(x, y) {
      x$Model <- y
      x
    }, trained_models_list, namesModels)


    FinWork <-
      map2(.x = wflow_list, .y = best_params, ~ finalize_workflow(.x, .y))

    Julia <- map(.x = FinWork, ~ fit(.x, uni_train))

    final_metrics3 <- map(
      .x = Julia,

      function(Wf = .x) {
        # Split the data into analysis and assessment tables

        # Summarise Predictions
        table <-
          tibble(
            num = j,
            truth = as.factor(uni_train$Label),
            .pred_Alive =
              predict(Wf,
                new_data = uni_train,
                type = "prob"
              )[[paste0(".pred_", levels(data$Label)[1])]],
            .pred_Dead =
              predict(Wf,
                new_data = uni_train,
                type = "prob"
              )[[paste0(".pred_", levels(data$Label)[2])]],
            .pred_Class =
              predict(Wf,
                new_data = uni_train
              ) %>%
                unlist() %>%
                as.character()
          ) %>%
          mutate(.pred_Class = factor(.pred_Class))
      }
    )


    final_metrics <-
      map_df(
        .x = FinWork,
        .y = uni_split,
        ~ last_fit(.x, .y, metrics = metrics)
      )


    FinalPred <- final_metrics %>%
      pull(.predictions)

    names(FinalPred) <- namesModels

    FinalPred3 <- bind_rows(FinalPred, .id = "Model")

    h <- names(dataFS)[-which(names(dataFS) %in% "Label")]
    h2 <- names(f)[-which(names(f) %in% "Label")]


    Back <- invisible(
      list(
        best_params,
        final_metrics3,
        data.frame(
          FinalPred3,
          num = j,
          Variables = paste(h, collapse = "+"),
          Variables2 = paste(h2, collapse = "+")
        ),
        Julia
      )
    )
  }



  FurrDat <-
    furrr::future_map(.x = c(1:Bootstraps), ~ (Pipeline(.x, dataFS, method, SelectedModels2, seed)), future.seed = TRUE)

  # print("yes")
  # return(FurrDat)
}


## ----Results -------------------------------------------------------------------------------

TrainTest <- function(a, a1, SelectedModels) {


  # Model coding:- those selected in the modelling had been stored in SelectedModels
  # 1 -logit
  # 2 -rf
  # 3 -dt
  # 4 -bt
  # 5 - dtR

  namesModels <- c("logit", "rf", "dt", "bt", "dtR")

  Basis <- list()

  Basis <- lapply(seq_along(1:length(SelectedModels)), function(i) {
    a %>%
      dplyr::select(starts_with(c(namesModels[SelectedModels][i], "Boost", "Run"))) %>%
      set_colnames(c(
        "num",
        "truth",
        "pred_Alive",
        "pred_Dead",
        "pred_Class",
        "Boost",
        "Run"
      )) %>%
      mutate(Model = namesModels[SelectedModels][i])
  })


  Basis <- rbindlist(Basis)

  hp.detach("readr")
  hp.detach("rfUtilities")
  hp.detach("cutpointr")
  hp.detach("rcompanion")
  multi_metric <- metric_set(roc_auc)
  multi_metric2 <- metric_set(accuracy, sens, spec)

  Basis <- Basis %>%
    mutate_if(is.character, as.factor)

  Basis$pred_Class2 <- factor(Basis$pred_Class, levels = c(levels(Basis$truth)[1], levels(Basis$truth)[2]))

  Output <- bind_rows(
    Basis %>%
      group_by(Run, Model, Boost) %>%
      roc_auc(truth, pred_Alive) %>%
      dplyr::select(-c(Boost, .estimator)) %>%
      group_by(Model, Run, .metric) %>%
      dplyr::summarize(Mean = mean(.estimate), sd = sd(.estimate)) %>%
      arrange(desc(Mean)) %>%
      mutate_if(is.numeric, ~ round(., 3)),

    Basis %>%
      group_by(Run, Model, Boost) %>%
      multi_metric2(truth = truth, estimate = pred_Class2) %>%
      dplyr::select(-c(Boost, .estimator)) %>%
      group_by(Model, Run, .metric) %>%
      dplyr::summarize(Mean = mean(.estimate), sd = sd(.estimate)) %>%
      mutate_if(is.numeric, ~ round(., 3))
  )


  Train <- Output %>%
    ggplot(aes(x = Run, y = Mean, col = Model)) +
    geom_pointrange(aes(ymin = Mean - sd, ymax = Mean + sd)) +
    facet_grid(. ~ .metric) +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    ) +
    ggtitle("Prediction Evaluation Statistics ")


  multi_metric2 <- metric_set(accuracy, sens, spec)
  Name <- names(a1)[2]

  a1 <- a1 %>%
    mutate_if(is.character, as.factor)

  a1$.pred_class <- factor(a1$.pred_clas, levels = c(levels(Basis$truth)[1], levels(Basis$truth)[2]))

  Output2 <- bind_rows(
    a1 %>%
      group_by(Run, Model, Boost) %>%
      roc_auc(Label, paste0(Name)) %>%
      dplyr::select(-c(Boost, .estimator)) %>%
      group_by(Model, Run, .metric) %>%
      dplyr::summarize(Mean = mean(.estimate), sd = sd(.estimate)) %>%
      arrange(desc(Mean)) %>%
      mutate_if(is.numeric, ~ round(., 3)),

    a1 %>%
      group_by(Run, Model, Boost) %>%
      multi_metric2(Label, estimate = .pred_class) %>%
      dplyr::select(-c(Boost, .estimator)) %>%
      group_by(Model, Run, .metric) %>%
      dplyr::summarize(Mean = mean(.estimate), sd = sd(.estimate)) %>%
      mutate_if(is.numeric, ~ round(., 3))
  )


  Test <- Output2 %>% ggplot(aes(x = Run, y = Mean, col = Model)) +
    geom_pointrange(aes(ymin = Mean - sd, ymax = Mean + sd)) +
    facet_grid(. ~ .metric) +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    ) +
    ggtitle("Prediction Evaluation Statistics ")


  MergeOut <- rbind(
    Output %>%
      mutate(Type = "Train"),
    Output2 %>%
      mutate(Type = "Test")
  )

  MergeOut$.metric <- as.factor(MergeOut$.metric)

  MergeOut <- MergeOut %>%
    mutate(ModelFinal = paste0(Model, "+", Type)) %>%
    mutate(
      Metrics = recode(
        .metric,
        "roc_auc" = "AUROC",
        "sens" = "Sensitivity",
        "spec" = "Specificity",
        "accuracy" = "Accuracy"
      )
    )



  return(MergeOut)
}

## ----Plots -------------------------------------------------------------------------------

Tablize <- function(pp) {
  
  ft <- flextable(pp)
  ft <- colformat_num(x = ft, 
                      big.mark=",", digits = 3, na_str = "N/A")
  ft <- autofit(ft) %>%
    # theme_booktabs()
    #theme_tron()
    # theme_vanilla()
    theme_zebra( odd_header = "transparent",
                 odd_body = "rosybrown2",
                 even_header = "blue",
                 even_body = "transparent") %>%
    align(align = "center", part = "all") %>%
    border(border.top = fp_border(color = "black"), border.bottom = fp_border(color = "black"), part = "all" )
  
  ft
}





MortalityTable <- function( Probs, ValidationWPred,hh) {
  
  CaseFatalValMort <- ValidationWPred %>%
    mutate(z = cut(G, round(Probs, 2), include.lowest = TRUE)) %>%
    group_by(z) %>%
    mutate(CaseFatality = length(Label[Label == "Dead"]) / (length(Label[Label == "Dead"]) + length(Label[Label == "Alive"]))) %>%
    select(hh[[ChosenRun]], z, CaseFatality,G, Label) %>%
    mutate(Risk = paste0(z, " & ", round(CaseFatality, 3)))
  
  #Probs <- round(seq(0, max(ValidationWPred$G) + 0.1, 0.1), 2)
  
  Probs <- Probs[-c(1,length(Probs))]
  
  MortalityTab <- list()
  
  Fin <- CaseFatalValMort
  
  #Probs1 <- Probs[-c(1)]
  
  for (i in seq_along(Probs)) {
    print(i)
    
    Cutoff <- Probs[i]
    
    Fin <- Fin %>%
      mutate(Var2 = case_when(G <= Cutoff ~ "AlivePred", TRUE ~ "DeadPred"))
    
    MortalityTab[[i]] <- data.frame(table(Fin$z, Fin$Label, Fin$Var2)) %>%
      group_by(Var2, Var3) %>%
      mutate(Sum = sum(Freq)) %>%
      select(-c(Var1, Freq)) %>%
      unique() %>%
      mutate(Variables = case_when(
        Var2 == "Alive" & Var3 == "AlivePred" ~ "TN",
        Var2 == "Alive" & Var3 == "DeadPred" ~ "FP",
        Var2 == "Dead" & Var3 == "AlivePred" ~ "FN",
        TRUE ~ "TP"
      )) %>%
      ungroup() %>%
      select(c(Variables, Sum)) %>%
      pivot_wider(names_from = Variables, values_from = Sum) %>%
      mutate(
        Cutoff = paste0("<=", Cutoff),
        NumPats = data.frame(table(Fin$Var2)) %>% filter(Var1 == "AlivePred") %>% select(Freq) %>% as.numeric(),
        Sensitivity = TP / (TP + FN) * 100,
        Specificity = TN / (TN + FP) * 100,
        PPV = TP / (TP + FP) * 100,
        NPV = TN / (TN + FN) * 100,
        Mortality = FN / NumPats * 100
      )
  }
  
  MortalityTab5 <- bind_rows(MortalityTab)
  
  MortalityTab <- list()
  
  #Probs2 <- Probs[-c(1,length(Probs))]
  
  for (i in seq_along(Probs)) {
    print(i)
    
    Cutoff <- Probs[i]
    
    # Fin["Numbers"] <- cut(Fin$NormProb, breaks= c( 0, 0.2, 0.4, 0.6, 0.8, 1), include.lowest = T , labels = c(0.2, 0.4, 0.6, 0.8, 1))
    # Fin$Numbers <- as.numeric(as.character(Fin$Numbers))
    Fin <- Fin %>%
      mutate(Var2 = case_when(G >= Cutoff ~ "DeadPred", TRUE ~ "AlivePred"))
    
    MortalityTab[[i]] <- data.frame(table(Fin$z, Fin$Label, Fin$Var2)) %>%
      group_by(Var2, Var3) %>%
      mutate(Sum = sum(Freq)) %>%
      select(-c(Var1, Freq)) %>%
      unique() %>%
      mutate(Variables = case_when(
        Var2 == "Alive" & Var3 == "AlivePred" ~ "TN",
        Var2 == "Alive" & Var3 == "DeadPred" ~ "FP",
        Var2 == "Dead" & Var3 == "AlivePred" ~ "FN",
        TRUE ~ "TP"
      )) %>%
      ungroup() %>%
      select(c(Variables, Sum)) %>%
      pivot_wider(names_from = Variables, values_from = Sum) %>%
      mutate(
        Cutoff = paste0(">=", Cutoff),
        NumPats = data.frame(table(Fin$Var2)) %>% filter(Var1 == "DeadPred") %>% select(Freq) %>% as.numeric(),
        Sensitivity = TP / (TP + FN) * 100,
        Specificity = TN / (TN + FP) * 100,
        PPV = TP / (TP + FP) * 100,
        NPV = TN / (TN + FN) * 100,
        Mortality = TP / NumPats * 100
      )
  }
  
  MortalityTab6 <- bind_rows(MortalityTab)
  
  qq <- rbind(MortalityTab5, MortalityTab6) %>%
    #filter(Cutoff %in% c("<=0.2", "<=0.4", "<=0.6", ">=0.6", ">=0.8")) %>%
    mutate(Percentage = round(NumPats / dim(ValidationWPred)[1] * 100, 2)) %>% #before dataA1?
    mutate(NumPer = paste0(NumPats, " (", Percentage, ")")) %>%
    select(-c(Percentage, NumPats)) %>%
    relocate(Cutoff, NumPer, TP, TN, FP, FN, Sensitivity, Specificity, PPV, NPV, Mortality) %>%
    dplyr::rename('Cutoff Risk' = Cutoff, 
                  'No of patients (%)' = NumPer, 
                  'Sensitivity (%)' = Sensitivity, 
                  'Specificity (%)' = Specificity, 
                  'PPV (%)' = PPV, 
                  'NPV (%)' = NPV, 
                  'Mortality (%)' = Mortality) 
  
  return(qq)
  
}
