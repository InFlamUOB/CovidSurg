
DrakeBuildResults <- function(g1, hh, SelectedModels,Bootstraps) {
 
   BestParams <- lapply(seq_along(1:length(g1)), function(i) {
    lapply(
      seq_along(1:length(g1[[1]])),
      function(j) {
        data.frame(g1[[i]][[j]][[1]], Boost = j, Run = i)
      }
    )
  }) %>%
    bind_rows()

  fwrite(BestParams, "BestParams.csv")

  TrainingMods <- lapply(seq_along(1:length(g1)), function(i) {
    print(paste0("i", i))
    lapply(
      seq_along(1:length(g1[[1]])),
      function(j) {
        print(paste0("j", j))
        data.frame(g1[[i]][[j]][[2]], Boost = j, Run = i)
      }
    )
  })

  m <- do.call("rbind", TrainingMods)
  a <- rbindlist(m)


  fwrite(a, "TrainingMods.csv")

  FinalFinal <- lapply(seq_along(1:length(g1)), function(i) {
    lapply(
      seq_along(1:length(g1[[1]])),
      function(j) {
        data.frame(g1[[i]][[j]][[3]], Boost = j, Run = i)
      }
    )
  })


  m1 <- do.call("rbind", FinalFinal)
  a1 <- rbindlist(m1)

  fwrite(a1, "TestingMods.csv")


  ##############

  # logit has no hyperparameters
  if (length(intersect(SelectedModels, c(2, 3))) != 0) {
    print("hey")
    # BestParams <- fread("BestParams.csv")
    BestParams <- fread("BestParams.csv")
    Best <-
      pivot_longer(
        select_if(BestParams, is.numeric),
        -c(Boost, Run),
        names_to = "Params",
        values_to = "vals"
      )

    pdf("SFig2_Hyperparameters.pdf", 15, 9)

    print(Best %>%
      ggplot(aes(x = as.factor(Run), y = vals, fill = Params)) +
      geom_boxplot() +
      coord_flip() +
      facet_grid(. ~ Params, scales = "free") +
      labs(title = paste0("Bootstraps n=", Bootstraps)) +
      theme(text = element_text(size = 9)) +
      theme(
        plot.title = element_text(size = 11, face = "bold"),
        strip.text.x = element_text(size = 10, color = "black", face = "bold"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11)
      ) +
      labs(
        colour = "Hyperparameters",
        x = "Predictor Combination",
        y = "Value",
        fill = "Hyperparameters"
      ) +
      theme_bw()
)
    dev.off()
  } else {
    print("no hype")
  }
  ## -----------------------------------------------------------------------------------------------------

  # a <- fread("TrainingMods.csv")
  a <- fread("TrainingMods.csv")
  # a1 <- fread("TestingMods.csv")
  a1 <- fread("TestingMods.csv")


  MergeOut <- TrainTest(a, a1, SelectedModels)

  save(MergeOut, file = "TrainingTest.RData")

  Size <- 12

  pdf("TrainingTestingPlot.pdf", 20, 10)

  print(MergeOut %>%
    # filter(.metric == "roc_auc") %>%
    ggplot(aes(
      x = Run,
      y = Mean,
      col = Model,
      shape = Type
    )) +
    geom_pointrange(aes(ymin = Mean - sd, ymax = Mean + sd),
      position = position_dodge(width = 0.8),
      size = 0.4
    ) +
    facet_grid(Metrics ~ ., scales = "free") +
    scale_y_continuous(
      labels = scales::percent,
      breaks = seq(0, 1, 0.1),
      expand = c(0, 0)
    ) +
    scale_x_continuous(breaks = seq(0, max(MergeOut$Run) + 2, 2), expand = c(0, 0)) +
    labs(title = paste0("Bootstraps n=", Bootstraps)) +
    theme(text = element_text(size = Size)) +
    theme(
      plot.title = element_text(size = Size, face = "bold"),
      # legend.title = element_text(Size),
      legend.text = element_text(size = Size),
      strip.text = element_text(size = Size),
      panel.spacing = unit(3, "lines")
    )
)
  dev.off()


  q3 <- MergeOut %>%
    filter(Type == "Test", Metrics == "AUROC") %>%
    select(-c(.metric, Type, ModelFinal)) %>%
    arrange(desc(Mean)) %>%
    mutate_if(is.numeric, ~ round(., 3))


  # Add model names

  hh1 <- hh
  count <- list()
  for (i in 1:length(hh1)) {
    count[[i]] <- length(hh1[[i]])
  }

  Variables <-
    data.frame(
      Names = (
        gsub("^c\\(|\\)$", "", unlist(
          as.character(
            hh1
          )
      ))),
      NumVars = t(data.frame(count)),
      Run = c(1:length(hh1))
    ) %>%
    remove_rownames()
  
  
  Variables$Names <- Variables$Names %>%
    str_replace_all(
      c("\"" = "", 
        c("\"" = ""), 
        c("age" = "Age"), 
        c("asa3" = "ASA grade"), 
        c("cardiacrisk" = "Revised Cardiac Risk Index"), 
        c("RespPreop" = "Preoperative respiratory support"), 
        c("spec2" = "Speciality")
        
      )
    ) 

  IPP <- inner_join(q3, Variables)

  pp <- IPP %>%
    head(20)

  pdf("TableTop20Results.pdf", 15, 12)

  grid.arrange(tableGrob(pp))
  write.csv(pp, "Top20Models.csv")

  dev.off()

  IPP$NumVars <- as.factor(IPP$NumVars)


  q3ANum <- tryCatch(
    {
      IPP %>%
        group_by(NumVars) %>%
        arrange(desc(Mean), .by_group = TRUE) #%>%
        #top_n(n = 5, Mean)
    },
    warning = function(w) {
      print("here3")
    },
    error = function(e) {
      IPP %>%
        group_by(NumVars) %>%
        arrange(desc(Mean)) #%>%
        #top_n(n = 5, Mean)
    },
    finally = {
      print("here2")
    }
  )


  q3AMods <- tryCatch(
    {
      IPP %>%
        group_by(Model) %>%
        arrange(desc(Mean), .by_group = TRUE) #%>%
        #top_n(n = 5, Mean)
    },
    warning = function(w) {
      print("here3")
    },
    error = function(e) {
      IPP %>%
        group_by(Model) %>%
        arrange(desc(Mean)) #%>%
        #top_n(n = 5, Mean)
    },
    finally = {
      print("here2")
    }
  )


  write.csv(q3ANum, "TableModelsNumVars.csv")
  write.csv(q3AMods, "TableModelsNumMods.csv")



  return(list(q3, IPP))
}
