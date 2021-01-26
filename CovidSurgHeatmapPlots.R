
HeatmapPlots <- function(IPP, PlotsNames) {
  
  KeyFeats <- c(PlotsNames[[1]]$`Variable Name`[c(1:5)]) %>% 
    str_replace_all(c(
      "sex" = "Sex", 
      "age" = "Age",
      "grade998" =  'Grade of surgery', 
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
      #"Preop" = 'Preoperative', 
      "\\." = "-", 
      "-years" = ""
    )
    ) 

  pp <- list()

  for (i in KeyFeats) {
    print(i)
    pp[[i]] <- IPP %>%
      filter(grepl(i, Names)) %>%
      mutate(Names2 = gsub(as.character(i), "", Names)) %>%
      add_column(b = i)
  }

  pp1 <- pp %>%
    bind_rows() %>%
    mutate_if(is.character, as.factor) %>%
    unite(New, c(Run, Names), sep = ":  ", remove = FALSE) %>%
    filter(., Model == "logit")
  
  if (dim(pp1)[1] != 0){
    print("yei")

  pdf("ModelsPlotFSLogit.pdf", 10, 7)

  print(ggplot(pp1, aes(reorder(New, Mean), b)) +
    geom_tile(aes(fill = Mean), colour = "black", width = 1, height = 0.001) +
    scale_fill_stepsn(colours = topo.colors(20)[c(5, 8, 16)], name = "Mean AUC", n.breaks = 12) +
    facet_grid(NumVars ~ b, scales = "free", space = "free") +
    theme_linedraw() +
    theme(
      axis.text = element_text(size = 5),
      panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey"),
      strip.text = element_text(size = 4, colour = "black"),
      legend.text = element_text(size = 5)
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0), labels = c()) +
    coord_flip() +
    xlab("Model Runs") +
    ylab("Variables")
  

  )

  dev.off()

  LogitPlot <- ggplot(pp1, aes(reorder(New, Mean), b)) +
    geom_tile(aes(fill = Mean), colour = "black", width = 1, height = 0.001) +
    scale_fill_stepsn(colours = topo.colors(20)[c(5, 8, 16)], name = "Mean AUC", n.breaks = 12) +
    facet_grid(NumVars ~ b, scales = "free", space = "free") +
    theme_linedraw() +
    theme(
      axis.text = element_text(size = 5),
      panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey"),
      strip.text = element_text(size = 4, colour = "black"),
      legend.text = element_text(size = 5)
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0), labels = c()) +
    coord_flip() +
    xlab("Model Runs") +
    ylab("Variables")
  
}else{
  
  LogitPlot <- NULL
}
  
  
  

  pp1 <- pp %>%
    bind_rows() %>%
    mutate_if(is.character, as.factor) %>%
    unite(New, c(Run, Names), sep = ":  ", remove = FALSE) %>%
    filter(., Model == "rf")
  
  if (dim(pp1)[1] != 0){
    print("yei")
  

  pdf("ModelsPlotFSRF.pdf", 6, 7)

 print( ggplot(pp1, aes(reorder(New, Mean), b)) +
    geom_tile(aes(fill = Mean), colour = "black", width = 1, height = 0.001) +
    scale_fill_stepsn(colours = topo.colors(20)[c(5, 8, 16)], name = "Mean AUC", n.breaks = 12) +
    facet_grid(NumVars ~ b, scales = "free", space = "free") +
    theme_linedraw() +
    theme(
      axis.text = element_text(size = 5),
      panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey"),
      strip.text = element_text(size = 4, colour = "black"),
      legend.text = element_text(size = 5)
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0), labels = c()) +
    coord_flip() +
    xlab("Model Runs") +
    ylab("Variables")
)
  dev.off()
  }

  pp1 <- pp %>%
    bind_rows() %>%
    mutate_if(is.character, as.factor) %>%
    unite(New, c(Run, Names), sep = ":  ", remove = FALSE) %>%
    filter(., Model == "dt")

  if (dim(pp1)[1] != 0){
    print("yei")
  
  pdf("ModelsPlotFSDT.pdf", 6, 7)

 print( ggplot(pp1, aes(reorder(New, Mean), b)) +
    geom_tile(aes(fill = Mean), colour = "black", width = 1, height = 0.001) +
    scale_fill_stepsn(colours = topo.colors(20)[c(5, 8, 16)], name = "Mean AUC", n.breaks = 12) +
    facet_grid(NumVars ~ b, scales = "free", space = "free") +
    theme_linedraw() +
    theme(
      axis.text = element_text(size = 5),
      panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey"),
      strip.text = element_text(size = 4, colour = "black"),
      legend.text = element_text(size = 5)
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0), labels = c()) +
    coord_flip() +
    xlab("Model Runs") +
    ylab("Variables")
 )

  dev.off()

  }
  
  


  return(LogitPlot)


  ############
}
