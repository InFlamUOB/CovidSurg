
EqsFun <- function(xx, data, m) {
  
  
  xx <- xx %>%
    select((order(colnames(.))))
  # linpred <- cbind(Intercept= 1 , Age = dummy(xx$age), dummy(xx$cardiacrisk)) %*% m$Other
  
  dummy <- list()
  for (i in 1:dim(xx)[2]) {
    dummy[[i]] <- dummy(xx[[i]])
  }
  
  ff <- as.matrix(data.frame(dummy))
  linpred <- cbind(Intercept = 1, ff) %*% m$Other
  
  pi <- exp(linpred) / (1 + exp(linpred))
  h <- data %>%
    add_column(
      G = pi,
      G1 = linpred
    )
  
  return(h)
}



AUCPlot <- function(data, hh, ChosenRun,Validation, eq){

  xx <- data %>%
    select(c(hh[[ChosenRun]]))  #will have matching columns with m 
  
  xx1 <- pivot_longer(xx, everything(), values_to = "Vals")
  xx1["Names"] <- paste(xx1$name, xx1$Vals, sep = "")
  eq4 <- match(row.names(data.frame(eq)), c("(Intercept)", levels(as.factor(xx1$Names))))
  m <- data.frame(Names = c("(Intercept)", levels(as.factor(xx1$Names))), Other = 0)
  m$Other[eq4] <- eq
  
  ROCplot <- function(h, title,ChosenRun) {
    obj <- roc(h$Label, h$G, ci = TRUE, plot = FALSE)
    ciobj <- ci.se(obj, specificities = seq(0, 1, l = 25), method = "bootstrap")
    dat.ci <- data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    )
    
    
    basicplot <- ggplot(h, aes(d = Label, m = G)) +
      geom_roc(n.cuts = 50, labels = FALSE) +
      style_roc(
        ylab = "Sensitivity",
        xlab = "1 - Specificity",
        minor.breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)), guide = TRUE
      )
    
    dat.ci2 <- dat.ci %>%
      mutate(x = 1 - x)
    
    
    Derv <- basicplot +
      ggtitle(paste0(title,"-", ChosenRun)) +
      annotate("text", x = .7, y = .25, label = paste("AUC =", round(calc_auc(basicplot)$AUC, 4)), fontface = "bold") +
      annotate("text", x = .7, y = .15, label = paste(gsub("\\s*\\([^\\)]+\\)", "", capture.output(obj$ci))), fontface = "bold") +
      geom_abline(slope = 1, intercept = 0, color = "grey", linetype = "dashed") +
      geom_ribbon(data = dat.ci2, aes(x = x, ymin = lower, ymax = upper), inherit.aes = FALSE, fill = "steelblue", alpha = 0.2)
    
    return(Derv)
  }
  
  
  
  # Get predicted values for derivation and validation sets and append at the end
  
  DerivationWPred <- EqsFun(xx, data, m)
  
  Derv <- ROCplot(DerivationWPred, "Derivation set AUROC", ChosenRun)
  
  xxV <- Validation %>%
    select(c(hh[[ChosenRun]]))
  
  ValidationWPred <- EqsFun(xxV, Validation, m)
  Vald <- ROCplot(ValidationWPred, "Validation set AUROC",ChosenRun)
  
  
  pdf("AUCCurves.pdf", 10, 5)
  
  grid.arrange(Derv, Vald, ncol = 2)
  
  dev.off()
  
  Plot <- grid.arrange(Derv, Vald, ncol = 2)
  
  save(
    list = c(
      "ValidationWPred",
      "DerivationWPred",
      "ChosenRun",
      "m"
    ),
    file = paste0("Calibration.RData")
  )
  
  return(list(DerivationWPred,ValidationWPred, Plot,m ))
  
}
