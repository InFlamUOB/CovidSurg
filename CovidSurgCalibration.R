Plot1Cal1 <- function(DerivationWPred, Title) {
  
  pred_var <- "G"
  model_name <- Title
  
  # The calibration plot
  g1 <- mutate(DerivationWPred, bin = ntile(get(pred_var), 10)) %>%
    # Bin prediction into 10ths
    group_by(bin) %>%
    mutate(
      n = n(), # Get ests and CIs
      bin_pred = mean(get(pred_var)),
      bin_prob = mean(as.numeric(as.factor(Label)) - 1),
      se = sqrt((bin_prob * (1 - bin_prob)) / n),
      ul = bin_prob + 1.96 * se,
      ll = bin_prob - 1.96 * se
    ) %>%
    ungroup() %>%
    ggplot(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
    geom_pointrange(size = 0.5, color = "black") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_abline() + # 45 degree line indicating perfect calibration
    geom_smooth(
      method = "lm", se = FALSE, linetype = "dashed",
      color = "black", formula = y ~ -1 + x
    ) +
    # straight line fit through estimates
    geom_smooth(aes(x = get(pred_var), y = as.numeric(as.factor(Label)) - 1),
                color = "red", se = FALSE, method = "loess"
    ) +
    # loess fit through estimates
    xlab("") +
    ylab("Observed Probability") +
    theme_minimal() +
    ggtitle(model_name)
  
  # The distribution plot
  g2 <- ggplot(DerivationWPred, aes(x = get(pred_var))) +
    geom_histogram(fill = "black", bins = 50) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    xlab("Predicted Probability") +
    ylab("") +
    theme_minimal() +
    scale_y_continuous(breaks = c(0, 40)) +
    theme(panel.grid.minor = element_blank())
  
  # Combine them
  g <- arrangeGrob(g1, g2, respect = TRUE, heights = c(1, 0.25), ncol = 1)
  grid.newpage()
  grid.draw(g)
  return(g[[3]])
}


Plot1Cal <- function(DerivationWPred, Title) {
  
  pred_var <- "G"
  model_name <- Title
  
  # The calibration plot
  g1 <- mutate(DerivationWPred, bin = ntile(get(pred_var), 10)) %>%
    # Bin prediction into 10ths
    group_by(bin) %>%
    mutate(
      n = n(), # Get ests and CIs
      bin_pred = mean(get(pred_var)),
      bin_prob = mean(as.numeric(as.factor(Label)) - 1),
      se = sqrt((bin_prob * (1 - bin_prob)) / n),
      ul = bin_prob + 1.96 * se,
      ll = bin_prob - 1.96 * se
    ) %>%
    ungroup() %>%
    ggplot(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
    geom_pointrange(size = 0.5, color = "black") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_abline() + # 45 degree line indicating perfect calibration
    geom_smooth(
      method = "lm", se = FALSE, linetype = "dashed",
      color = "red", formula = y ~ -1 + x
    ) +
    # straight line fit through estimates
    # geom_smooth(aes(x = get(pred_var), y = as.numeric(as.factor(Label)) - 1),
    #            color = "red", se = FALSE, method = "loess"
    #) +
    # loess fit through estimates
    xlab("") +
    ylab("Observed Probability") +
    theme_minimal() +
    ggtitle(model_name) + 
    theme(text = element_text(size=14))
  
  # The distribution plot
  g2 <- ggplot(DerivationWPred, aes(x = get(pred_var))) +
    geom_histogram(fill = "black", bins = 50) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    xlab("Predicted Probability") +
    ylab("") +
    theme_minimal() +
    scale_y_continuous(breaks = c(0, 200)) +
    theme(panel.grid.minor = element_blank()) + 
    theme(text = element_text(size=14))
  
  # Combine them
  
  return(list(g1, g2))
}

Plot2Hist <- function(DerivationWPred, xaxis) {
  xaxis <- xaxis
  
  
  g1 <- ggplot(DerivationWPred, aes(x = G1, fill = Label)) +
    geom_histogram(bins = 15) +
    theme_minimal() +
    xlab(xaxis) +
    ylab("Number of Participants") +
    scale_fill_brewer("Label", palette = "Paired")
  
  g2 <- ggplot(DerivationWPred, aes(x = G1, fill = Label)) +
    geom_histogram(position = "fill", bins = 10) +
    theme_minimal() +
    xlab(xaxis) +
    ylab("Proportion") +
    scale_fill_brewer("Label", palette = "Paired")
  
  return(grid.arrange(g1, g2, ncol = 1))
}

Plot3Bee <- function(DerivationWPred, yaxis) {
  yaxis <- yaxis
  ggplot(DerivationWPred, aes(y = G, x = Label, fill = Label, color = Label)) +
    geom_beeswarm() +
    # geom_jitter() +
    geom_boxplot(alpha = 0, color = "black") +
    theme_minimal() +
    ylab(yaxis) +
    xlab("Mortality") +
    scale_fill_brewer(guide = FALSE, palette = "Paired") +
    scale_color_brewer(guide = FALSE, palette = "Paired")
}



CalibrationPipe <- function(DerivationWPred,Title, xaxis, yaxis, ValidationWPred){


pdf("DerivationCalibration.pdf", 10, 8)
Plot1Cal1(DerivationWPred, "Calibration in Derivation Set")
Plot2Hist(DerivationWPred, "Probability")
Plot3Bee(DerivationWPred, "Probability")
dev.off()

pdf("ValidationCalibration.pdf", 10, 8)
Plot1Cal1(ValidationWPred, "Calibration in Validation Set")
Plot2Hist(ValidationWPred, "Probability")
Plot3Bee(ValidationWPred, "Probability")
dev.off()
## ----Calibration--------------------------------------------------------------------------------------------

set.seed(132)


CalibrationParams <- function(ValidationWPred) {
  val_m1 <- val.prob(ValidationWPred$G, as.numeric(as.factor(ValidationWPred$Label)) - 1,
                     pl = FALSE
  ) %>% round(3)
  return(val_m1)
}

CalibrationVals <- rbind(Validation = CalibrationParams(ValidationWPred), Derivation = CalibrationParams(DerivationWPred))


#pdf("ParamsCalibration.pdf", 7, 5)
#
#mytheme <- gridExtra::ttheme_default(
#  core = list(fg_params = list(cex = 0.5)),
#  colhead = list(fg_params = list(cex = 1.0)),
#  rowhead = list(fg_params = list(cex = 1.0))
#)
#
#grid.arrange(tableGrob(CalibrationVals, rows = NULL, theme = mytheme))
#
#dev.off()



return(CalibrationVals)

}
