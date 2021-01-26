Variables <- function(hh) {

  # Chosen models to run
  count <- list()
  for (i in 1:length(hh)) {
    count[[i]] <- length(hh[[i]])
  }

  Variables <-
    data.frame(
      Names = (
        gsub("^c\\(|\\)$", "", unlist(
          as.character(
            hh
          )
      ))),
      NumVars = t(data.frame(count)),
      Run = c(1:length(hh))
    ) %>%
    remove_rownames()

  save(Variables, file = "FeatureSelect2.RData")

  pdf("VariablesNames.pdf", 10, 25)

  write.csv(Variables, "Variables.csv")

  Variables %>%
    kable() %>%
    kable_styling("striped", position = "float_left", font_size = 12, full_width = FALSE)


  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = 0.5)),
    colhead = list(fg_params = list(cex = 1.0)),
    rowhead = list(fg_params = list(cex = 1.0))
  )

  grid.arrange(tableGrob(Variables, rows = NULL, theme = mytheme))

  dev.off()
}
