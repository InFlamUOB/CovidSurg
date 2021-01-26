

CoeffsLogit <- function(g1, ChosenRun){

m <- lapply(seq_along(1:length(g1[[ChosenRun]])), function(i) {
  data.frame(
    Coefs = coef(g1[[ChosenRun]][[i]][[4]]$logit$fit$fit$fit),
    tidy(
      g1[[ChosenRun]][[i]][[4]]$logit$fit$fit$fit,
      conf.int = TRUE,
      exponentiate = TRUE
    ),
    Run = ChosenRun
  )
})

m2 <- do.call("rbind", m)

fwrite(m2, "CoefficientsModel.csv")

Coefs2 <- m2 %>%
  filter(Run %in% c(ChosenRun)) %>%
  group_by(term) %>%
  dplyr::summarize(Mean = mean(Coefs), sd = sd(Coefs)) %>%
  mutate(Odds = exp(Mean))


eq <- SetNames(Coefs2$Mean, names = Coefs2$term)

saveRDS(eq, "FinalCoeffients.RDS")
saveRDS(Coefs2, "FinalCoeffients.RDS") # this are the coffients that are going into the shiny app

return(list(eq, Coefs2))


}
