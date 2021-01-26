DataSplit <- function(full_data) {
  set.seed(132)


  # dataA1 <- full_data[[1]] # now dataA1 changes to the full_data
  dataA1 <- full_data # now dataA1 changes to the full_data
  data <- dataA1 %>%
    filter(month %in% c("Feb", "Mar", "Apr", "May")) %>%
    select(-month)

  Validation <- filter(dataA1, month %in% c("Jun", "July")) %>%
    select(-month)

  save(
    list = c("Validation",  "dataA1", "data"),
    file = paste0("Data3ValidationA1data.RData")
  )

  return(list(Validation, data))
}
