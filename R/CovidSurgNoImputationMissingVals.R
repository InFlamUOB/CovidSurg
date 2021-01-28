

PreProc2 <- function(dataA) {
  # choose either age or agegroup and if so change 17-19
  dataA1Old <- dataA %>%
    dplyr::rename(RespPreop = preop_resp) %>%
    dplyr::rename(Label = mortality) %>%
    dplyr::rename(FinAnas = anaesthesia) %>%
    mutate(
      cardiacrisk = recode(cardiacrisk,
                           "0" = "0",
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4+",
                           "5" = "4+",
                           "6" = "4+"
      ),
      asa3 = recode(asa4,
                    "Grades 1" = "Grade 1",
                    "Grade 2" = "Grade 2",
                    "Grade 3" = "Grade 3",
                    "Grade 4" = "Grade 4-5",
                    "Grade 5" = "Grade 4-5"
      ),
      age = recode(age, '20-29 years' = "<40 years",
                   '30-39 years' = "<40 years")
    ) %>%
    select(-c(
      record_id,
      redcap_data_access_group,
      # crp,
      starts_with("anaes"),
      starts_with("respsupport"),
      agegroup,
      asa4,
      asa2 # ,
      # month
    )) %>%
    dplyr::rename(anaesthesia = FinAnas) %>%
    # drop_na() %>%
    filter(age != "17-19 years")
  
  dataA1Old$cardiacrisk <- as.factor(dataA1Old$cardiacrisk)
  head(dataA1Old)
  
  
  dataA1Labs <- dataA1Old %>%
    mutate(
      spec2 = recode(spec2,
                     "Abdominal" = "Abdominal surgery",
                     "Head and neck" = "Head and neck surgery",
                     "Limb" = "Orthopaedic surgery",
                     "obstetric" = "Obstetrics",
                     "other" = "Other surgery",
                     "Thorax" = "Cardiothoracic surgery"
      ),
      RespPreop = recode(RespPreop,
                         "none" = "None",
                         "oxygen" = "Supplemental oxygen",
                         "ventilated" = "Ventilation"
      ),
      time = recode(time,
                    "Preop" = "Preoperative",
                    "Postop" = "Postoperative"
      ),
      indication = recode(indication, "Benign/Obs" = "Benign"),
      grade998 = recode(grade998,
                        "major" = "Major",
                        "minor" = "Minor"
      ),
      month = recode(month,
                     "Apr" = "April",
                     "Mar" = "March",
                     "Jun" = "June",
                     "Jul" = "July",
                     "Feb" = "February"
      )
    ) %>%
    dplyr::rename(
      Age = age,
      Month = month,
      Sex = sex,
      "Grade of surgery" = grade998,
      Anaesthesia = anaesthesia,
      Speciality = spec2,
      "Haemoglobin (g/dl)" = hb,
      "White cell count (10^9/L)" = wcc,
      "Preoperative respiratory support" = RespPreop,
      "Indication for surgery" = indication,
      "Urgency of surgery" = urgency4,
      "Timing of SARS-CoV-2 diagnosis" = time,
      "Current smoker" = smoker,
      "Respiratory comorbidity" = respdisease,
      "Revised Cardiac Risk Index" = cardiacrisk,
      "ASA grade" = asa3,
      "C-reactive protein" = crp,
    )
  
  
  ## ----Missing Values--------------------------------------------------------------------------------
  
  
  ll <- data.frame(is.na(dataA1Labs)) # %>%
  cols <- sapply(ll, is.logical)
  ll[, cols] <- lapply(ll[, cols], as.numeric)
  ll <- ll %>%
    dplyr::rename(
      "Haemoglobin" = "Haemoglobin..g.dl.",
      "White cell count" = "White.cell.count..10.9.L.",
      "Preoperative respiratory support" = "Preoperative.respiratory.support",
      "C-reactive protein" = "C.reactive.protein"
    )
  
  #Not working
  
  # Miss1 <- upset(ll,
  #   nsets = 6, number.angles = 30, point.size = 3.5, line.size = 2,
  #   mainbar.y.label = "Missing Values", sets.x.label = "Total Number Missing Values",
  #   text.scale = c(2.3, 2.3, 2, 2, 2, 1.75), order.by = "freq", sets.bar.color = "red3"
  # )
  #
  # Miss2 <- vis_miss(dataA1Labs, sort_miss = TRUE) + scale_colour_manual(
  #   values = c("springgreen3", "red3"),
  #   aesthetics = c("fill")
  # ) + labs(fill = "Missing Values") +
  #   theme(axis.text.x = element_text(color = "grey20", size = 13, angle = 75, face = "plain"))
  #
  # pdf("MissingVal1.pdf", 15, 6)
  # Miss1
  # dev.off()
  #
  # pdf("MissingVal2.pdf", 15, 8)
  # Miss2
  # dev.off()
  
  
  
  ## ----Data_Distribution Non Imputed-------------------------------------------------------------------------------
  
  theme_set(theme_bw())
  
  pdf("DataDistributionNonImputed.pdf", 14, 7)
  
  print(Plot1a(dataA1Labs))
  print(Plot1b(dataA1Labs))
  
  dev.off()
  
  ## ----Imputation------------------------------------------------------------------------------
  
  
  # md.pattern(dataA1Old, rotate.names = TRUE) # very cool but cannot see anything
  # ini <- mice(dataA1Old, maxit = 0)
  # ini$nmis
  
  dataA2 <- dataA1Old %>%
    select(-crp) %>%
    drop_na() %>%
    mutate_if(is.character, as.factor)
  
  #ini2 <- mice(dataA2, maxit = 0)
  #ini2$nmis
  #
  #md.pattern(dataA2, rotate.names = TRUE) # very cool but cannot see anything
  #
  #imputed_data <- mice(dataA2, m = 2, seed = 132)
  ## imputed_data$imp$hb
  #full_data <- complete(imputed_data, 2)
  
  save(
    list = c( "dataA2"),
    file = paste0("ImputedDataAndFirst.RData")
  )
  
  ## ----Data_Distribution-Imputed-------------------------------------------------------------------------------
  
  
  dataA1ImpLabs <- dataA2 %>%
    mutate(
      spec2 = recode(spec2,
                     "Abdominal" = "Abdominal surgery",
                     "Head and neck" = "Head and neck surgery",
                     "Limb" = "Orthopaedic surgery",
                     "obstetric" = "Obstetrics",
                     "other" = "Other surgery",
                     "Thorax" = "Cardiothoracic surgery"
      ),
      RespPreop = recode(RespPreop,
                         "none" = "None",
                         "oxygen" = "Supplemental oxygen",
                         "ventilated" = "Ventilation"
      ),
      time = recode(time,
                    "Preop" = "Preoperative",
                    "Postop" = "Postoperative"
      ),
      indication = recode(indication, "Benign/Obs" = "Benign"),
      grade998 = recode(grade998,
                        "major" = "Major",
                        "minor" = "Minor"
      ),
      month = recode(month,
                     "Apr" = "April",
                     "Mar" = "March",
                     "Jun" = "June",
                     "Jul" = "July",
                     "Feb" = "February"
      )
    ) %>%
    dplyr::rename(
      Age = age,
      Month = month,
      Sex = sex,
      "Grade of surgery" = grade998,
      Anaesthesia = anaesthesia,
      Speciality = spec2,
      "Haemoglobin (g/dl)" = hb,
      "White cell count (10^9/L)" = wcc,
      "Preoperative respiratory support" = RespPreop,
      "Indication for surgery" = indication,
      "Urgency of surgery" = urgency4,
      "Timing of SARS-CoV-2 diagnosis" = time,
      "Current smoker" = smoker,
      "Respiratory comorbidity" = respdisease,
      "Revised Cardiac Risk Index" = cardiacrisk,
      "ASA grade" = asa3
    )
  
  
  
  
  
  ## ----Data_Distribution-Imputed-------------------------------------------------------------------------------
  
  
 # pdf("DataDistributionNoMissing.pdf", 15, 9)
 # 
 # print(Plot1a(dataA1ImpLabs))
 # print(Plot1b(dataA1ImpLabs))
 # 
 # dev.off()
  
  save(
    list = c("dataA1ImpLabs"),
    file = paste0("DataImpLabs.RData")
  )
  
  return(list(dataA2, dataA1Old, dataA1Labs, dataA1ImpLabs))
}
