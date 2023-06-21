library(tidyverse)
library(readxl)
require(openxlsx)


file <- file.choose()

for (sheet in excel_sheets(file)) {
  name <- make.names(excel_sheets(file)[sheet])
  
  data <- read_excel(file, sheet = sheet) %>%
    rename_all(list(~make.names(.)))
  
  assign(sheet, data)
}


################################################################################
############################# Race #############################################
################################################################################

for (factor in colnames(All)[1:15]) {
  hold <- All %>%
    filter(Race != "Data not collected") %>%
    mutate(factor_of_interest := as.numeric(get(factor))) %>%
    filter(!is.na(factor_of_interest))
  
  res.aov <- aov(factor_of_interest ~ Race, data = hold)
  
  significance <- as.data.frame(TukeyHSD(res.aov)[[1]]) %>%
    mutate(race = rownames(.)) %>%
    select(race, `p adj`) %>%
    rename(!!quo_name(factor) := `p adj`)
  
  if (exists("all_significances")) {
    all_significances <- all_significances %>%
      left_join(significance, by = "race")
  } else {
    all_significances <- significance
  }
}

race_averages <- All[, 1:16] %>%
  group_by(Race) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
            n = n())


################################################################################
############################# Gender ###########################################
################################################################################

for (factor in colnames(All)[1:15]) {
  hold <- All %>%
    mutate(factor_of_interest := as.numeric(get(factor))) %>%
    filter(!is.na(factor_of_interest))
  
  res.aov <- aov(factor_of_interest ~ Gender, data = hold)
  
  significance <- as.data.frame(TukeyHSD(res.aov)[[1]]) %>%
    mutate(Gender = rownames(.)) %>%
    select(Gender, `p adj`) %>%
    rename(!!quo_name(factor) := `p adj`)
  
  if (exists("gender_significances")) {
    gender_significances <- gender_significances %>%
      left_join(significance, by = "Gender")
  } else {
    gender_significances <- significance
  }
}

gender_averages <- All[, c(1:15, 18)] %>%
  group_by(Gender) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
            n = n())

################################################################################
######################### Race & Gender ########################################
################################################################################

for (factor in colnames(All)[1:15]) {
  hold <- All %>%
    filter(Race != "Data not collected") %>%
    mutate(factor_of_interest := as.numeric(get(factor)),
           intersection = paste0(Race, " + ", Gender)) %>%
    filter(!is.na(factor_of_interest)) 
  
  res.aov <- aov(factor_of_interest ~ intersection, data = hold)
  
  significance <- as.data.frame(TukeyHSD(res.aov)[[1]]) %>%
    mutate(intersection = rownames(.)) %>%
    select(intersection, `p adj`) %>%
    rename(!!quo_name(factor) := `p adj`)
  
  if (exists("intersectional_significances")) {
    intersectional_significances <- intersectional_significances %>%
      left_join(significance, by = "intersection")
  } else {
    intersectional_significances <- significance
  }
}

intersectional_averages <- All[, c(1:16, 18)] %>%
  filter(Race != "Data not collected") %>%
  mutate(intersection = paste0(Race, " + ", Gender)) %>%
  group_by(intersection) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
            n = n())


write.xlsx(all_significances, file="Significances.xlsx", sheetName="race_significances", row.names=FALSE)
write.xlsx(race_averages, file="Significances.xlsx", sheetName="race_averages", append=TRUE, row.names=FALSE)
write.xlsx(gender_significances, file="Significances.xlsx", sheetName="gender_significances", append=TRUE, row.names=FALSE)
write.xlsx(gender_averages, file="Significances.xlsx", sheetName="gender_averages", append=TRUE, row.names=FALSE)
write.xlsx(intersectional_significances, file="Significances.xlsx", sheetName="intersectional_significances", append=TRUE, row.names=FALSE)
write.xlsx(intersectional_averages, file="Significances.xlsx", sheetName="intersectional_averages", append=TRUE, row.names=FALSE)

list_of_datasets <- list("all_significances" = all_significances,
                         "race_averages" = race_averages,
                         "gender_significances" = gender_significances,
                         "gender_averages" = gender_averages,
                         "intersectional_significances" = intersectional_significances,
                         "intersectional_averages" = intersectional_averages)

write.xlsx(list_of_datasets, file = "Significances.xlsx")
