# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 

library(archive)
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/00_read_2024_csv.R")

insert <- function(original_list, item, position) {
  c(original_list[1:(position-1)], 
    item, 
    original_list[position:length(original_list)])
}

for (file in names(hmis_csvs_fy24)){
  
  data <- get(file) #%>%
  # mutate(across(
  #   where(~ inherits(.x, "Date") || inherits(.x, "POSIXct")),
  #   ~ .x %m+% years(2)
  # ))
  
  current_columns <- colnames(data)
  
  if (file == "Export") {
    data$ExportDate <- format.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S")
    data$CSVVersion <- "2026 v1.0"
  }
  
  if (file == "Client") {
    new_cols <- current_columns[current_columns %nin% 
                                  c("Woman", "Man","NonBinary", "CulturallySpecific", 
                                    "Transgender", "Questioning", "DifferentIdentity",
                                    "GenderNone", "DifferentIdentityText")]
    new_cols <- insert(new_cols, "Sex", 11)
    
    data <- data %>%
      mutate(Sex = as.integer(
        sample(c(0, 1, 8, 9, 99, NA), nrow(data), replace = TRUE,
               prob = c(25, 25, 5, 5, 5, 35)))) %>%
      select(all_of(new_cols)) %>%
      rename("HispanicLatinao" = "HispanicLatinaeo")
  }
  
  if (file == "Enrollment") {
    new_cols <- current_columns[current_columns %nin% c("SexualOrientation", 
                                                        "SexualOrientationOther",
                                                        "TranslationNeeded",
                                                        "PreferredLanguage",
                                                        "PreferredLanguageDifferent",
                                                        "LastPermanentStreet",
                                                        "LastPermanentCity",
                                                        "LastPermanentZIP")]
    new_cols <- insert(new_cols, "MentalHealthConsultation", 62)
    
    data <- data %>%
      mutate(MentalHealthConsultation = as.integer(case_when(
        EnrollmentID %in% 
          Enrollment$EnrollmentID[Enrollment$ProjectID %in% 
                                    Project$ProjectID[Project$ProjectType %in% c(6, 12, 13)] &
                                    Enrollment$ProjectID %in%
                                    Funder$ProjectID[Funder$Funder %in% c(33, 37:42, 45)]] ~ 
          sample(c(NA, 1:4), nrow(data), replace = TRUE)))) %>%
      select(all_of(new_cols))
  }
  
  assign(file, data)
  
}

## added per Meradith's 8/15 email
Exit <- Exit %>% 
  mutate(DestinationSubsidyType = if_else(Destination == 435 & 
                                            is.na(DestinationSubsidyType), 
                                          420, DestinationSubsidyType))

if (!dir.exists("created_files")) {
  dir.create("created_files")
}

# write to zipped folder
{
  for (file in unique(CSV_columns$File)) {
    write.csv(get(file) %>%
                mutate(
                  across(colnames(file)[str_locate_all(pattern = "T",
                                                       get(file,
                                                           hmis_csvs_fy26))[[1]][,1]],
                         ~ format(., format = "%Y-%m-%d %H:%M:%S")),
                  across(colnames(file)[str_locate_all(pattern = "D",
                                                       get(file,
                                                           hmis_csvs_fy26))[[1]][,1]],
                         ~ format(., format = "%Y-%m-%d")),
                  across(colnames(file)[str_locate_all(pattern = "d",
                                                       get(file,
                                                           hmis_csvs_fy26))[[1]][,1]],
                         ~ if_else(. > 0, sprintf("%.2f", .), NA))),
              file.path(paste0("created_files/", file, ".csv")),
              row.names=FALSE, na = "",
              quote = which(as.character(lapply(get(file), class)) %nin%
                              c("integer", "numeric")))
  }
  
  archive_write_dir(paste0("Data Lab - 2026 Zips ", Sys.Date(), ".zip"),
                    paste0(getwd(), "/created_files"))
  
  unlink(paste0(getwd(), "/created_files/*"))
}
