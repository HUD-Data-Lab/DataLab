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

kit_type <- "new_kit"
combining_files <- kit_type == "old_kit"

source("datalab_functions.R")
source("DataLab_Lists.R")

if (combining_files) {
  file_source_dir <- choose.dir()
  
  datalab_zips <- list.files(file_source_dir, full.names = TRUE)
  
  for (zip in datalab_zips) {
    for (file in names(hmis_csvs)){
      
      data <- read_csv(unzip(zip, paste0(file, ".csv")),
                       col_types = get(file, hmis_csvs))
      
      if (exists(file)) {
        data <- get(file) %>%
          full_join(data, by = intersect(colnames(get(file)),
                                         colnames(data)))
      } 
      
      assign(file, data)
      
      file.remove(paste0(file, ".csv"))
    }
  }
} else {
  file_source <- file.choose()
  
  for (file in names(hmis_csvs)){
    
    data <- read_csv(unzip(file_source, paste0(file, ".csv")),
                     col_types = get(file, hmis_csvs))
    
    if (exists(file)) {
      data <- get(file) %>%
        full_join(data, by = intersect(colnames(get(file)),
                                       colnames(data)))
    } 
    
    assign(file, data)
    
    file.remove(paste0(file, ".csv"))
  }
}

source("DataLab_hc_variables.R")

# remove deleted records exportID colummns before proceeding with processing
for (file in names(hmis_csvs)){
  
  data <- get(file) %>%
    distinct()
  
  new_ExportID <- data$ExportID[1]
  col_order <- colnames(data)
  
  data <- data %>%
    select(-ExportID) %>%
    distinct() %>%
    mutate(ExportID = new_ExportID) %>%
    select(all_of(col_order))
  
  if ("DateDeleted" %in% colnames(get(file))) {
    data <- data %>%
      filter(is.na(DateDeleted) |
               DateDeleted > report_end_date)
  }
  
  if (file == "Enrollment") {
    data <- data %>%
      # rename(enroll_DateCreated = DateCreated) %>%
      mutate(MoveInDate = case_when(
        MoveInDate <= report_end_date &
          MoveInDate >= EntryDate ~ MoveInDate)) %>%
      filter(EntryDate <= report_end_date &
               EnrollmentID %nin% Exit$EnrollmentID[Exit$ExitDate < report_start_date])
  }
  
  if (file == "Exit") {
    data <- data %>%
      # rename(exit_DateCreated = DateCreated) %>%
      mutate(days_to_shift = sample(1:21, nrow(Exit), replace = TRUE),
             exit_DateCreated = as.POSIXct(ExitDate + days_to_shift)) %>%
      filter(ExitDate >= report_start_date &
               ExitDate <= report_end_date) %>%
      select(-days_to_shift)
  }
  
  if (file == "Funder") {
    data$Funder[data$ProjectID == 1552] <- 2
    data$Funder[data$ProjectID == 1554] <- 3
    data$Funder[data$ProjectID == 1565] <- 4
  }
  
  if (file == "Export") {
    data$SoftwareName <- "DataLab"
    data$SoftwareVersion <- "1.0"
  }
  
  assign(file, data)
  
}