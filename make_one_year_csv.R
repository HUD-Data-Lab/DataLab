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

#source("datalab_functions.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/datalab_functions.R")
#source("DataLab_Lists.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_Lists.R")

file_source <- file.choose()

for (file in names(hmis_csvs_fy24)){
  
  data <- read_csv(utils::unzip(file_source, paste0(file, ".csv")),
                   col_types = get(file, hmis_csvs_fy24))
  
  if (exists(file)) {
    data <- get(file) %>%
      full_join(data, by = intersect(colnames(get(file)),
                                     colnames(data)))
  } 
  
  assign(file, data)
  
  file.remove(paste0(file, ".csv"))
}

source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_hc_variables.R")

report_start_date <- "2021-10-01"
rearranged_hmis_csvs_fy24 <- c("Enrollment", "Project", "Exit", "Export", 
                               "Organization", "User", 
                               "Funder", "ProjectCoC", "Inventory", "Affiliation",
                               "HMISParticipation", "CEParticipation", "Client",
                               "IncomeBenefits", "HealthAndDV",
                               "EmploymentEducation", "Disabilities", "Services",
                               "CurrentLivingSituation", "Assessment",
                               "AssessmentQuestions", "AssessmentResults",
                               "Event", "YouthEducationStatus")

# remove deleted records exportID colummns before proceeding with processing
for (file in rearranged_hmis_csvs_fy24){
  
  data <- get(file) %>%
    distinct()
  
  print(paste0(file, ": ", nrow(data)))
  
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
      mutate(MoveInDate = case_when(
        MoveInDate <= report_end_date &
          MoveInDate >= EntryDate ~ MoveInDate)) %>%
      filter(EntryDate <= report_end_date &
               EnrollmentID %nin% Exit$EnrollmentID[Exit$ExitDate < report_start_date])
  }
  
  if (file == "Exit") {
    data <- data %>%
      filter(ExitDate >= report_start_date &
               ExitDate <= report_end_date) 
  }
  
  if ("EnrollmentID" %in% colnames(get(file))) {
    data <- data %>%
      filter(EnrollmentID %in% Enrollment$EnrollmentID)
  }
  
  if ("ProjectID" %in% colnames(get(file))) {
    data <- data %>%
      filter(ProjectID %in% Enrollment$ProjectID)
  }
  
  if ("PersonalID" %in% colnames(get(file))) {
    data <- data %>%
      filter(PersonalID %in% Enrollment$PersonalID)
  }
  
  if (file == "Organization") {
    data <- data %>%
      filter(OrganizationID %in% Project$OrganizationID)
  }
  
  if (file == "Export") {
    data$SoftwareName <- "DataLab"
    data$SoftwareVersion <- "1.0"
  }
  
  print(paste0(file, ": ", nrow(data)))
  
  assign(file, data)
  
}



library(archive)

folder_name <- paste("Test Kit", format(Sys.Date(), "%m.%d.%y"))

if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

if (!dir.exists("created_files")) {
  dir.create("created_files")
}

if (!dir.exists("created_files_2")) {
  dir.create("created_files_2")
}

if (!dir.exists(paste0(folder_name, "/HMIS CSVs"))) {
  dir.create(paste0(folder_name, "/HMIS CSVs"))
}

write_csvs_for(Project$ProjectID, zip_title = "Mini Zip Set",
               write_to <- paste0(folder_name, "/HMIS CSVs"))
