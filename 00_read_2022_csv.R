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

kit_type <- "old_kit" # can switch from new_kit to old_kit
combining_files <- kit_type == "old_kit"

source("datalab_functions.R")
source("DataLab_Lists.R")

library(here)
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor) 

if(!exists("directory")) directory <- "CurrentGood"

# Affiliation -------------------------------------------------------------

Affiliation <-
  read_csv(here(paste0(directory, "/Affiliation.csv")),
           col_types = "cccTTcTc",
           n_max = 1000L)

# Client ------------------------------------------------------------------

# This script later overwrites the Client.csv, masking Name and SSN PII. So
# this logic will read in the modified file - or - the raw one straight from SP

Client <-
  read_csv(paste0(directory, "/Client.csv"),
           col_types = "cccccncn?nnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc")

# Current Living Situation ------------------------------------------------

CurrentLivingSituation <-
  read_csv(paste0(directory, "/CurrentLivingSituation.csv"),
           col_types = "cccDncnnnnncTTcTc",
           n_max = 1000L) 

# Disabilities ------------------------------------------------------------

Disabilities <-
  read_csv(paste0(directory, "/Disabilities.csv"),
           col_types = "cccDnnnnnnnnnnnTTcTc")

# EmploymentEducation -----------------------------------------------------

EmploymentEducation <-
  read_csv(paste0(directory, "/EmploymentEducation.csv"),
           col_types = "cccDnnnnnnTTcTc")

# Exit --------------------------------------------------------------------

Exit <-
  read_csv(paste0(directory, "/Exit.csv"),
           col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc") %>%
  mutate(DateCreated = format.Date(DateCreated, "%F %T"),
         DateUpdated = format.Date(DateUpdated, "%F %T"))

# Project -----------------------------------------------------------------

Project <- 
  read_csv(paste0(directory, "/Project.csv"),
           col_types = "ccccDDnnnnnnnnnTTcTc") 

# EnrollmentCoC -----------------------------------------------------------

EnrollmentCoC <- 
  read_csv(paste0(directory, "/EnrollmentCoC.csv"), 
           col_types = "cccccDcnTTcTc") 

# Enrollment --------------------------------------------------------------

Enrollment <-
  read_csv(paste0(directory, "/Enrollment.csv"),
           col_types =
             "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc") %>%
  mutate(DateCreated = format.Date(DateCreated, "%F %T"),
         DateUpdated = format.Date(DateUpdated, "%F %T"))

# Event -------------------------------------------------------------------

Event <-
  read_csv(paste0(directory, "/Event.csv"),
           col_types = "ccnDnnncnDTTcTc",
           n_max = 1000L) 

# Export ------------------------------------------------------------------

Export <- 
  read_csv(paste0(directory, "/Export.csv"),
           col_types = "cncccccccTDDcccnnn")

# Funder ------------------------------------------------------------------

Funder <- 
  read_csv(paste0(directory, "/Funder.csv"),
           col_types = "ccnccDDTTcTc")

# HealthAndDV -------------------------------------------------------------

HealthAndDV <-
  read_csv(paste0(directory, "/HealthAndDV.csv"),
           col_types = "cccDnnnnnnnDnnnnnTTcTc")

# IncomeBenefits ----------------------------------------------------------

IncomeBenefits <- 
  read_csv(paste0(directory, "/IncomeBenefits.csv"),
           col_types = 
             "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")

# Inventory ---------------------------------------------------------------

Inventory <-
  read_csv(paste0(directory, "/Inventory.csv"),
           col_types = "cccnnnnnnnnnnnnDDTTcTc")

# Organization ------------------------------------------------------------

Organization <- 
  read_csv(paste0(directory, "/Organization.csv"),
           col_types = "ccncTTcTc")

# ProjectCoC --------------------------------------------------------------

ProjectCoC <- 
  read_csv(paste0(directory, "/ProjectCoC.csv"),
           col_types = "cccccccccnTTcTc")

# Users ------------------------------------------------------------------
User <- read_csv(paste0(directory, "/User.csv"),
                 col_types = "ccccccTTTc")

# Users ------------------------------------------------------------------
YouthEducationStatus <- read_csv(paste0(directory, "/YouthEducationStatus.csv"),
                                 col_types = "cccDnnnnTTcTc")

# Services ----------------------------------------------------------------

Services <- read_csv(paste0(directory, "/Services.csv"),
                     col_types = "cccDnnccnnnTTcTc")

# Assessment --------------------------------------------------------------

Assessment <- read_csv(paste0(directory, "/Assessment.csv"),
                       col_types = "cccDcnnnTTcTc")

# AssessmentQuestions -----------------------------------------------------

AssessmentQuestions <- read_csv(paste0(directory, "/AssessmentQuestions.csv"),
                                col_types = "cccccnccTTcTc")

# AssessmentResults -------------------------------------------------------

AssessmentResults <- read_csv(paste0(directory, "/AssessmentResults.csv"),
                              col_types = "ccccccTTcTc")

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
