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

library(tidyverse)
library(lubridate)
library(readxl)
library(kableExtra)

#SPM Programming specs: https://icfonline.sharepoint.com/:w:/r/sites/NHDAP/_layouts/15/Doc.aspx?action=edit&sourcedoc=%7B4ec4f3da-89f4-4c2d-810c-1f93ecc0e60c%7D&wdOrigin=TEAMS-ELECTRON.teamsSdk.openFilePreview&wdExp=TEAMS-CONTROL&web=1
#HMIS Glossary: https://icfonline.sharepoint.com/:w:/r/sites/NHDAP/_layouts/15/Doc.aspx?action=edit&sourcedoc=%7Bda79cae5-b933-41f0-b408-40162ade797d%7D&wdOrigin=TEAMS-ELECTRON.teamsSdk.openFilePreview&wdExp=TEAMS-CONTROL&web=1

lookback_stop_date <- ymd("2014-10-1")

system_performance_measures <- TRUE

source("00_read_2022_csv.R")
source("FY2024_mapping.R")

report_start_date <- lookback_stop_date %m+% years(7)

single_CoC_projects <- ProjectCoC %>%
  mutate(relevant_to_CoC = 
           str_sub(relevant_CoC, 4, 6) == str_sub(CoCCode, 4, 6)) %>%
  group_by(ProjectID) %>%
  summarise(num_of_CoCs = uniqueN(CoCCode),
            relevant_to_CoC = max(relevant_to_CoC)) %>%
  ungroup() %>%
  filter(num_of_CoCs == 1 &
           relevant_to_CoC == 1)

relevant_household_ids <- Enrollment %>%
  filter(RelationshipToHoH == 1 &
           (str_sub(relevant_CoC, 4, 6) == str_sub(EnrollmentCoC, 4, 6) |
              (is.na(EnrollmentCoC) & ProjectID %in% single_CoC_projects$ProjectID))) %>%
  select(HouseholdID) %>%
  distinct()

Enrollment <- Enrollment %>%
  filter(HouseholdID %in% relevant_household_ids$HouseholdID)

for (csv in c("Exit", "IncomeBenefits")) {
  assign(csv,
         get(csv) %>%
           filter(EnrollmentID %in% Enrollment$EnrollmentID))
}

enrollment_data <- Enrollment %>%
  select(all_of(colnames(Enrollment)[1:18])) %>%
  left_join(Exit %>%
              select(EnrollmentID, ExitDate, Destination),
            by = "EnrollmentID") %>%
  left_join(Project %>%
              select(ProjectID, ProjectType),
            by = "ProjectID")

NbN_projects <- c(1212, 1210)
source("create_NbN_stays.R")
Project$ProjectType[Project$ProjectID %in% NbN_projects] <- 1

all_bed_nights <- Services %>%
  inner_join(enrollment_data %>%
               filter(ProjectType == 1) %>%
               select(EnrollmentID, EntryDate, ExitDate),
             by = "EnrollmentID") %>%
  filter(RecordType == 200 & 
           TypeProvided == 200 &
           DateProvided >= EntryDate &
           DateProvided <= report_end_date &
           # removed because specs call this an error in data entry
           # which--correcting for data quality is a whole other thing
           (is.na(ExitDate) |
              DateProvided < ExitDate))

bed_nights_in_report <- all_bed_nights %>%
  filter(DateProvided >= report_start_date)

# need to review this logic...
bed_nights_in_report_ee_format <- bed_nights_in_report %>%
  left_join(enrollment_data %>%
              select(EnrollmentID, ProjectID, ProjectType),
            by = "EnrollmentID") %>%
  mutate(EnrollmentID = paste("S_", ServicesID),
         EntryDate = DateProvided,
         ExitDateAdj = DateProvided) %>%
  select(EnrollmentID, PersonalID, EntryDate, ExitDateAdj,
         ProjectID, ProjectType)

method_5_active_enrollments <- enrollment_data %>%
  filter(EntryDate <= report_end_date &
           (is.na(ExitDate) |
              ExitDate > report_start_date) &
           (
             (ProjectType == 1 &
                EnrollmentID %in% bed_nights_in_report$EnrollmentID)
             |
               (ProjectType %in% c(0, 2, 3, 8, 9, 10, 13))
           )
  )

items_to_keep <- c("items_to_keep", ls())

source(paste0(getwd(), "/System Performance Measures/system_performance_measures_1.R"))
source(paste0(getwd(), "/System Performance Measures/system_performance_measures_3.R"))
source(paste0(getwd(), "/System Performance Measures/system_performance_measures_5.R"))
source(paste0(getwd(), "/System Performance Measures/system_performance_measures_7.R"))


