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

library(plyr)
library(lubridate)    ## used anywhere we do date math
library(ivs)          ## used in spm 14
library(writexl)

kit_type <- "new_kit"

if (kit_type == "new_kit") {
  lookback_stop_date <- ymd("2014-10-1")
} else {
  lookback_stop_date <- ymd("2012-10-1")
}

source("00_read_2024_csv.R")

report_start_date <- ymd("2021-10-1")
report_end_date <- ymd("2022-9-30")

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

hmids <- Enrollment %>%
  filter(RelationshipToHoH == 1 &
           ProjectID %in% Project$ProjectID[Project$ProjectType %in% c(3, 9, 10, 13)]) %>%
  group_by(HouseholdID) %>%
  arrange(EntryDate) %>%
  slice(1L) %>%
  ungroup() %>%
  select(HouseholdID, MoveInDate) %>%
  rename(HoH_HMID = MoveInDate)

enrollment_data <- Enrollment %>%
  select(all_of(colnames(Enrollment)[1:18])) %>%
  left_join(Exit %>%
              select(EnrollmentID, ExitDate, Destination),
            by = "EnrollmentID") %>%
  left_join(Project %>%
              select(ProjectID, ProjectType),
            by = "ProjectID") %>%
  left_join(Client %>%
              select(PersonalID, DOB),
            by = "PersonalID") %>%
  left_join(hmids,
            by = "HouseholdID") %>%
  mutate(
    MoveInDateAdj = case_when(
      !is.na(HoH_HMID) &
        HoH_HMID >= DOB &
        HoH_HMID >= EntryDate &
        (HoH_HMID <= ExitDate |
           is.na(ExitDate)) ~ HoH_HMID,
      !is.na(HoH_HMID) &
        (HoH_HMID <= ExitDate |
           is.na(ExitDate)) ~ EntryDate),
    lh_at_entry = ProjectType %in% c(0, 1, 4, 8) |
      (ProjectType %in% c(2, 3, 9, 10, 13) &
         (LivingSituation %in% 100:199 |
            (LivingSituation %in% c(0:99, 200:499) &
               LOSUnderThreshold == 1 &
               PreviousStreetESSH == 1))),
    date_for_age = (if_else( 
      EntryDate <= report_start_date,
      report_start_date, # Return Report Start date if true
      EntryDate)),
    age = trunc((DOB %--% date_for_age) / years(1))) %>%
  select(-date_for_age)

all_bed_nights <- Services %>%
  inner_join(enrollment_data %>%
               filter(ProjectType == 1) %>%
               inner_join(Project %>%
                            select(ProjectID, OperatingStartDate),
                          by = "ProjectID") %>%
               select(EnrollmentID, EntryDate, ExitDate, OperatingStartDate),
             by = "EnrollmentID") %>%
  filter(RecordType == 200 & 
           TypeProvided == 200 &
           DateProvided >= EntryDate &
           DateProvided <= report_end_date &
           DateProvided >= OperatingStartDate,
         # removed because specs call this an error in data entry
         # which--correcting for data quality is a whole other thing
         (is.na(ExitDate) |
            DateProvided < ExitDate))

bed_nights_in_report <- all_bed_nights %>%
  filter(DateProvided >= report_start_date)
# 
# 
# # need to review this logic...
# bed_nights_in_report_ee_format <- bed_nights_in_report %>%
#   left_join(enrollment_data %>%
#               select(EnrollmentID, ProjectID, ProjectType),
#             by = "EnrollmentID") %>%
#   mutate(EnrollmentID = paste("S_", ServicesID),
#          EntryDate = DateProvided,
#          ExitDate = DateProvided) %>%
#   select(EnrollmentID, PersonalID, EntryDate, ExitDate,
#          ProjectID, ProjectType)

# need to review this logic...
bed_nights_ee_format <- all_bed_nights %>%
  left_join(enrollment_data %>%
              select(EnrollmentID, ProjectID, ProjectType),
            by = "EnrollmentID") %>%
  mutate(original_enrollment_id = EnrollmentID,
         EnrollmentID = paste("S_", ServicesID),
         EntryDate = DateProvided,
         ExitDate = DateProvided) %>%
  select(EnrollmentID, PersonalID, EntryDate, ExitDate,
         ProjectID, ProjectType, original_enrollment_id)

propagate_3.917 <- enrollment_data %>%
  filter(RelationshipToHoH == 1 &
           lh_at_entry &
           !is.na(DateToStreetESSH)) %>%
  group_by(HouseholdID) %>%
  slice(1L) %>%
  ungroup() %>%
  select(HouseholdID, DateToStreetESSH) %>%
  rename(HoH_DateToStreetESSH = DateToStreetESSH)

active_enrollments <- enrollment_data %>%
  left_join(propagate_3.917, 
            by = "HouseholdID") %>%
  mutate(
    Method1 = EntryDate <= report_end_date &
      (is.na(ExitDate) |
         ExitDate >= report_start_date),
    Method2 = (ExitDate >= report_start_date &
                 ExitDate <= report_end_date) | 
      (Method1 &
         EnrollmentID %in% bed_nights_in_report$EnrollmentID
       # don't need to check bed night date against project 
       # start at this point because it's done in `all_bed_nights`
      ),
    Method5 = Method1 &
      # don't need to check project type because that's done in `all_bed_nights`
      (EnrollmentID %in% bed_nights_in_report$EnrollmentID |
         ProjectType %in% c(0, 2, 3, 8, 9, 10, 13)),
    DateToStreetESSH = case_when(
      !is.na(age) & 
        is.na(DateToStreetESSH) &
        age < 18 ~ HoH_DateToStreetESSH,
      TRUE ~ DateToStreetESSH),
    DateToStreetESSH = case_when(
      !is.na(DateToStreetESSH) &
        DateToStreetESSH < DOB ~ DOB,
      TRUE ~ DateToStreetESSH
    )) %>%
  select(-HoH_DateToStreetESSH)

items_to_keep <- c("items_to_keep", ls())

#############################

source(paste0(getwd(), "/System Performance Measures/system_performance_measures_1.R"))
source(paste0(getwd(), "/System Performance Measures/system_performance_measures_2.R"))
source(paste0(getwd(), "/System Performance Measures/system_performance_measures_3.R"))
source(paste0(getwd(), "/System Performance Measures/system_performance_measures_4.R"))
source(paste0(getwd(), "/System Performance Measures/system_performance_measures_5.R"))
source(paste0(getwd(), "/System Performance Measures/system_performance_measures_7.R"))

dq_tables <- ls()[sapply(ls(),function(t) is.data.frame(get(t))) &
                    str_detect(ls(), "_dq")]

spm_tables <- llply(
  mget(ls()[sapply(ls(),function(t) is.data.frame(get(t))) &
              str_detect(ls(), "spm_") &
              !str_detect(ls(), "_dq")]), 
  set_hud_format)

write_xlsx(
  mget(dq_tables),
  paste0("SPM_dq ", Sys.Date(), ".xlsx")
)

write_xlsx(
  spm_tables,
  paste0("SPM_tables ", Sys.Date(), ".xlsx")
)
