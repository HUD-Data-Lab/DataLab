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

##  Measure 1a
{
  ##  uses this algorithm 
  ##  https://www.pharmasug.org/proceedings/2019/BP/PharmaSUG-2019-BP-219.pdf
  Q1a_line1_detail <- method_5_active_enrollments %>%
    filter(ProjectType %in% c(0, 8)) %>%
    mutate(ExitDateAdj = if_else(
      is.na(ExitDate) | ExitDate >= report_end_date,
      report_end_date, ExitDate %m-% days(1))) %>%
    # is this right?
    filter(EntryDate < ExitDateAdj) %>%
    # ...and the way I add it here
    full_join(bed_nights_in_report_ee_format,
              by = colnames(bed_nights_in_report_ee_format)) %>%
    # unnecessary, added for algorithm QA
    select(colnames(bed_nights_in_report_ee_format)) %>%
    arrange(PersonalID, EntryDate, ExitDateAdj) %>%
    group_by(PersonalID) %>%
    mutate(lag_EntryDate = lag(EntryDate),
           lag_ExitDateAdj = lag(ExitDateAdj),
           cummax_ExitDateAdj = as.Date(cummax(
             if_else(is.na(lag_ExitDateAdj), -Inf, as.integer(lag_ExitDateAdj))
           ), "1970-01-01"),
           cummax_ExitDateAdj = case_when(
             cummax_ExitDateAdj != -Inf ~ cummax_ExitDateAdj)) %>%
    ungroup() %>%
    mutate(
      after_earlier = (EntryDate > cummax_ExitDateAdj &
                            !is.na(lag_EntryDate)),
      different_person = PersonalID != lag(PersonalID) &
        !is.na(lag(PersonalID)),
      lot_block = cumsum(after_earlier | different_person) + 1
           )
  
  View(Q1a_line1_detail %>%filter(PersonalID == 670613))
  }