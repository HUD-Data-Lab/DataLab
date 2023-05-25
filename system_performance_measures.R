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
            by = "EnrollmentID")

### pick NbN projects
print(enrollment_data %>%
        filter(ProjectID %in% Project$ProjectID[Project$ProjectType == 0]) %>%
        group_by(ProjectID) %>%
        summarize(enrollments = n(), 
                  current_enrollments = n_distinct(
                    EnrollmentID[is.na(ExitDate) | ExitDate > report_start_date]),
                  household_enrollments = n_distinct(HouseholdID)) %>%
        filter(enrollments == household_enrollments) %>%
        select(-household_enrollments) %>%
        arrange(desc(enrollments)), 
      n = 20)

NbN_projects <- c(1212, 1210)
source("create_NbN_stays.R")
Project$ProjectType[Project$ProjectID %in%]

all_bed_nights <- Services %>%
  left_join(Enrollment %>%
              filter(ProjectID %in% Project$ProjectID[Project$ProjectType == 1]) %>%
              select(EnrollmentID, EntryDate) ,
            by = "EnrollmentID") %>%
  filter(RecordType == 200 & 
           TypeProvided == 200 &
           DateProvided >= EntryDate &
           DateProvided <= report_end_date)

bed_nights_in_report <- all_bed_nights %>%
  filter(DateProvided >= report_start_date)
