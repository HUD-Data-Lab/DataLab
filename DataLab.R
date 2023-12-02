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

#source("00_read_2024_csv.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/00_read_2024_csv.R")

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

# apply NbN active logic to the enrollment table, since everything is based on that
Enrollment <- Enrollment %>%
  filter(ProjectID %nin% Project$ProjectID[Project$ProjectType == 1] |
           EnrollmentID %in% Exit$EnrollmentID[Exit$ExitDate >= report_start_date &
                                                 Exit$ExitDate <= report_end_date] |
           EnrollmentID %in% bed_nights_in_report$EnrollmentID)

disability_table <- Disabilities %>% 
  filter(DisabilityResponse %in% c(0, 1) |
           (DisabilityType == 10 &
              DisabilityResponse %in% c(2, 3))) %>%
  mutate(disability_name = case_when(DisabilityType == 5 ~ "Physical Disability",
                                     DisabilityType == 6 ~ "Developmental Disability",
                                     DisabilityType == 7 ~ "Chronic Health Condition",
                                     DisabilityType == 8 ~ "HIV/AIDS",
                                     DisabilityType == 9 ~ "Mental Health Disorder",
                                     DisabilityResponse == 1 ~ "Alcohol Use Disorder",
                                     DisabilityResponse == 2 ~ "Drug Use Disorder",
                                     DisabilityResponse == 3 ~ "Both Alcohol and Drug Use Disorders"),
         disabilities = case_when(
           DisabilityResponse %in% 1:3 ~
             if_else(disability_name == "Both Alcohol and Drug Use Disorders", 2, 1)),
         indefinite_and_impairs = ((DisabilityResponse == 1 &
                                     DisabilityType %in% c(6, 8)) |
                                     (DisabilityResponse %in% c(2, 3) &
                                        DisabilityType %in% c(5, 7, 9, 10) &
                                        IndefiniteAndImpairs == 1))) %>%
  select(EnrollmentID, DataCollectionStage, InformationDate, disability_name, 
         DisabilityResponse, indefinite_and_impairs, disabilities)

