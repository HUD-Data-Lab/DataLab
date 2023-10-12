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

source("00_read_2024_csv.R")

report_start_date <- ymd("2021-10-1")
report_end_date <- ymd("2022-9-30")

relevant_projects <- Funder %>%
  filter(Funder == 21 &
           ProjectID %in% Project$ProjectID[Project$ProjectType %in% c(4, 6)] &
           StartDate <= report_end_date &
           (is.na(EndDate) |
              EndDate >= report_start_date)) %>%
  .$ProjectID %>%
  unique()

PATH_activity_date_columns <- c("EnrollmentID", "active_date", "type")

PATH_activity_dates <- CurrentLivingSituation %>%
  select(EnrollmentID, InformationDate) %>%
  mutate(type = "CLS") %>%
  `colnames<-`(PATH_activity_date_columns) %>%
  union(Enrollment %>%
          mutate(DateOfPATHStatus = if_else(ClientEnrolledInPATH == 1,
                                            DateOfPATHStatus, NA)) %>%
          select(EnrollmentID, DateOfEngagement, DateOfPATHStatus) %>% 
          pivot_longer(contains("Date"), names_to = "type", 
                       values_to = "active_date", values_drop_na = TRUE)) %>%
  union(Services %>%
          filter(RecordType == 141) %>%
          mutate(type = "Service") %>%
          select(EnrollmentID, DateProvided, type) %>%
          `colnames<-`(PATH_activity_date_columns)) %>%
  inner_join(Enrollment %>%
               select(EnrollmentID, EntryDate),
             by = "EnrollmentID") %>%
  left_join(Exit %>%
               select(EnrollmentID, ExitDate),
             by = "EnrollmentID") %>%
  filter(active_date >= EntryDate &
           (active_date <= ExitDate |
              is.na(ExitDate))) %>%
  select(-c(EntryDate, ExitDate))  %>%
  group_by(EnrollmentID, active_date) %>%
  mutate(
    number_in_day = seq(n()),
    in_report_period = active_date >= report_start_date &
      active_date <= report_end_date) %>%
  ungroup()

enrollment_data <- Enrollment %>%
  select(all_of(colnames(Enrollment)[1:21])) %>%
  left_join(Exit %>%
              select(EnrollmentID, ExitDate, Destination),
            by = "EnrollmentID") %>%
  left_join(Project %>%
              select(ProjectID, ProjectType),
            by = "ProjectID") %>%
  # left_join(Client %>%
  #             select(PersonalID, DOB),
  #           by = "PersonalID") %>%
  # mutate(
  #   MoveInDateAdj = case_when(
  #     !is.na(HoH_HMID) &
  #       HoH_HMID >= DOB &
  #       HoH_HMID >= EntryDate &
  #       (HoH_HMID <= ExitDate |
  #          is.na(ExitDate)) ~ HoH_HMID,
  #     !is.na(HoH_HMID) &
  #       (HoH_HMID <= ExitDate |
  #          is.na(ExitDate)) ~ EntryDate),
  #   lh_at_entry = ProjectType %in% c(0, 1, 4, 8) |
  #     (ProjectType %in% c(2, 3, 9, 10, 13) &
  #        (LivingSituation %in% 100:199 |
  #           (LivingSituation %in% c(0:99, 200:499) &
  #              LOSUnderThreshold == 1 &
  #              PreviousStreetESSH == 1))),
  #   date_for_age = (if_else( 
  #     EntryDate <= report_start_date,
  #     report_start_date, # Return Report Start date if true
  #     EntryDate)),
  #   age = trunc((DOB %--% date_for_age) / years(1))) %>%
  # select(-date_for_age)
  filter(
    ProjectID %in% relevant_projects &
      ((ExitDate >= report_start_date &
          ExitDate <= report_end_date) | 
         (EntryDate <= report_end_date &
            (is.na(ExitDate) |
               ExitDate > report_end_date) &
            EnrollmentID %in% PATH_activity_dates$EnrollmentID[PATH_activity_dates$in_report_period]))) %>%
  mutate(
    new_and_active = EntryDate >= report_start_date &
      EntryDate <= report_end_date,
    active_and_enrolled = !is.na(ClientEnrolledInPATH) &
      !is.na(DateOfPATHStatus) &
      ClientEnrolledInPATH == 1 &
      DateOfPATHStatus <= report_end_date &
      DateOfPATHStatus >= EntryDate &
      (DateOfPATHStatus <= ExitDate |
         is.na(ExitDate)),
    status_during_period = DateOfPATHStatus >= report_start_date &
      DateOfPATHStatus <= report_end_date,
    enrolled_during_period = status_during_period &
      ClientEnrolledInPATH == 1) %>%
  group_by(PersonalID) %>%
  arrange(desc(EntryDate)) %>%
  slice(1L) %>%
  ungroup()

# Q8-Q16
{
  Q12a_detail <- PATH_activity_dates %>%
    inner_join(enrollment_data %>%
                 filter(enrolled_during_period),
               join_by(EnrollmentID,
                       active_date <= DateOfPATHStatus))
  
  Q12b_detail <- PATH_activity_dates %>%
    filter(EnrollmentID %in% enrollment_data$EnrollmentID[enrollment_data$enrolled_during_period] &
             in_report_period)
  
  Q16_detail <- Services %>%
    filter(EnrollmentID %in% enrollment_data$EnrollmentID[enrollment_data$active_and_enrolled] &
             DateProvided >= report_start_date &
             DateProvided <= report_end_date &
             ((RecordType == 141 & #  141 is PATH service |
                 TypeProvided == 4) |
                (RecordType == 161 & #  161 is PATH referral
                   TypeProvided == 1 &
                   ReferralOutcome == 1))) 
    
  count <- c(
    # Q8
    # Number of persons contacted by PATH-funded staff this reporting period
    n_distinct(
      enrollment_data$PersonalID,
      na.rm = TRUE),
    # Q9
    # Number of new persons contacted this reporting period in a PATH Street 
    # Outreach project
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$new_and_active &
                                   enrollment_data$ProjectType == 4],
      na.rm = TRUE),
    # Q10
    # Number of new persons contacted this reporting period in a PATH Services 
    # Only project
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$new_and_active &
                                   enrollment_data$ProjectType == 6],
      na.rm = TRUE),
    # Q11
    # Total number of new persons contacted this reporting period (#9 + #10 = 
    # total new clients contacted)
      # question about using the most recent enrollment? what's a universe?
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$new_and_active],
      na.rm = TRUE),
    # Q12a
    # Instances of contact this reporting period prior to date of enrollment
      # reporting specifications don't say that the date must be within the 
      # report period, even though the specifications for the next row do
    nrow(Q12a_detail %>%
           filter(type == "CLS" |
                    number_in_day == 1)),
    # Q12b
    # Total instances of contact during the reporting period
    nrow(Q12b_detail %>%
           filter(type == "CLS" |
                    number_in_day == 1)),
    # Q13a
    # Number of new persons contacted this reporting period who could not be 
    # enrolled because of ineligibility for PATH
    nrow(enrollment_data %>%
      filter(new_and_active &
               status_during_period &
               ClientEnrolledInPATH == 0 &
               ReasonNotEnrolled == 1) %>%
        distinct(PersonalID)),
    # Q13b
    # Number of new persons contacted this reporting period who could not be 
    # enrolled because provider was unable to locate the client
    nrow(enrollment_data %>%
           filter(new_and_active &
                    status_during_period &
                    ClientEnrolledInPATH == 0 &
                    ReasonNotEnrolled == 3) %>%
           distinct(PersonalID)),
    # Q14
    # Number of new persons contacted this reporting period who became enrolled 
    # in PATH
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$new_and_active &
                                   enrollment_data$enrolled_during_period],
      na.rm = TRUE),
    # Q15
    # Number with active, enrolled PATH status at any point during the 
    # reporting period
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$active_and_enrolled],
      na.rm = TRUE),
    # Q16
    # Number of active, enrolled PATH clients receiving community mental 
    # health services through any funding source at any point during the 
    # reporting period
    n_distinct(
      Q16_detail$PersonalID,
      na.rm = TRUE
    )
  )
}

# Q17
{
  
}

# Q18
{
  
}

# Q19-Q24
{
  
}

# Q25
{
  
}

# Q26
{
  
}