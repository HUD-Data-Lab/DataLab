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
      ClientEnrolledInPATH == 1,
    leaver = !is.na(ExitDate) &
      ExitDate >= report_start_date &
      ExitDate <= report_end_date,
    stayer = is.na(ExitDate)) %>%
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
    
  Q8_16 <- as.data.frame(matrix(c(
    "Q8. Number of persons contacted by PATH-funded staff this reporting period",
    n_distinct(
      enrollment_data$PersonalID,
      na.rm = TRUE),
    "Q9. Number of new persons contacted this reporting period in a PATH Street Outreach project",
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$new_and_active &
                                   enrollment_data$ProjectType == 4],
      na.rm = TRUE),
    "Q10. Number of new persons contacted this reporting period in a PATH Services Only project",
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$new_and_active &
                                   enrollment_data$ProjectType == 6],
      na.rm = TRUE),
    "Q11. Total number of new persons contacted this reporting period (#9 + #10 = total new clients contacted)",
      # question about using the most recent enrollment? what's a universe?
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$new_and_active],
      na.rm = TRUE),
    "Q12a. Instances of contact this reporting period prior to date of enrollment",
      # reporting specifications don't say that the date must be within the 
      # report period, even though the specifications for the next row do
    nrow(Q12a_detail %>%
           filter(type == "CLS" |
                    number_in_day == 1)),
    "Q12b. Total instances of contact during the reporting period",
    nrow(Q12b_detail %>%
           filter(type == "CLS" |
                    number_in_day == 1)),
    "Q13a. Number of new persons contacted this reporting period who could not be enrolled because of ineligibility for PATH",
    nrow(enrollment_data %>%
      filter(new_and_active &
               status_during_period &
               ClientEnrolledInPATH == 0 &
               ReasonNotEnrolled == 1) %>%
        distinct(PersonalID)),
    "Q13b. Number of new persons contacted this reporting period who could not be enrolled because provider was unable to locate the client",
    nrow(enrollment_data %>%
           filter(new_and_active &
                    status_during_period &
                    ClientEnrolledInPATH == 0 &
                    ReasonNotEnrolled == 3) %>%
           distinct(PersonalID)),
    "Q14. Number of new persons contacted this reporting period who became enrolled in PATH",
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$new_and_active &
                                   enrollment_data$enrolled_during_period],
      na.rm = TRUE),
    "Q15. Number with active, enrolled PATH status at any point during the reporting period",
    n_distinct(
      enrollment_data$PersonalID[enrollment_data$active_and_enrolled],
      na.rm = TRUE),
    "Q16. Number of active, enrolled PATH clients receiving community mental health services through any funding source at any point during the reporting period",
    n_distinct(
      Q16_detail$PersonalID,
      na.rm = TRUE
    )
  ), ncol = 2, byrow = TRUE)) %>% 
    setNames(c("Persons served during this reporting period", "Count"))
}

# Q17
{
  service_list <- as.data.frame(matrix(c(
    "17a. Re-engagement", 1,
    "17b. Screening", 2,
    "17c. Clinical assessment", 14,
    "17d. Habilitation/rehabilitation", 3,
    "17e. Community Mental Health", 4,
    "17f. Substance use treatment", 5,
    "17g. Case management", 6,
    "17h. Residential suport services", 7,
    "17i. Housing minor renovation", 8,
    "17j. Housing moving assistance", 9, 
    "17k. Housing eligibility determination", 10,
    "17l. Security deposits", 11,
    "17m. One-time rent for eviction prevention", 12
  ), ncol = 2, byrow = TRUE)) %>% 
    setNames(c("Type.of.Service", "TypeProvided")) %>%
    mutate(TypeProvided = as.numeric(TypeProvided))
  
  Q17_detail <- service_list %>%
    left_join(Services %>%
                select(EnrollmentID, PersonalID, DateProvided, RecordType, 
                       TypeProvided) %>%
                filter(EnrollmentID %in% enrollment_data$EnrollmentID[enrollment_data$active_and_enrolled] &
                         DateProvided >= report_start_date &
                         DateProvided <= report_end_date &
                         RecordType == 141),
              by = "TypeProvided")
  
  Q17 <- Q17_detail %>%
    group_by(Type.of.Service) %>%
    summarise(Number.of.people.receiving.service = n_distinct(PersonalID,
                                                              na.rm = TRUE)) %>%
    arrange(Type.of.Service)
}

# Q18
{
  referral_list <- as.data.frame(matrix(c(
    "Community Mental Health", 1,
    "Substance Use Treatment", 2,
    "Primary Health/ Dental Care", 3,
    "Job Training", 4,
    "Educational Services", 5,
    "Housing Services", 6,
    "Temporary Housing", 11,
    "Permanent Housing", 7,
    "Income Assistance", 8,
    "Employment Assistance", 9,
    "Medical insurance", 10
  ), ncol = 2, byrow = TRUE)) %>% 
    setNames(c("Type.of.Referral", "TypeProvided")) %>%
    mutate(TypeProvided = as.numeric(TypeProvided))
    
  Q18_detail <- referral_list %>%
    left_join(Services %>%
                select(EnrollmentID, PersonalID, DateProvided, RecordType, 
                       TypeProvided, ReferralOutcome) %>%
                filter(EnrollmentID %in% enrollment_data$EnrollmentID[enrollment_data$active_and_enrolled] &
                         DateProvided >= report_start_date &
                         DateProvided <= report_end_date &
                         RecordType == 161),
              by = "TypeProvided")
  
  Q18 <- Q18_detail %>%
    group_by(Type.of.Referral) %>%
    summarise(Number.receiving.each.referral = n_distinct(PersonalID,
                                                          na.rm = TRUE),
              Number.who.attained.the.service.from.the.referral = n_distinct(PersonalID[ReferralOutcome == 1],
                                                                             na.rm = TRUE)) %>%
    arrange(Type.of.Referral)
}

convert_list_1.8 <- function(column_name) {
  names_for_1.8 <- c("Yes", "No", "Client doesn't know", 
                     "Client prefers not to answer", "Data not collected")
  values_for_1.8 <- c(1, 0, 8, 9, 99)
  
  
}

# Example usage:
original_values <- c(1, 0, 8, 9, 99, 1, 0)
converted_values <- convert_list_1.8(original_values)
print(converted_values)



# Q19-Q24
{
  
  names_for_1.8 <- c("Yes", "No", "Client doesn't know", 
                     "Client prefers not to answer", "Data not collected")
  values_for_1.8 <- c(1, 0, 8, 9, 99)
    
  convert_list_1.8 <- function(column_name) {
    names_for_1.8[match(column_name, values_for_1.8)]
  }
  
  columns_for_calculation <- c(IncomeTypes$IncomeGroup,
                               InsuranceTypes$InsuranceGroup)
  columns_for_Q19_24 <- c("IncomeFromAnySource", "ssi_or_ssdi",
                          "BenefitsFromAnySource", "InsuranceFromAnySource",
                          "medicaid_or_medicare", "other_health_insurance")
  
  Q19_detail <- IncomeBenefits %>%
    select(-PersonalID) %>%
    inner_join(enrollment_data %>%
                 mutate(ExitDateAdj = if_else(is.na(ExitDate), report_end_date, 
                                              ExitDate)) %>%
                 filter(active_and_enrolled),
               join_by(EnrollmentID,
                       InformationDate >= EntryDate,
                       InformationDate <= ExitDateAdj)) %>%
    select(c(PersonalID, EnrollmentID, InformationDate, EntryDate, ExitDate, 
             DataCollectionStage, IncomeFromAnySource, BenefitsFromAnySource,
             InsuranceFromAnySource, all_of(columns_for_calculation), leaver, stayer))
  
  Q19_processing <- Q19_detail %>%
    mutate(at_project_start = InformationDate == EntryDate &
             DataCollectionStage == 1,
           at_exit = leaver &
             InformationDate == ExitDate &
             DataCollectionStage == 3,
           at_report_end = stayer &
             DataCollectionStage %in% c(1, 2),
           across(all_of(columns_for_calculation),
                  ~ if_else(. == 1 & !is.na(.), 1, 0)),
           ssi_or_ssdi = pmax(SSI, SSDI), 
           medicaid_or_medicare = pmax(Medicaid, Medicare), 
           other_health_insurance = pmax(SCHIP, VHAServices, EmployerProvided,
                                         COBRA, PrivatePay, StateHealthIns,
                                         IndianHealthServices, OtherInsurance),
           across(all_of(columns_for_Q19_24),
                  ~ convert_list_1.8(.))) %>%
    group_by(EnrollmentID, at_project_start, at_exit, at_report_end) %>%
    arrange(desc(InformationDate)) %>%
    slice(1L) %>%
    ungroup()
  
  add_19_24_result_columns <- function(dataframe, target) {
    dataframe %>%
      mutate(Outcomes = {{target}}) %>%
      group_by(Outcomes) %>%
      summarize(at_project_start = n_distinct(PersonalID[at_project_start],
                                              na.rm = TRUE),
                at_exit = n_distinct(PersonalID[at_exit],
                                     na.rm = TRUE),
                at_report_end = n_distinct(PersonalID[at_report_end],
                                           na.rm = TRUE))
  }
  
  Q19_24 <- as.data.frame(c("19. Income from Any Source",
                            names_for_1.8))  %>%
    `colnames<-`(c("Outcomes")) %>%
    full_join(Q19_processing %>%
                add_19_24_result_columns(., IncomeFromAnySource),
              by = "Outcomes") %>%
    adorn_totals() %>%
    rbind(as.data.frame(c("20. Supplemental Security Income (SSI)/ Social",
                          names_for_1.8[1:2]))  %>%
            `colnames<-`(c("Outcomes")) %>%
            full_join(Q19_processing %>%
                        add_19_24_result_columns(., ssi_or_ssdi),
                      by = "Outcomes")) %>%
    rbind(as.data.frame(c("21. Non-Cash Benefits from Any Source",
                          names_for_1.8))  %>%
            `colnames<-`(c("Outcomes")) %>%
            full_join(Q19_processing %>%
                        add_19_24_result_columns(., BenefitsFromAnySource),
                      by = "Outcomes") %>%
            adorn_totals()) %>%
    rbind(as.data.frame(c("22. Covered by Health Insurance",
                          names_for_1.8))  %>%
            `colnames<-`(c("Outcomes")) %>%
            full_join(Q19_processing %>%
                        add_19_24_result_columns(., InsuranceFromAnySource),
                      by = "Outcomes") %>%
            adorn_totals()) %>%
    rbind(as.data.frame(c("23. MEDICAID/MEDICARE",
                          names_for_1.8[1:2]))  %>%
            `colnames<-`(c("Outcomes")) %>%
            full_join(Q19_processing %>%
                        add_19_24_result_columns(., medicaid_or_medicare),
                      by = "Outcomes")) %>%
    rbind(as.data.frame(c("24. All other health insurance",
                          names_for_1.8[1:2]))  %>%
            `colnames<-`(c("Outcomes")) %>%
            full_join(Q19_processing %>%
                        add_19_24_result_columns(., other_health_insurance),
                      by = "Outcomes"))
  
}

# Q25
{

  Q25_detail <- enrollment_data %>%
    select(EnrollmentID, PersonalID, EntryDate, ExitDate, Destination,
           stayer, leaver)
  
  destination_counts <- Q25_detail %>%
    group_by(Destination) %>%
    summarise(count = n_distinct(PersonalID, na.rm = TRUE)) %>%
    ungroup() 
  
  for(residence_type in c("Homeless", "Institutional","Temporary", "Permanent",  
                          "Other")) {
    
    residences_to_include <- ResidenceUses %>%
      filter(APR_LocationGroup == residence_type &
               !is.na(LocationDescription) &
               Destination) %>%
      mutate(
        LocationDescription = case_when(
          # Location %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
          # Location %in% c(30, 99) ~ "Data.Not.Collected",
          Location == 9 ~ "Client prefers not to answer",
          TRUE ~ LocationDescription),
        APR_LocationOrder = case_when(
          Location == 99 ~ 39,
          Location == 9 ~ 38,
          TRUE ~ APR_LocationOrder))
    
    group_of_residences <- residences_to_include %>%
      left_join(destination_counts,
                by = c("Location" = "Destination")) %>%
      arrange(APR_LocationOrder) %>%
      select(LocationDescription, count) %>%
      adorn_totals("row") %>%
      ifnull(., 0) %>%
      mutate(LocationDescription = case_when(
        LocationDescription == "Total" ~ paste(str_to_title(residence_type), "Subtotal"),
        TRUE ~ LocationDescription))
    
    group_title_row <- group_of_residences[0,]
    group_title_row[1,1] <- paste(residence_type, "Situations")
    
    group_of_residences <- rbind(group_title_row, group_of_residences)
    
    if (residence_type == "Homeless") {
      destination_group <- group_of_residences
    } else {
      destination_group <- destination_group %>%
        union(group_of_residences)
    }
  }
  
  Q25 <- destination_group %>%
    rbind(c("PATH-enrolled clients still active as of report end date (stayers)",
            n_distinct(Q25_detail$PersonalID[Q25_detail$stayer],
                       na.rm = TRUE))) %>%
    rbind(c("Total",
            n_distinct(Q25_detail$PersonalID,
                       na.rm = TRUE)))
  
}

# Q26
{
  
}