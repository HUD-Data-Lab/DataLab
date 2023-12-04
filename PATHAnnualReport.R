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

source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab.R")

report_start_date <- ymd("2022-7-1")
report_end_date <- ymd("2023-6-30")
generate_new_kits <- TRUE

# for (organization in c(47, 106, 109)) {
for (organization in c(473, 1153)) {
  relevant_projects <- Funder %>%
    filter(Funder == 21 &
             ProjectID %in% Project$ProjectID[Project$ProjectType %in% c(4, 6) &
                                                Project$OrganizationID == organization] &
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
  
  data_prep <- Enrollment %>%
    select(all_of(colnames(Enrollment)[1:21])) %>%
    left_join(Exit %>%
                select(EnrollmentID, ExitDate, Destination),
              by = "EnrollmentID") %>%
    left_join(Project %>%
                select(ProjectID, ProjectType),
              by = "ProjectID") %>%
    filter(
      ProjectID %in% relevant_projects &
        ((ExitDate >= report_start_date &
            ExitDate <= report_end_date) | 
           (EntryDate <= report_end_date &
              (is.na(ExitDate) |
                 ExitDate > report_end_date) &
              EnrollmentID %in% PATH_activity_dates$EnrollmentID[PATH_activity_dates$in_report_period]))) 
  
  general_detail <- data_prep %>%
    left_join(Client %>%
                select(PersonalID, DOB),
              by = "PersonalID") %>%
    mutate(
      ExitDateAdj = if_else(is.na(ExitDate), report_end_date, 
                            ExitDate),
      new_and_active = EntryDate >= report_start_date &
        EntryDate <= report_end_date &
        PersonalID %nin% data_prep$PersonalID[data_prep$EntryDate < report_start_date],
      active_and_enrolled = !is.na(ClientEnrolledInPATH) &
        !is.na(DateOfPATHStatus) &
        ClientEnrolledInPATH == 1 &
        DateOfPATHStatus <= report_end_date &
        DateOfPATHStatus >= EntryDate &
        DateOfPATHStatus <= ExitDateAdj,
      status_during_period = DateOfPATHStatus >= report_start_date &
        DateOfPATHStatus <= report_end_date,
      enrolled_during_period = status_during_period &
        ClientEnrolledInPATH == 1,
      leaver = !is.na(ExitDate) &
        ExitDate >= report_start_date &
        ExitDate <= report_end_date,
      stayer = is.na(ExitDate),
      date_for_age = (if_else(
        EntryDate <= report_start_date,
        report_start_date, # Return Report Start date if true
        EntryDate)),
      age = trunc((DOB %--% date_for_age) / years(1))) %>%
    select(-date_for_age) %>%
    group_by(PersonalID) %>%
    arrange(desc(EntryDate)) %>%
    slice(1L) %>%
    ungroup()
  
  # Q8-Q16
  {
    Q12a_detail <- PATH_activity_dates %>%
      inner_join(general_detail %>%
                   filter(enrolled_during_period),
                 join_by(EnrollmentID,
                         active_date <= DateOfPATHStatus))
    
    Q12b_detail <- PATH_activity_dates %>%
      filter(EnrollmentID %in% general_detail$EnrollmentID[general_detail$enrolled_during_period] &
               in_report_period)
    
    Q16_detail <- Services %>%
      filter(EnrollmentID %in% general_detail$EnrollmentID[general_detail$active_and_enrolled] &
               DateProvided >= report_start_date &
               DateProvided <= report_end_date &
               ((RecordType == 141 & #  141 is PATH service |
                   TypeProvided == 4) |
                  (RecordType == 161 & #  161 is PATH referral
                     TypeProvided == 1 &
                     ReferralOutcome == 1))) 
    
    Q8_16 <- as.data.frame(matrix(c(
      "8. Number of persons contacted by PATH-funded staff this reporting period",
      n_distinct(
        general_detail$PersonalID,
        na.rm = TRUE),
      "9. Number of new persons contacted this reporting period in a PATH Street Outreach project",
      n_distinct(
        general_detail$PersonalID[general_detail$new_and_active &
                                    general_detail$ProjectType == 4],
        na.rm = TRUE),
      "10. Number of new persons contacted this reporting period in a PATH Services Only project",
      n_distinct(
        general_detail$PersonalID[general_detail$new_and_active &
                                    general_detail$ProjectType == 6],
        na.rm = TRUE),
      "11. Total number of new persons contacted this reporting period (#9 + #10 = total new clients contacted)",
      # question about using the most recent enrollment? what's a universe?
      n_distinct(
        general_detail$PersonalID[general_detail$new_and_active],
        na.rm = TRUE),
      "12a. Instances of contact this reporting period prior to date of enrollment",
      # reporting specifications don't say that the date must be within the 
      # report period, even though the specifications for the next row do
      nrow(Q12a_detail %>%
             filter(type == "CLS" |
                      number_in_day == 1)),
      "12b. Total instances of contact during the reporting period",
      nrow(Q12b_detail %>%
             filter(type == "CLS" |
                      number_in_day == 1)),
      "13a. Number of new persons contacted this reporting period who could not be enrolled because of ineligibility for PATH",
      nrow(general_detail %>%
             filter(new_and_active &
                      status_during_period &
                      ClientEnrolledInPATH == 0 &
                      ReasonNotEnrolled == 1) %>%
             distinct(PersonalID)),
      "13b. Number of new persons contacted this reporting period who could not be enrolled because provider was unable to locate the client",
      nrow(general_detail %>%
             filter(new_and_active &
                      status_during_period &
                      ClientEnrolledInPATH == 0 &
                      ReasonNotEnrolled == 3) %>%
             distinct(PersonalID)),
      "14. Number of new persons contacted this reporting period who became enrolled in PATH",
      n_distinct(
        general_detail$PersonalID[general_detail$new_and_active &
                                    general_detail$enrolled_during_period],
        na.rm = TRUE),
      "15. Number with active, enrolled PATH status at any point during the reporting period",
      n_distinct(
        general_detail$PersonalID[general_detail$active_and_enrolled],
        na.rm = TRUE),
      "16. Number of active, enrolled PATH clients receiving community mental health services through any funding source at any point during the reporting period",
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
                  filter(EnrollmentID %in% general_detail$EnrollmentID[general_detail$active_and_enrolled] &
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
      "Medical Insurance", 10
    ), ncol = 2, byrow = TRUE)) %>% 
      setNames(c("Type.of.Referral", "TypeProvided")) %>%
      mutate(TypeProvided = as.numeric(TypeProvided),
             Type.of.Referral = factor(Type.of.Referral, 
                                       levels=Type.of.Referral, 
                                       ordered=TRUE))
    
    Q18_detail <- referral_list %>%
      left_join(Services %>%
                  select(EnrollmentID, PersonalID, DateProvided, RecordType, 
                         TypeProvided, ReferralOutcome) %>%
                  filter(EnrollmentID %in% general_detail$EnrollmentID[general_detail$active_and_enrolled] &
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
  
  # convert_list_1.8 <- function(column_name) {
  #   names_for_1.8 <- c("Yes", "No", "Client doesn't know", 
  #                      "Client prefers not to answer", "Data not collected")
  #   values_for_1.8 <- c(1, 0, 8, 9, 99)
  # }
  
  # Q19-Q24
  {
    
    names_for_1.8 <- c("Yes", "No", "Client doesn't know", 
                       "Client prefers not to answer", "Data not collected")
    values_for_1.8 <- c(1, 0, 8, 9, 99)
    
    convert_list_1.8 <- function(column_name) {
      # replace_na(column_name, 99)
      names_for_1.8[match(column_name %>%
                            replace_na(99), values_for_1.8)]
    }
    
    columns_for_calculation <- c(IncomeTypes$IncomeGroup,
                                 InsuranceTypes$InsuranceGroup)
    columns_for_Q19_24 <- c("IncomeFromAnySource", "ssi_or_ssdi",
                            "BenefitsFromAnySource", "InsuranceFromAnySource",
                            "medicaid_or_medicare", "other_health_insurance")
    
    Q19_detail <- IncomeBenefits %>%
      select(-PersonalID) %>%
      inner_join(general_detail %>%
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
                        by = "Outcomes"))%>%
      `colnames<-`(c("Outcomes",	"At PATH Project Start",	
                     "At PATH Project Exit (for clients who were exited from PATH in the reporting period - leavers)",
                     "At Report End Date (for clients who were still active in PATH as of Report End Date - stayers"))
    
  }
  
  # Q25
  {
    Q25_detail <- general_detail %>%
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
    Q26a_e_detail <- general_detail %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDate, age, Destination,
             stayer, leaver) %>%
      left_join(Client %>%
                  select(PersonalID, DOB, DOBDataQuality, VeteranStatus,
                         all_of(names(gender_columns)), GenderNone, 
                         all_of(unname(race_columns)), RaceNone),
                by = "PersonalID")
    
    # Q26a
    gender_table <- as.data.frame(c(unname(gender_columns),
                                    names_for_1.8[3:5]))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(
        Q26a_e_detail %>% 
          select(PersonalID, all_of(names(gender_columns)), GenderNone) %>%
          ##  this bit sets GenderNone to 99 if there are no responses at all
          mutate(GenderNone = if_else(
            rowSums(across(c(all_of(names(gender_columns)), GenderNone)),
                    na.rm = TRUE) == 0, 
            99, GenderNone)) %>% 
          gather(ind, value, -PersonalID) %>%
          filter(value > 0) %>%
          full_join(stack(gender_columns),
                    by = "ind") %>%
          mutate(col_b = case_when(
            value == 1 |
              is.na(value) ~ values,
            TRUE ~ convert_list_1.8(value))) %>%
          group_by(col_b) %>%
          summarise(individuals = n_distinct(PersonalID,
                                             na.rm = TRUE)),
        by = "col_b") %>%
      rbind(c("Total",
              n_distinct(Q26a_e_detail$PersonalID,
                         na.rm = TRUE))) %>% 
      mutate(col_a = case_when(
        col_b == unname(gender_columns)[1] ~ "26a. Gender"), .before = col_b)
    
    # Q26b
    age_groups <- c("17 and under", "18 – 23", "24 – 30", "31 – 40",
                    "41 – 50", "51 – 61", "62 and over")
    
    age_table <- as.data.frame(c(age_groups, names_for_1.8[3:5])) %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26a_e_detail %>%
                  mutate(
                    col_b = case_when(
                      is.na(DOB) ~ convert_list_1.8(DOBDataQuality),
                      age <= 17 ~ age_groups[1],
                      age <= 23 ~ age_groups[2],
                      age <= 30 ~ age_groups[3],
                      age <= 40 ~ age_groups[4],
                      age <= 50 ~ age_groups[5],
                      age <= 61 ~ age_groups[6],
                      TRUE ~ age_groups[7])) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>% 
      mutate(col_a = case_when(
        col_b == age_groups[1] ~ "26b. Age"), .before = col_b)
    
    # Q26c
    race_table <- as.data.frame(c(names(race_columns),
                                  names_for_1.8[3:5]))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(
        Q26a_e_detail %>% 
          select(PersonalID, all_of(unname(race_columns)), RaceNone) %>%
          ##  this bit sets RaceNone to 99 if there are no responses at all
          mutate(RaceNone = if_else(
            rowSums(across(c(all_of(unname(race_columns)), RaceNone)),
                    na.rm = TRUE) == 0, 
            99, RaceNone)) %>% 
          gather(values, value, -PersonalID) %>%
          filter(value > 0) %>%
          full_join(stack(race_columns),
                    by = "values") %>%
          mutate(col_b = case_when(
            value == 1 |
              is.na(value) ~ ind,
            TRUE ~ convert_list_1.8(value))) %>%
          group_by(col_b) %>%
          summarise(individuals = n_distinct(PersonalID,
                                             na.rm = TRUE)),
        by = "col_b") %>%
      rbind(c("Total",
              n_distinct(Q26a_e_detail$PersonalID,
                         na.rm = TRUE))) %>% 
      mutate(col_a = case_when(
        col_b == names(race_columns)[1] ~ "26c. Race and Ethnicity"), 
        .before = col_b)
    
    # Q26d was ethnicity, removed this year
    
    # Q26e
    veteran_table <- as.data.frame(c("Veteran", "Non-veteran",
                                     names_for_1.8[3:5]))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(
        Q26a_e_detail %>%
          filter(!is.na(age) &
                   age >= 18) %>%
          mutate(
            col_b = case_when(
              is.na(VeteranStatus) |
                VeteranStatus > 1 ~ convert_list_1.8(VeteranStatus),
              VeteranStatus == 1 ~ "Veteran",
              TRUE ~ "Non-veteran")) %>%
          group_by(col_b) %>%
          summarise(individuals = n_distinct(PersonalID,
                                             na.rm = TRUE)),
        by = "col_b") %>%
      adorn_totals() %>% 
      mutate(col_a = case_when(
        col_b == "Veteran" ~ "26e. Veteran Status (adults only)"), 
        .before = col_b)
    
    # Q26f
    Q26f_detail <- general_detail %>%
      filter(active_and_enrolled) %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDateAdj) %>%
      left_join(Disabilities %>%
                  select(-PersonalID) %>%
                  filter(InformationDate <= report_end_date &
                           DisabilityType == 10),
                join_by(EnrollmentID,
                        EntryDate <= InformationDate,
                        ExitDateAdj >= InformationDate)) %>%
      group_by(EnrollmentID) %>%
      arrange(desc(InformationDate)) %>%
      slice(1L) %>%
      ungroup()
    
    cooccurring_disorder_table <- as.data.frame(c("Co-occurring substance use disorder",
                                                  "No co-occurring substance use disorder",
                                                  "Unknown"))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26f_detail %>%
                  mutate(col_b = case_when(
                    is.na(DisabilityResponse) |
                      DisabilityResponse > 3 ~ "Unknown",
                    DisabilityResponse == 0 ~ "No co-occurring substance use disorder",
                    TRUE ~ "Co-occurring substance use disorder")) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>% 
      mutate(col_a = case_when(
        col_b == "Co-occurring substance use disorder" ~ "26f. Co-occurring disorder"), 
        .before = col_b)
    
    # Q26g
    Q26g_detail <- general_detail %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDateAdj) %>%
      left_join(IncomeBenefits %>%
                  select(EnrollmentID, InformationDate, ConnectionWithSOAR) %>%
                  filter(InformationDate <= report_end_date),
                join_by(EnrollmentID,
                        EntryDate <= InformationDate,
                        ExitDateAdj >= InformationDate)) %>%
      group_by(EnrollmentID) %>%
      arrange(desc(InformationDate)) %>%
      slice(1L) %>%
      ungroup()
    
    soar_table <- as.data.frame(names_for_1.8)  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26g_detail %>%
                  mutate(
                    col_b = convert_list_1.8(ConnectionWithSOAR)) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>% 
      mutate(col_a = case_when(
        col_b == "Yes" ~ "26g. Connection with SOAR"), 
        .before = col_b)
    
    # Q26h
    Q26h_j_detail <- general_detail %>%
      select(HouseholdID, EnrollmentID, PersonalID, EntryDate, ExitDate, 
             DisablingCondition, LivingSituation, LengthOfStay, 
             LOSUnderThreshold, PreviousStreetESSH, DateToStreetESSH, 
             TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears) %>%
      add_chronicity_data()
    
    living_situation_counts <- Q26h_j_detail %>%
      mutate(LivingSituation = if_else(
        is.na(LivingSituation), 99, LivingSituation)) %>%
      group_by(LivingSituation) %>%
      summarise(individuals = n_distinct(PersonalID, na.rm = TRUE)) %>%
      ungroup() 
    
    for(residence_type in c("Homeless", "Institutional", "Temporary",
                            "Permanent", "Other")) {
      residences_to_include <- ResidenceUses %>%
        filter(APR_LocationGroup == residence_type &
                 !is.na(LocationDescription) &
                 PriorLivingSituation) %>%
        # arrange(APR_LocationOrder) %>%
        mutate(
          LocationDescription = case_when(
            Location == 9 ~ "Client prefers not to answer",
            TRUE ~ LocationDescription))
      
      group_of_residences <- residences_to_include %>%
        left_join(living_situation_counts,
                  by = c("Location" = "LivingSituation")) %>%
        mutate(APR_LocationOrder = case_when(
          Location == 99 ~ 39,
          Location == 9 ~ 38,
          Location == 205 ~ 12,
          Location == 204 ~ 13,
          TRUE ~ APR_LocationOrder)) %>%
        arrange(APR_LocationOrder) %>%
        select(LocationDescription, individuals) %>%
        ifnull(., 0) %>%
        mutate(LocationDescription = case_when(
          LocationDescription == "Total" ~ paste(str_to_title(residence_type), "Subtotal"),
          TRUE ~ LocationDescription))
      
      group_title_row <- group_of_residences[0,]
      group_title_row[1,1] <- paste(residence_type, "Situations")
      
      group_of_residences <- rbind(group_title_row, group_of_residences)
      
      if (residence_type != "Homeless") {
        living_situation_table <- living_situation_table %>%
          union(group_of_residences)
      } else {
        living_situation_table <- group_of_residences
      }
    }
    
    living_situation_table <- living_situation_table %>%
      adorn_totals() %>% 
      rename(col_b = LocationDescription) %>%
      mutate(col_a = case_when(
        col_b == "Homeless Situations" ~ "26h. Prior Living Situation"), 
        .before = col_b)
    
    # Q26i
    lengths_of_stay <- c("One night or less", "Two to six nights",
                         "One week or more, but less than one month",
                         "One month or more, but less than 90 days",
                         "90 days or more, but less than one year",
                         "One year or longer")
    
    length_of_stay_table <- as.data.frame(c(lengths_of_stay,
                                            names_for_1.8[3:5]))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(
        Q26h_j_detail %>%
          filter(LivingSituation %in% 100:199) %>%
          mutate(
            col_b = case_when(
              is.na(LengthOfStay) |
                LengthOfStay %in% c(8, 9, 99) ~ convert_list_1.8(LengthOfStay),
              LengthOfStay == 10 ~ lengths_of_stay[1],
              LengthOfStay == 11 ~ lengths_of_stay[2],
              LengthOfStay == 2 ~ lengths_of_stay[3],
              LengthOfStay == 3 ~ lengths_of_stay[4],
              LengthOfStay == 4 ~ lengths_of_stay[5],
              TRUE ~ lengths_of_stay[6])) %>%
          group_by(col_b) %>%
          summarise(individuals = n_distinct(PersonalID,
                                             na.rm = TRUE)),
        by = "col_b") %>%
      adorn_totals() %>% 
      mutate(col_a = case_when(
        col_b == "One night or less" ~ "26i. Length of stay in prior living situation (emergency shelter or place not meant for human habitation only)"), 
        .before = col_b)
    
    # Q26j
    
    chronicity_table <- as.data.frame(c("Yes", "No", "Unknown"))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26h_j_detail %>%
                  mutate(col_b = case_when(
                    chronic == "Y" ~ "Yes",
                    chronic == "N" ~ "No", 
                    TRUE ~ "Unknown")) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>% 
      mutate(col_a = case_when(
        col_b == "Yes" ~ "26j. Chronically homeless (at project start)"), 
        .before = col_b)
    
    # Q26k
    Q26k_detail <- general_detail %>%
      filter(!is.na(age) &
               age >= 18) %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDateAdj) %>%
      left_join(HealthAndDV %>%
                  select(EnrollmentID, InformationDate, DomesticViolenceSurvivor) %>%
                  filter(InformationDate <= report_end_date),
                join_by(EnrollmentID,
                        EntryDate <= InformationDate,
                        ExitDateAdj >= InformationDate)) %>%
      group_by(EnrollmentID) %>%
      arrange(desc(InformationDate)) %>%
      slice(1L) %>%
      ungroup()
    
    dv_table <- as.data.frame(names_for_1.8)  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26k_detail %>%
                  mutate(
                    col_b = convert_list_1.8(DomesticViolenceSurvivor)) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>% 
      mutate(col_a = case_when(
        col_b == "Yes" ~ "26k. Survivor of Domestic Violence (adults only)"), 
        .before = col_b)
    
    # combine all
    Q26 <- gender_table %>%
      rbind(age_table) %>%
      rbind(race_table) %>%
      rbind(veteran_table) %>%
      rbind(cooccurring_disorder_table) %>%
      rbind(soar_table) %>%
      rbind(living_situation_table) %>%
      rbind(length_of_stay_table) %>%
      rbind(chronicity_table) %>%
      rbind(dv_table) %>%
      mutate(
        individuals = ifnull(individuals, 0))  %>%
      `colnames<-`(c(" ", "  ",
                     "Of those with an active, enrolled PATH status during this reporting period, how many individuals are in each of the following categories?"))
  }
  
  if (generate_new_kits) {
    
    PATH_files <- c("Q8_16", "Q17", "Q18", "Q19_24", "Q25", "Q26")
    PATH_details <- ls()[sapply(ls(),function(t) is.data.frame(get(t))) &
                           str_detect(ls(), "_detail")]
    organization_included <- Organization$OrganizationName[Organization$OrganizationID == organization]
    
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
      
    if (!dir.exists(paste0(folder_name, "/Reports"))) {
      dir.create(paste0(folder_name, "/Reports"))
    }
    
    write_csvs_for(Project$ProjectID[Project$OrganizationID == organization], 
                   zip_title = Organization$OrganizationName[Organization$OrganizationID == organization],
                   write_to <- paste0(folder_name, "/HMIS CSVs"))
    
    for (question in PATH_files) {
      if (exists(question)) {
        
        to_write <- get(question) %>%
          set_hud_format(., ignore_row_names = TRUE) %>% 
          ifnull(., 0) 
        
        to_write[is.na(to_write)] <- ""
        
        write_csv(to_write, file.path(paste0("created_files/", question, ".csv")))
      } else {
        missing_files <- c(missing_files, paste("PATH -", organization_included, "-", question))
      }
    }
    
    for (detail in PATH_details) {
      write.csv(get(paste0(detail)),
                file.path(paste0("created_files_2/", detail, ".csv")),
                row.names = FALSE)
    }
    
    # general_wd <- getwd()
    general_wd <- "C:/Users/57695/OneDrive - ICF/NHDAP/General/Data Lab/Test Kits/DataLab"
    setwd(paste0(general_wd, "/", folder_name, "/Reports"))
    
    archive_write_dir(paste0("PATH - ", organization_included, " (A).zip"),
                      paste0(general_wd, "/created_files"))
    archive_write_dir(paste0("PATH - ", organization_included, " (D).zip"),
                      paste0(general_wd, "/created_files_2"))
    
    setwd(general_wd)
    
    unlink(paste0(getwd(), "/created_files/*"))
    unlink(paste0(getwd(), "/created_files_2/*"))
  }
  
}
