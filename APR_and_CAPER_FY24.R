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

generate_new_kits <- TRUE

# this bracket will run everything; use with care!
{
  source("DataLab.R")
  
  # used for building
  {
    project_list <- c(
      # 1362#,	#"DataLab - ES-EE ESG I",
      # 93#,	#"DataLab - ES-NbN ESG",
      # 1409#,	"DataLab - HP ESG",
      # 780#,	#"DataLab - PSH CoC I",
      1428,	#"DataLab - RRH CoC I",
      1495#,	#"DataLab - RRH CoC II",
      # 1060#,	#"DataLab - RRH ESG I",
      # 1647#,	#"DataLab - SO CoC",
      # 1419,	#"DataLab - SO ESG",
      # 1615#,	#"DataLab - SSO CoC",
      # 388#,	#"DataLab - TH CoC",
      # 340,	#"DataLab - TH ESG"
    )
  }
  
  # used for running all reports
  {
    full_project_list <- c(Project$ProjectID[Project$ProjectID %in% Funder$ProjectID[Funder$Funder %in% 1:11]])
    full_project_list <- full_project_list[full_project_list %nin% c(1647, 340)]
  }
  
  items_to_keep <- c("items_to_keep", ls())
  
  # loop for running all
  for(project_id in full_project_list) {
    project_list <- c(project_id)
    
    # run all table creation logic, including questions
    {
      # removes exits for prevention projects for Q23c of the APR/CAPER,
      # remove upon finalization of guidance
      Exit <- Exit %>%
        filter(EnrollmentID %nin% Enrollment$EnrollmentID[Enrollment$ProjectID %in% Project$ProjectID[Project$ProjectType == 12]]) %>%
        rename(exit_DateCreated = DateCreated)
      
      Enrollment <- Enrollment %>%
        rename(enroll_DateCreated = DateCreated)
      
      all_program_enrollments <- Enrollment %>%
        filter(ProjectID %in% project_list) %>%
        left_join(Project %>%
                    select(ProjectID, ProjectType, ProjectName),
                  by = "ProjectID") %>%
        left_join(Exit %>%
                    select(-PersonalID),
                  by = "EnrollmentID")
      
      recent_program_enrollment <- all_program_enrollments %>%
        group_by(PersonalID) %>%
        arrange(desc(EntryDate)) %>%
        slice(1L) %>%
        ungroup() 
      
      # get additional client information (age for reporting)
      client_plus <- add_client_info(recent_program_enrollment) 
      
      annual_assessment_dates <- Enrollment %>%
        group_by(HouseholdID) %>%
        mutate(start_for_annual = max(EntryDate[RelationshipToHoH == 1]),
               years_in_project = trunc((start_for_annual %--% report_end_date) / years(1))) %>%
        filter(years_in_project > 0) %>%
        mutate(annual_due = start_for_annual %m+% years(years_in_project)) %>%
        select(HouseholdID, annual_due) %>%
        distinct()
      
      household_info <- get_household_info(all_program_enrollments,
                                           return_type = "household")
      
      recent_program_enrollment <- recent_program_enrollment %>%
        left_join(client_plus, by = "PersonalID") %>%
        left_join(household_info, by = "HouseholdID") %>%
        left_join(chronicity_data, by = "EnrollmentID")
      
      # Q4a
      {
        Q4a_detail <- "See Q5a_detail.csv"
        
        Q4a <- program_information_table(project_list,
                                         recent_program_enrollment)
        }
      
      # Q5
      # Q5a
      {
        recent_program_enrollment_dq <- recent_program_enrollment %>%
          filter(ProjectType != 4 |
                   (!is.na(DateOfEngagement) &
                      DateOfEngagement <= report_end_date))
        
        Q5a_detail <- recent_program_enrollment %>%
          add_length_of_time_groups(., EntryDate,
                                    ifnull(ExitDate, ymd(report_end_date) + days(1)),
                                    "APR") %>%
          select(c(all_of(standard_detail_columns),
                   all_of(demographic_detail_columns),
                          number_of_days)) %>%
          mutate(IncludedInDQ = EnrollmentID %in% recent_program_enrollment_dq$EnrollmentID) 
        
        Q5a <- create_summary_table(recent_program_enrollment_dq, "Count.of.Clients.for.DQ") %>%
          left_join(create_summary_table(recent_program_enrollment, "Count.of.Clients"), 
                    by = "Group")
      }
      
      
      # Q6
      # Q6a
      {
        Q6a_data <- create_dq_Q1(recent_program_enrollment_dq)
        Q6a <- Q6a_data[[1]]
        Q6a_detail <- Q6a_data[[2]]
      }
      
      # Q6b 
      {
        
        Q6b_earlier_enrollment <- recent_program_enrollment_dq %>%
          left_join(Enrollment %>%
                      left_join(Exit %>%
                                  select(EnrollmentID, ExitDate),
                                by = "EnrollmentID") %>%
                      `colnames<-`(paste0("Earlier", c(colnames(Enrollment), "ExitDate"))), 
                    by = c("PersonalID" = "EarlierPersonalID", 
                           "ProjectID" = "EarlierProjectID"),
                    multiple = "all") %>%
          filter(EntryDate > EarlierEntryDate &
                   EntryDate < EarlierExitDate)
        
        Q6b_hoh_count <- all_program_enrollments %>%
          group_by(HouseholdID) %>%
          summarise(hohs = uniqueN(PersonalID[RelationshipToHoH == 1])) %>%
          filter(hohs != 1)
        
        # accounts for test kit CoC codes, remove XX- values for real data
        valid_cocs <- c(valid_cocs, "XX-500", "XX-501")
        
        recent_disability_date <- Disabilities %>%
          group_by(EnrollmentID) %>%
          summarise(InformationDate = max(InformationDate)) %>%
          ungroup()
          
        recent_disability_check <- Disabilities %>%
          inner_join(recent_disability_date, 
                     by = colnames(recent_disability_date)) %>%
          filter((DisabilityResponse == 1 |
                    (DisabilityType == 10 &
                       DisabilityResponse %in% c(2, 3))) &
                   (DisabilityType %in% c(6, 8) |
                      (DisabilityType %in% c(5, 7, 9, 10) &
                         IndefiniteAndImpairs == 1)))
        
        Q6b_detail <- recent_program_enrollment_dq %>%
          left_join(EnrollmentCoC %>%
                      filter(DataCollectionStage == 1) %>%
                      select(EnrollmentID, CoCCode),
                    by = "EnrollmentID") %>%
          select(c(all_of(standard_detail_columns),
                   all_of(demographic_detail_columns),
                   "CoCCode")) %>%
          mutate(CoC_Valid = CoCCode %in% valid_cocs,
                 Disability_Check = EnrollmentID %in% recent_disability_check$EnrollmentID)
        
        Q6b <- Q6b_detail %>%
          mutate(Veteran.Status.3.07 = (VeteranStatus %in% c(8, 9, 99) &
                                          age_group == "Adults") |
                   (age_group == "Children" &
                      VeteranStatus == 1),
                 Project.Start.Date.3.10 = EnrollmentID %in% Q6b_earlier_enrollment$EnrollmentID,
                 Relationship.to.Head.of.Household.3.15 = is.na(RelationshipToHoH) |
                   RelationshipToHoH %nin% 1:5 |
                   HouseholdID %in% Q6b_hoh_count$HouseholdID,
                 Client.Location.3.16 = RelationshipToHoH == 1 & !CoC_Valid,
                 Disabling.Condition.3.08 = DisablingCondition %in% c(8, 9, 99) |
                   is.na(DisablingCondition) |
                   (DisablingCondition == 0 & Disability_Check)) %>%
          summarise(Veteran.Status.3.07 = n_distinct(PersonalID[Veteran.Status.3.07], 
                                                     na.rm = TRUE),
                    Project.Start.Date.3.10 = n_distinct(PersonalID[Project.Start.Date.3.10], 
                                                         na.rm = TRUE),
                    Relationship.to.Head.of.Household.3.15 = n_distinct(PersonalID[Relationship.to.Head.of.Household.3.15], 
                                                                        na.rm = TRUE),
                    Client.Location.3.16 = n_distinct(PersonalID[Client.Location.3.16], 
                                                      na.rm = TRUE),
                    Disabling.Condition.3.08 = n_distinct(PersonalID[Disabling.Condition.3.08], 
                                                          na.rm = TRUE),
          ) %>% 
          mutate(rowname = "Error.Count") %>% 
          pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
          pivot_wider(names_from = "rowname", values_from = "values") %>%
          mutate(Percent.of.Error.Count = decimal_format(
            case_when(
              Group == "Veteran.Status.3.07" ~ Error.Count / Q5a$Count.of.Clients.for.DQ[2],
              Group == "Client.Location.3.16" ~ Error.Count / (Q5a$Count.of.Clients.for.DQ[14] + Q5a$Count.of.Clients.for.DQ[15]),
              TRUE ~ Error.Count / Q5a$Count.of.Clients.for.DQ[1]), 4)
          )
        
      }
      
      # Q6c
      {
        income_sources <- IncomeBenefits %>%
          mutate(across(c(Earned, Unemployment, SSI, SSDI,
                          VADisabilityService, VADisabilityNonService,
                          PrivateDisability, WorkersComp, TANF, GA,
                          SocSecRetirement, Pension, ChildSupport,
                          Alimony, OtherIncomeSource), ~ ifnull(., 0)),
                 number_of_sources = 
                   (Earned == 1) + (Unemployment == 1) +
                   (SSI == 1) + (SSDI == 1) +
                   (VADisabilityService == 1) + (VADisabilityNonService == 1) +
                   (PrivateDisability == 1) + (WorkersComp == 1) +
                   (TANF == 1) + (GA == 1) +
                   (SocSecRetirement == 1) + (Pension == 1) +
                   (ChildSupport == 1) + (Alimony == 1) +
                   (OtherIncomeSource == 1)) %>%
          select(EnrollmentID, DataCollectionStage, InformationDate, 
                 IncomeFromAnySource, number_of_sources, IncomeBenefitsID)
        
        income_annual <- recent_program_enrollment %>%
          get_annual_id(., IncomeBenefits, "IncomeBenefitsID") %>%
          left_join(income_sources %>%
                      select(colnames(income_sources)[colnames(income_sources) %nin% colnames(recent_program_enrollment)]), 
                    by = "IncomeBenefitsID") %>%
          rename(annual_number_of_sources = number_of_sources,
                 annual_IncomeFromAnySource = IncomeFromAnySource) %>%
          select(EnrollmentID, annual_IncomeFromAnySource, annual_number_of_sources)
        
        
        Q6c_detail <- recent_program_enrollment_dq %>%
          select(c(all_of(standard_detail_columns), "age_group", 
                   "Destination")) %>%
          left_join(annual_assessment_dates, by = "HouseholdID") %>%
          left_join(income_sources %>%
                      filter(DataCollectionStage == 1) %>%
                      select(-DataCollectionStage) %>%
                      rename(enroll_number_of_sources = number_of_sources,
                             enroll_IncomeFromAnySource = IncomeFromAnySource),
                    by = c("EnrollmentID" = "EnrollmentID",
                           "EntryDate" = "InformationDate")) %>%
          left_join(income_sources %>%
                      filter(DataCollectionStage == 3) %>%
                      select(-DataCollectionStage) %>%
                      rename(exit_number_of_sources = number_of_sources,
                             exit_IncomeFromAnySource = IncomeFromAnySource),
                    by = c("EnrollmentID" = "EnrollmentID",
                           "ExitDate" = "InformationDate")) %>%
          left_join(income_annual, by = "EnrollmentID") %>%
          mutate(Destination.3.12 = !is.na(ExitDate) &
                   (is.na(Destination) |
                      Destination %in% c(8, 9, 99, 30)),
                 Income.and.Sources.4.02.at.Start = (RelationshipToHoH == 1 |
                                                       age_group == "Adults") &
                   (enroll_IncomeFromAnySource %in% c(8, 9, 99) |
                      is.na(enroll_IncomeFromAnySource) |
                      (enroll_IncomeFromAnySource == 0 &
                         !is.na(enroll_number_of_sources) &
                         enroll_number_of_sources > 0) |
                      (enroll_IncomeFromAnySource == 1 &
                         (is.na(enroll_number_of_sources) |
                            enroll_number_of_sources == 0))),
                 Income.and.Sources.4.02.at.Annual.Assessment = (RelationshipToHoH == 1 |
                                                                   age_group == "Adults") &
                   !is.na(annual_due) &
                   is.na(ExitDate) &
                   (annual_IncomeFromAnySource %in% c(8, 9, 99) |
                      is.na(annual_IncomeFromAnySource) |
                      (annual_IncomeFromAnySource == 0 &
                         !is.na(annual_number_of_sources) &
                         annual_number_of_sources > 0) |
                      (annual_IncomeFromAnySource == 1 &
                         (is.na(annual_number_of_sources) |
                            annual_number_of_sources == 0))),
                 Income.and.Sources.4.02.at.Exit = (RelationshipToHoH == 1 |
                                                      age_group == "Adults") &
                   !is.na(ExitDate) &
                   (exit_IncomeFromAnySource %in% c(8, 9, 99) |
                      is.na(exit_IncomeFromAnySource) |
                      (exit_IncomeFromAnySource == 0 &
                         !is.na(exit_number_of_sources) &
                         exit_number_of_sources > 0) |
                      (exit_IncomeFromAnySource == 1 &
                         (is.na(exit_number_of_sources) |
                            exit_number_of_sources == 0)))) 
        Q6c <- Q6c_detail %>%
          summarise(Destination.3.12 = n_distinct(PersonalID[Destination.3.12], 
                                                  na.rm = TRUE),
                    Income.and.Sources.4.02.at.Start = n_distinct(PersonalID[Income.and.Sources.4.02.at.Start], 
                                                                  na.rm = TRUE),
                    Income.and.Sources.4.02.at.Annual.Assessment = n_distinct(PersonalID[Income.and.Sources.4.02.at.Annual.Assessment],
                                                                              na.rm = TRUE),
                    Income.and.Sources.4.02.at.Exit = n_distinct(PersonalID[Income.and.Sources.4.02.at.Exit],
                                                                 na.rm = TRUE)
          ) %>% 
          mutate(rowname = "Error.Count") %>% 
          pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
          pivot_wider(names_from = "rowname", values_from = "values") %>%
          mutate(Percent.of.Error.Count = decimal_format(
            case_when(
              Group == "Destination.3.12" ~ Error.Count / Q5a$Count.of.Clients.for.DQ[5],
              Group == "Income.and.Sources.4.02.at.Start" ~ Error.Count / (Q5a$Count.of.Clients.for.DQ[2] + Q5a$Count.of.Clients.for.DQ[15]),
              Group == "Income.and.Sources.4.02.at.Annual.Assessment" ~ Error.Count / Q5a$Count.of.Clients.for.DQ[16],
              Group == "Income.and.Sources.4.02.at.Exit" ~ Error.Count / Q5a$Count.of.Clients.for.DQ[7]), 4)
          ) %>%
          ifnull(., 0)
      }
      
      # Q6d
      {
        Entering.into.project.type <- c("ES.SH.Street.Outreach", "TH", "PH.all")
        
        Q6d_detail <- recent_program_enrollment_dq %>%
          filter(EntryDate >= mdy("10/1/2016") &
                   ProjectType %in% c(0, 1, 2, 3, 4, 8, 9, 10, 13)) %>%
          keep_adults_and_hoh_only() %>%
          select(c("ProjectType", all_of(standard_detail_columns), 
                   all_of(lot_homeless_detail_columns))) %>%
          mutate(Entering.into.project.type = case_when(
            ProjectType %in% c(0, 1, 4, 8) ~ "ES.SH.Street.Outreach",
            ProjectType == 2 ~ "TH",
            ProjectType %in% c(3, 9, 10, 13) ~ "PH.all"),
            missing_institution = Entering.into.project.type != "ES.SH.Street.Outreach" &
              LivingSituation %in% 200:299 &
              (LengthOfStay %in% c(8, 9, 99) |
                 is.na(LengthOfStay)),
            missing_housing = Entering.into.project.type != "ES.SH.Street.Outreach" &
              (LivingSituation %in% c(0:99, 300:499) |
                 is.na(LivingSituation)) &
              (LengthOfStay %in% c(8, 9, 99) |
                 is.na(LengthOfStay)),
            include_for_EFG = Entering.into.project.type == "ES.SH.Street.Outreach" |
              LivingSituation %in% 100:199 |
              (PreviousStreetESSH == 1 & 
                 ((LivingSituation %in% 200:299 &
                     LengthOfStay %in% c(10, 11, 2, 3)) |
                    ((LivingSituation %in% c(0:99, 300:499) |
                        is.na(LivingSituation)) &
                       LengthOfStay %in% c(10, 11)))),
            missing_date = include_for_EFG &
              is.na(DateToStreetESSH),
            missing_times = include_for_EFG &
              (is.na(TimesHomelessPastThreeYears) |
                 TimesHomelessPastThreeYears %in% c(8, 9, 99)),
            missing_months = include_for_EFG &
              (is.na(MonthsHomelessPastThreeYears) |
                 MonthsHomelessPastThreeYears %in% c(8, 9, 99))
          ) 
        
        Q6d_by_program <- Q6d_detail %>%
          group_by(Entering.into.project.type) %>%
          summarise(Count.of.total.records = n_distinct(PersonalID, na.rm = TRUE),
                    Missing.time.in.institution.3.917.2 = n_distinct(PersonalID[missing_institution], 
                                                                     na.rm = TRUE),
                    Missing.time.in.housing.3.917.2 = n_distinct(PersonalID[missing_housing], 
                                                                 na.rm = TRUE),
                    Approximate.Date.started.3.917.3.Missing = n_distinct(PersonalID[missing_date], 
                                                                          na.rm = TRUE),
                    Number.of.times.3.917.4.DK.R.missing = n_distinct(PersonalID[missing_times], 
                                                                      na.rm = TRUE),
                    Number.of.months.3.917.5.DK.R.missing = n_distinct(PersonalID[missing_months], 
                                                                       na.rm = TRUE),
                    all_errors = n_distinct(PersonalID[missing_institution |
                                                         missing_housing |
                                                         missing_date |
                                                         missing_times |
                                                         missing_months],
                                            na.rm = TRUE)) %>% 
          mutate(Percent.of.records.unable.to.calculate = all_errors / Count.of.total.records) %>%
          select(-all_errors)
        
        Q6d_data_summary <- Q6d_detail %>%
          summarise(Entering.into.project.type = "Total",
                    Count.of.total.records = n_distinct(PersonalID, na.rm = TRUE),
                    all_errors = n_distinct(PersonalID[missing_institution |
                                                         missing_housing |
                                                         missing_date |
                                                         missing_times |
                                                         missing_months], 
                                            na.rm = TRUE)) %>%
          mutate(Percent.of.records.unable.to.calculate = all_errors / Count.of.total.records) %>%
          select(-all_errors)
        
        Q6d <- as.data.frame(Entering.into.project.type) %>%
          left_join(Q6d_by_program,
                    by = "Entering.into.project.type") %>%
          ifnull(., 0) %>%
          full_join(Q6d_data_summary,
                    by = c("Entering.into.project.type",
                           "Count.of.total.records",
                           "Percent.of.records.unable.to.calculate")) %>%
          mutate(Percent.of.records.unable.to.calculate = decimal_format(
            Percent.of.records.unable.to.calculate, 4))
        
        
        Q6d[Q6d$Entering.into.project.type == "ES.SH.Street.Outreach", 
            c("Missing.time.in.institution.3.917.2",
              "Missing.time.in.housing.3.917.2")] <- NA
      }
      
      # Q6e
      {
        Q6e_detail <- recent_program_enrollment_dq %>%
          select(c(all_of(standard_detail_columns)), "enroll_DateCreated",
                 "exit_DateCreated") %>%
          mutate(days_for_entry = case_when(
            EntryDate >= report_start_date &
              EntryDate <= report_end_date ~ floor(
                interval(EntryDate, floor_date(enroll_DateCreated, "day")) / days(1))),
            TimeForEnrollmentEntry = case_when(
              days_for_entry < 0 ~ "Error",
              days_for_entry == 0 ~ "0 days",
              days_for_entry <= 3 ~ "1-3 days",
              days_for_entry <= 6 ~ "4-6 days",
              days_for_entry <= 10 ~ "7-10 days",
              !is.na(days_for_entry) ~ "11+ days"),
            days_for_exit = case_when(!is.na(ExitDate) ~ floor(
              interval(ExitDate, floor_date(exit_DateCreated, "day")) / days(1))),
            TimeForExitEntry = case_when(
              days_for_exit < 0 ~ "Error",
              days_for_exit == 0 ~ "0 days",
              days_for_exit <= 3 ~ "1-3 days",
              days_for_exit <= 6 ~ "4-6 days",
              days_for_exit <= 10 ~ "7-10 days",
              !is.na(days_for_exit) ~ "11+ days"))
        
        Q6e <- data.frame(TimeForEntry = c("0 days", "1-3 days", "4-6 days",
                                           "7-10 days", "11+ days")) %>%
          left_join(Q6e_detail %>%
                      filter(!is.na(TimeForEnrollmentEntry)) %>%
                      group_by(TimeForEnrollmentEntry) %>%
                      summarise(Number.of.Project.Start.Records = n_distinct(PersonalID, na.rm = TRUE)) %>%
                      rename(TimeForEntry = TimeForEnrollmentEntry),
                    by = "TimeForEntry")%>%
          left_join(Q6e_detail %>%
                      filter(!is.na(TimeForExitEntry)) %>%
                      group_by(TimeForExitEntry) %>%
                      summarise(Number.of.Project.Exit.Records = n_distinct(PersonalID, na.rm = TRUE)) %>%
                      rename(TimeForEntry = TimeForExitEntry),
                    by = "TimeForEntry") %>%
          ifnull(., 0)
        
      }
      
      # Q6f
      {
        # programming specs are unclear--most recent for individual or enrollment?
        most_recent_CLS <- CurrentLivingSituation %>%
          filter(InformationDate >= report_start_date &
                   InformationDate <= report_end_date) %>%
          arrange(desc(InformationDate)) %>%
          group_by(EnrollmentID) %>%
          slice(1L) %>%
          ungroup()
        
        most_recent_bed_night <- bed_nights_in_report %>%
          arrange(desc(DateProvided)) %>%
          group_by(EnrollmentID) %>%
          slice(1L) %>%
          ungroup()
        
        Q6f_detail <- recent_program_enrollment_dq %>%
          filter(is.na(ExitDate) &
                   trunc((EntryDate %--% report_end_date) / days(1)) >= 90 &
                   (ProjectType %in% c(1, 4))) %>%
          select(all_of(standard_detail_columns)) %>%
          left_join(most_recent_CLS %>%
                      select(EnrollmentID, InformationDate),
                    by = "EnrollmentID") %>%
          left_join(most_recent_bed_night %>%
                      select(EnrollmentID, DateProvided),
                    by = "EnrollmentID") 
        
        Q6f <- create_inactive_table(recent_program_enrollment_dq,
                                     most_recent_CLS,
                                     "contact") %>%
          union(create_inactive_table(recent_program_enrollment_dq,
                                      most_recent_bed_night,
                                      "bed night")) %>%
          ifnull(., 0)
        
      }
      
      # Q7
      # Q7a
      {
        Q7a_detail <- recent_program_enrollment %>%
          select(all_of(housing_program_detail_columns),
                 all_of(demographic_detail_columns))
        
        Q7a_all <- Q7a_detail %>%
          mutate(client_group = "Total") %>%
          return_household_groups(., client_group, c("Total")) 
        
        Q7a_moved_in <- Q7a_detail %>%
          filter(HoH_HMID <= report_end_date) %>% 
          mutate(client_group = "For PSH & RRH - the total persons served who moved into housing") %>%
          return_household_groups(., client_group, "For PSH & RRH - the total persons served who moved into housing") 
        
        Q7a <- Q7a_detail %>%
          return_household_groups(., age_group, age_groups) %>%
          rename(client_group = age_group) %>%
          union(Q7a_all) %>%
          union(Q7a_moved_in)
        
      }
      
      # Q7b
      {
        year_household_info <- all_program_enrollments %>%
          group_by(HouseholdID) %>%
          mutate(HoH_HMID = suppressWarnings(min(case_when(
            RelationshipToHoH == 1 ~ MoveInDate), na.rm = TRUE))) %>%
          ungroup() %>%
          select(EnrollmentID, HoH_HMID)
        
        pit_dates <- create_pit_dates(report_start_date, report_end_date)
        pit_months <- c("January", "April", "July", "October")
        
        for (pit_month in pit_months) {
          pit_date <- pit_dates[[which(pit_dates$month == pit_month), 2]]
          
          pit_nbn_people <- all_program_enrollments %>%
            inner_join(bed_nights_in_report %>%
                         filter(DateProvided == pit_date) %>%
                         select(-c(PersonalID, EntryDate)),
                       by = "EnrollmentID")
          
          pit_enrollment_detail <- all_program_enrollments %>%
            left_join(client_plus, by = "PersonalID") %>%
            left_join(year_household_info, by = "EnrollmentID") %>%
            select(c("ProjectType", "ProjectID", "ProjectName", "HouseholdID", 
                     "PersonalID", "EnrollmentID", "RelationshipToHoH", 
                     "EntryDate", "HoH_HMID", "ExitDate")) %>%
            filter(EntryDate <= pit_date &
                     (ProjectType %nin% c(3, 13) |
                        (ProjectType %in% c(3, 13) &
                           HoH_HMID <= pit_date)) &
                     (EnrollmentID %in% pit_nbn_people$EnrollmentID |
                        (ProjectID %nin% Project$ProjectID[Project$ProjectType == 1] &
                           (is.na(ExitDate) |
                              (ProjectType %in% c(0, 1, 2, 3, 8, 9, 10, 13) &
                                 ExitDate > pit_date) |
                              (ProjectType %in% c(4, 6, 11) &
                                 ExitDate >= pit_date))))) %>%
            left_join(household_info %>%
                        select(HouseholdID, household_type),
                      by = "HouseholdID") %>%
            mutate(Month = pit_month) 
          
          pit_enrollments <- pit_enrollment_detail %>%
            return_household_groups(., Month, pit_month)
          
          if(pit_month == "January") {
            Q7b_detail <- pit_enrollment_detail
            Q7b <- pit_enrollments
          } else {
            Q7b_detail <- Q7b_detail %>%
              union(pit_enrollment_detail) 
            Q7b <- Q7b %>%
              union(pit_enrollments)
          }
        }
      }
      
      # Q8a
      {
        Q8a_data <- households_served_table(recent_program_enrollment)
        Q8a <- Q8a_data[[1]]
        Q8a_detail <- Q8a_data[[2]]
      }
      
      # Q8b
      {
        for (pit_month in pit_months) {
          pit_date <- pit_dates[[which(pit_dates$month == pit_month), 2]]
          
          pit_nbn_people <- all_program_enrollments %>%
            inner_join(bed_nights_in_report %>%
                         filter(DateProvided == pit_date) %>%
                         select(-c(PersonalID, EntryDate)),
                       by = "EnrollmentID")
          
          pit_hh_enrollment_detail <- all_program_enrollments %>%
            left_join(year_household_info, by = "EnrollmentID") %>%
            filter(EntryDate <= pit_date &
                     (ProjectType %nin% c(3, 13) |
                        (ProjectType %in% c(3, 13) &
                           HoH_HMID <= pit_date)) &
                     (EnrollmentID %in% pit_nbn_people$EnrollmentID |
                        (ProjectID %nin% Project$ProjectID[Project$ProjectType == 1] &
                           (is.na(ExitDate) |
                              (ProjectType %in% c(0, 1, 2, 3, 8, 9, 10, 13) &
                                 ExitDate > pit_date) |
                              (ProjectType %in% c(4, 6, 11) &
                                 ExitDate >= pit_date))))) %>%
            select(HouseholdID) %>%
            distinct() %>%
            left_join(all_program_enrollments %>%
                        filter(RelationshipToHoH == 1) %>%
                        select(ProjectName, HouseholdID, PersonalID, EnrollmentID,
                               RelationshipToHoH, EntryDate, ExitDate),
                      by = "HouseholdID") %>%
            left_join(household_info %>%
                        select(HouseholdID, HoH_HMID, household_type),
                      by = "HouseholdID") %>%
            mutate(Month = pit_month) 
          
          pit_hh_enrollments <- pit_hh_enrollment_detail %>%
            return_household_groups(., Month, pit_month)
          
          if(pit_month == "January") {
            Q8b_detail <- pit_hh_enrollment_detail
            Q8b <- pit_hh_enrollments
          } else {
            Q8b_detail <- Q8b_detail %>%
              union(pit_hh_enrollment_detail)
            Q8b <- Q8b %>%
              union(pit_hh_enrollments)
          }
        }
      }
      
      # Q9a
      {
        # same question as before--is this the most recent CLS for the *person* or the *enrollment*?
        recent_CLS_for_Q9 <- recent_program_enrollment %>%
          left_join(CurrentLivingSituation %>%
                      select(-c(PersonalID, ExportID)) %>%
                      rename(CLS_InformationDate = InformationDate,
                             CLS = CurrentLivingSituation), by = "EnrollmentID") %>%
          filter(CLS_InformationDate >= report_start_date &
                   CLS_InformationDate <= report_end_date &
                   (CLS_InformationDate <= DateOfEngagement |
                      is.na(DateOfEngagement)))
        
        # this one specifies enrollment as directed in the programming specifications
        only_CLS_for_Q9 <- recent_program_enrollment %>%
          left_join(CurrentLivingSituation %>%
                      select(-ExportID) %>%
                      rename(CLS_InformationDate = InformationDate,
                             CLS = CurrentLivingSituation), by = "EnrollmentID") %>%
          filter(CLS_InformationDate >= EntryDate &
                   (CLS_InformationDate <= ExitDate |
                      is.na(ExitDate)) &
                   (CLS_InformationDate <= DateOfEngagement |
                      is.na(DateOfEngagement)) &
                   CLS_InformationDate <= report_end_date) %>%
          select(EnrollmentID, CLS_InformationDate, CLS) 
        
        dates_of_engagement <- recent_program_enrollment %>%
          filter(DateOfEngagement <= report_end_date) %>%
          left_join(only_CLS_for_Q9, by = "EnrollmentID") %>%
          group_by(EnrollmentID) %>%
          mutate(had_CLS = max(DateOfEngagement == CLS_InformationDate), 
                 CLS = 999) %>%
          slice(1) %>%
          ungroup() %>%
          select(-CLS_InformationDate) %>%
          filter(is.na(had_CLS) | had_CLS != 1) %>%
          rename(CLS_InformationDate = DateOfEngagement) %>%
          select(EnrollmentID, CLS_InformationDate, CLS)
        
        all_CLS_for_Q9 <- only_CLS_for_Q9 %>%
          union(dates_of_engagement)
        
        first_CLS_group <- all_CLS_for_Q9 %>%
          arrange(CLS_InformationDate) %>%
          group_by(EnrollmentID) %>%
          slice(1L) %>%
          ungroup %>%
          mutate(CLS_group = case_when(
            CLS %in% 100:199 ~ "LH",
            CLS %in% c(37, 8, 9, 99, 999) ~ "Unknown",
            TRUE ~ "Not LH"
          ))
        
        Q9a_detail <- recent_program_enrollment %>%
          filter((ProjectType %in% c(1, 4))) %>%
          select(c(all_of(standard_detail_columns), "DateOfEngagement")) %>%
          left_join(all_CLS_for_Q9, 
                    by = "EnrollmentID") %>%
          left_join(first_CLS_group %>%
                      mutate(FirstCLS = TRUE), 
                    by = colnames(all_CLS_for_Q9)) %>%
          mutate(engaged_in_report_range = (DateOfEngagement >= report_start_date &
                                              DateOfEngagement <= report_end_date))
        
        Q9a <- recent_program_enrollment %>%
          filter((ProjectType %in% c(1, 4)) &
                   (EnrollmentID %in% recent_CLS_for_Q9$EnrollmentID |
                      (DateOfEngagement >= report_start_date &
                         DateOfEngagement <= report_end_date))) %>%
          keep_adults_and_hoh_only() %>%
          create_contact_table(., first_CLS_group, all_CLS_for_Q9)
      }
      
      # Q9b
      {
        Q9b_detail <- "See Q9a_detail.csv"
        
        Q9b <- recent_program_enrollment %>%
          filter((ProjectType %in% c(1, 4)) &
                   (DateOfEngagement >= report_start_date &
                      DateOfEngagement <= report_end_date)) %>%
          keep_adults_and_hoh_only() %>%
          create_contact_table(., first_CLS_group, all_CLS_for_Q9)
        
        rate_of_engagement <- c("EngagementRate")
        for (column in 2:5) {
          if (Q9a[[which(Q9a$ContactGroup == "Total"), column]] == 0) {
            rate <- 0
          } else {
            rate <- as.numeric(Q9b[[which(Q9b$ContactGroup == "Total"), column]]) / 
              Q9a[[which(Q9a$ContactGroup == "Total"), column]]
          }
          rate_of_engagement <- c(rate_of_engagement, decimal_format(rate, 4))
        }
        
        Q9b <- rbind(Q9b, rate_of_engagement)
        
      }
      
      # Q10a
      ############################
      ### PENDING FINALIZATION ###
      ############################
      {
        Q10a_detail <- recent_program_enrollment %>%
          keep_adults_only() %>%
          select(all_of(standard_detail_columns)) %>%
          left_join(Client %>%
                      select(PersonalID, all_of(names(gender_columns)), GenderNone),
                    by = "PersonalID")

        Q10a <- Q10a_detail %>%
          create_gender_groups(.) %>%
          select(-With.Only.Children)
      }
      
      # Q10b
      {
        Q10b_detail <- recent_program_enrollment %>%
          filter(age_group == "Children") %>%
          select(all_of(standard_detail_columns)) %>%
          left_join(Client %>%
                      select(PersonalID, all_of(names(gender_columns)), GenderNone),
                    by = "PersonalID")
        
        Q10b <- Q10b_detail %>%
          create_gender_groups(.) %>%
          select(-Without.Children)
      }
      
      # Q10c
      {
        Q10c_detail <- recent_program_enrollment %>%
          filter(age_group %in% c("Client.Does.Not.Know.or.Refused", "Data.Not.Collected")) %>%
          select(all_of(standard_detail_columns)) %>%
          left_join(Client %>%
                      select(PersonalID, all_of(names(gender_columns)), GenderNone),
                    by = "PersonalID") 
        
        Q10c <- Q10c_detail %>%
          create_gender_groups(.)
      }
      
      # Q10d
      {
        Q10d_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "age", "detailed_age_group") %>%
          left_join(Client %>%
                      select(PersonalID, all_of(names(gender_columns)), GenderNone),
                    by = "PersonalID") %>%
          mutate(Q10d_age_group = case_when(
            detailed_age_group %in% c("Under 5", "5-12", "13-17") ~ "Under18",
            detailed_age_group %in% c("25-34", "35-44", "45-54", "55-61") ~ "25-61",
            TRUE ~ detailed_age_group)) 
        
        Q10d <- gender_info %>%
          mutate(order = row_number()) %>%
          left_join(Q10d_detail,
                    by = all_of(names(gender_columns)),
                    multiple = "all") %>%
          mutate(across(
            all_of(names(gender_columns)),
            ~ as.numeric(.)),
            gender_count = rowSums(across(all_of(names(gender_columns))),
                                   na.rm = TRUE),
            gender_tabulation = case_when(
              gender_count %in% 1:2 ~ gender_name_list,
              gender_count > 2 ~ "More than 2 Gender Identities Selected",
              GenderNone %in% c(8, 9) ~ "Client Doesn’t Know/Prefers Not to Answer",
              TRUE ~ "Data Not Collected")) %>%
          group_by(gender_tabulation, order) %>%
          summarise(Total = n_distinct(PersonalID, na.rm = TRUE),
                    Under.Age.18 = n_distinct(PersonalID[Q10d_age_group == "Under18"], na.rm = TRUE),
                    Age.18.to.24 = n_distinct(PersonalID[Q10d_age_group == "18-24"], na.rm = TRUE),
                    Age.25.to.61 = n_distinct(PersonalID[Q10d_age_group == "25-61"], na.rm = TRUE),
                    Age.62.and.over = n_distinct(PersonalID[Q10d_age_group == "62+"], na.rm = TRUE),
                    Client.Does.Not.Know.or.Refused = n_distinct(PersonalID[Q10d_age_group == "Client.Does.Not.Know.or.Declined"], na.rm = TRUE),
                    Data.Not.Collected = n_distinct(PersonalID[Q10d_age_group == "Data.Not.Collected"], na.rm = TRUE)) %>%
          ifnull(., 0) %>%
          arrange(order) %>%
          select(-order) %>%
          adorn_totals("row")
        
      }
      
      # Q11
      {
        Q11_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "age", "detailed_age_group") %>%
          left_join(Client %>%
                      select(PersonalID, DOB),
                    by = "PersonalID")
        
        Q11 <- Q11_detail %>%
          create_age_groups(.)
      }
      
      # Q12a
      {
        Q12a_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "race_combined") %>%
          left_join(Client %>%
                      select(PersonalID, AmIndAKNative, Asian, BlackAfAmerican,
                             NativeHIPacific, White, RaceNone),
                    by = "PersonalID")
        
        Q12a <- Q12a_detail %>%
          return_household_groups(., race_combined, race_list) %>%
          adorn_totals("row")
      }
      
      # Q12b
      {
        Q12b_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns)) %>%
          left_join(Client %>%
                      select(PersonalID, Ethnicity), by = "PersonalID") %>%
          mutate(display_ethnicity = case_when(Ethnicity == 1 ~ "Hispanic/Latin(a)(o)(x)",
                                               Ethnicity == 0 ~ "Non-Hispanic/Non-Latin(a)(o)(x)",
                                               Ethnicity %in% c(8, 9) ~ "Client.Does.Not.Know.or.Refused",
                                               TRUE ~ "Data.Not.Collected")) 
        
        Q12b <- Q12b_detail %>%
          return_household_groups(., display_ethnicity, ethnicity_list) %>%
          adorn_totals("row")
      }
      
      # Q13a1
      {
        Q13a1_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "age_group", 
                 "new_veteran_status", "chronic") %>%
          inner_join(disability_table %>%
                       filter(DataCollectionStage == 1), by ="EnrollmentID",
                     multiple = "all")
        
        Q13a1 <- Q13a1_detail %>%
          return_household_groups(., disability_name, disability_list,
                                  split_by_age = TRUE)
      }
      
      # Q13b1
      {
        Q13b1_detail<- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "age_group", 
                 "new_veteran_status", "chronic") %>%
          filter(!is.na(ExitDate)) %>%
          inner_join(disability_table %>%
                       filter(DataCollectionStage == 3), by ="EnrollmentID",
                     multiple = "all")
        
        Q13b1 <- Q13b1_detail %>%
          return_household_groups(., disability_name, disability_list,
                                  split_by_age = TRUE)
      }
      
      # Q13c1
      {
        Q13c1_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "age_group", 
                 "new_veteran_status", "chronic") %>%
          filter(is.na(ExitDate)) %>%
          inner_join(disability_table %>%
                       filter(InformationDate <= report_end_date &
                                DataCollectionStage %in% c(1, 2, 5)) %>%
                       group_by(EnrollmentID) %>%
                       mutate(last_date = max(InformationDate)) %>%
                       ungroup() %>%
                       select(EnrollmentID, last_date) %>%
                       distinct() %>%
                       left_join(disability_table, 
                                 by = c("EnrollmentID", "last_date" = "InformationDate"),
                                 multiple = "all"), 
                     by ="EnrollmentID",
                     multiple = "all")
        
        Q13c1 <- Q13c1_detail %>%
          return_household_groups(., disability_name, disability_list,
                                  split_by_age = TRUE)
      }
      
      # Q13a2
      {
        Q13a2_detail <- "See Q13a1_detail.csv"
        Q13a2 <- recent_program_enrollment %>%
          left_join(Q13a1_detail %>%
                      # earlier data lab logic did not account for the following step
                      group_by(PersonalID) %>%
                      summarise(disability_count = sum(disabilities)) %>%
                      ungroup(), 
                    by = "PersonalID") %>%
          condition_count_groups() %>%
          return_household_groups(., disability_count_group, disability_count_group_list,
                                  split_by_age = TRUE) %>%
          adorn_totals("row")
      }
      
      # Q13b2
      {
        Q13b2_detail <- "See Q13b1_detail.csv"
        Q13b2 <- recent_program_enrollment %>%
          filter(!is.na(ExitDate)) %>%
          left_join(Q13b1_detail %>%
                      # earlier data lab logic did not account for the following step
                      group_by(PersonalID) %>%
                      summarise(disability_count = sum(disabilities)) %>%
                      ungroup(), 
                    by = "PersonalID") %>%
          condition_count_groups() %>%
          return_household_groups(., disability_count_group, disability_count_group_list,
                                  split_by_age = TRUE) %>%
          adorn_totals("row")
      }
      
      # Q13c2
      {
        Q13c2_detail <- "See Q13c1_detail.csv"
        Q13c2 <- recent_program_enrollment %>%
          filter(is.na(ExitDate)) %>%
          left_join(Q13c1_detail %>%
                      # earlier data lab logic did not account for the following step
                      group_by(PersonalID) %>%
                      summarise(disability_count = sum(disabilities)) %>%
                      ungroup(), 
                    by = "PersonalID") %>%
          condition_count_groups() %>%
          return_household_groups(., disability_count_group, disability_count_group_list,
                                  split_by_age = TRUE) %>%
          adorn_totals("row")
      }
      
      # Q14a
      {
        Q14 <- recent_program_enrollment %>%
          keep_adults_and_hoh_only() %>%
          left_join(HealthAndDV %>%
                      filter(InformationDate <= report_end_date) %>%
                      arrange(desc(InformationDate)) %>%
                      group_by(EnrollmentID) %>%
                      slice(1L) %>%
                      ungroup() %>%
                      select(EnrollmentID, DomesticViolenceVictim, CurrentlyFleeing),
                    by = "EnrollmentID")
        
        Q14a_detail <- Q14 %>%
          select(all_of(standard_detail_columns), DomesticViolenceVictim) %>%
          mutate(dv_experience = case_when(DomesticViolenceVictim == 1 ~ "Yes",
                                           DomesticViolenceVictim == 0 ~ "No",
                                           DomesticViolenceVictim %in% c(8, 9) ~ "Client.Does.Not.Know.or.Refused",
                                           TRUE ~ "Data.Not.Collected")) 
        
        Q14a <- Q14a_detail %>%
          return_household_groups(., dv_experience, y_n_dkr_dnc_list) %>%
          adorn_totals("row")
      }
      
      # Q14b
      {
        Q14b_detail <- Q14 %>%
          select(all_of(standard_detail_columns), DomesticViolenceVictim,
                 CurrentlyFleeing) %>%
          filter(DomesticViolenceVictim == 1) %>%
          mutate(currently_fleeing = case_when(CurrentlyFleeing == 1 ~ "Yes",
                                               CurrentlyFleeing == 0 ~ "No",
                                               CurrentlyFleeing %in% c(8, 9) ~ "Client.Does.Not.Know.or.Refused",
                                               TRUE ~ "Data.Not.Collected")) 
        Q14b <- Q14b_detail %>%
          return_household_groups(., currently_fleeing, y_n_dkr_dnc_list) %>%
          adorn_totals("row")
      }
      
      # Q15
      {
        Q15_detail <- recent_program_enrollment %>%
          keep_adults_and_hoh_only() %>%
          select(all_of(standard_detail_columns), LivingSituation)
        
        Q15 <- Q15_detail %>%
          create_prior_residence_groups(.)
      }
      
      # Q16
      {
        entry_income <- recent_program_enrollment %>%
          left_join(IncomeBenefits %>%
                      select(-c(PersonalID, ExportID)) %>%
                      filter(DataCollectionStage == 1),
                    by = c("EnrollmentID", "EntryDate" = "InformationDate"))
        
        annual_income <- recent_program_enrollment %>%
          filter(is.na(ExitDate)) %>%
          get_annual_id(., IncomeBenefits, IncomeBenefitsID) %>%
          left_join(IncomeBenefits %>%
                      select(colnames(IncomeBenefits)[colnames(IncomeBenefits) %nin% colnames(recent_program_enrollment)]),
                    by = c("IncomeBenefitsID" = "IncomeBenefitsID"))
        
        exit_income <- recent_program_enrollment %>%
          filter(!is.na(ExitDate)) %>%
          left_join(IncomeBenefits %>%
                      select(-c(PersonalID, ExportID)) %>%
                      filter(DataCollectionStage == 3),
                    by = c("EnrollmentID", "ExitDate" = "InformationDate"))
        
        Q16_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "new_veteran_status",
                 "chronic", "youth")
        
        for(period in entry_annual_exit) {
          
          data <- get(paste0(period, "_income")) %>%
            keep_adults_only() %>%
            determine_total_income(., annual = period == "annual") 
          
          Q16_detail <- Q16_detail %>%
            left_join(data %>%
                        select(EnrollmentID, calculated_total_income, 
                               earned_income, total_income_group) %>%
                        `colnames<-`(paste0(period, "_", 
                                            c("EnrollmentID", 
                                              "calculated_total_income", 
                                              "earned_income", 
                                              "total_income_group"))),
                      by = c("EnrollmentID" = paste0(period, "_EnrollmentID")))
          
          data <- data %>%
            create_income_groups(., annual = period == "annual") %>%
            `colnames<-`(c("total_income_group", paste0(period, "Income")))
          
          assign(paste0(period, "_income_groups"), data)
        }
        
        Q16 <- entry_income_groups %>%
          full_join(annual_income_groups, by = "total_income_group") %>%
          left_join(exit_income_groups, by = "total_income_group") %>%
          rename(Income.at.Start = entryIncome,
                 Income.at.Latest.Annual.Assessment.for.Stayers = annualIncome,
                 Income.at.Exit.for.Leavers = exitIncome) %>%
          adorn_totals("row")
      }
      
      # Q17
      {
        Q17_detail <- recent_program_enrollment %>%
          keep_adults_and_hoh_only() %>%
          select(all_of(standard_detail_columns), "new_veteran_status",
                 "chronic", "youth")
        
        for(period in entry_annual_exit) {
          data <- get(paste0(period, "_income")) %>%
            select(Earned, Unemployment, SSI, SSDI, VADisabilityService, 
                   VADisabilityNonService, PrivateDisability, WorkersComp, TANF,
                   GA, SocSecRetirement, Pension, ChildSupport, Alimony,
                   OtherIncomeSource, EnrollmentID)
          
          Q17_detail <- Q17_detail %>%
            left_join(data %>%
                        `colnames<-`(paste0(period, "_", colnames(data))),
                      by = c("EnrollmentID" = paste0(period, "_EnrollmentID")))
        }
        
        Q17 <- recent_program_enrollment %>%
          keep_adults_only() %>%
          create_income_sources(.)
      }
      
      # Q18
      {
        Q18_detail <- "See Q16_detail.csv"
        
        Q18_data <- recent_program_enrollment %>%
          keep_adults_only() %>%
          create_income_categories(.) %>%
          adorn_totals("row")
        
        has_income <- Q18_data %>%
          filter(income_category %in% c("Adults with Only Earned Income (i.e., Employment Income)", 
                                        "Adults with Only Other Income", 
                                        "Adults with Both Earned and Other Income")) %>%
          select(-income_category) %>% 
          colSums()
        
        Q18 <- Q18_data %>%
          mutate(income_category = as.character(income_category)) %>%
          rbind(., c("1 or more source of income", has_income)) %>%
          rbind(., income_information_present(recent_program_enrollment %>%
                                                keep_adults_only()))
      }
      
      # Q19a1
      {
        needed_columns <- c("earned_amount", "other_amount", "calculated_total_income")
        
        for(period in entry_annual_exit[1:2]) {
          income_for_changes <- get(paste0(period, "_income")) %>%
            keep_adults_only() %>%
            filter(PersonalID %in% intersect(
              entry_income$PersonalID[entry_income$IncomeFromAnySource %in% c(0, 1)],
              annual_income$PersonalID[annual_income$IncomeFromAnySource %in% c(0, 1)])) %>%
            determine_total_income(., annual = period == "annual") %>%
            mutate(earned_amount = if_else(calculated_total_income > 0 &
                                             earned_income > 0, 
                                           earned_income, 0),
                   other_amount = if_else(calculated_total_income > 0 &
                                            earned_income < calculated_total_income,
                                          calculated_total_income - earned_income, 0)) %>%
            select(c(PersonalID, all_of(needed_columns))) %>%
            `colnames<-`(c("PersonalID", paste0(period, "_", needed_columns[1:2]), 
                           paste0(period, "_total_amount"))) %>%
            ifnull(., 0)
          
          assign(paste0(period, "_income_for_changes"), income_for_changes)
        }
        
        Q19a1_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns)) %>%
          keep_adults_only() %>%
          filter(PersonalID %in% intersect(
            entry_income$PersonalID[entry_income$IncomeFromAnySource %in% c(0, 1)],
            annual_income$PersonalID[annual_income$IncomeFromAnySource %in% c(0, 1)])) %>%
          left_join(entry_income_for_changes, by = "PersonalID") %>%
          left_join(annual_income_for_changes, by = "PersonalID")
        
        for(row in c("earned", "other", "total")) {
          titles <- paste(c("Number of Adults with", "Average Change in"), 
                          str_to_title(row), "Income")
          data <- entry_income_for_changes %>%
            get_income_type_changes(., row, "annual") %>%
            cbind(titles, .)
          
          data[2, 1:9] <- decimal_format(data[2, 1:9])
          data[1, 10] <- decimal_format(data[1, 10], 4)
          
          if(row == "earned") {
            Q19a1 <- data
          } else {
            Q19a1 <- Q19a1 %>%
              union(data)
          }
          rm(data)
        }
        
        Q19a1 <- Q19a1 %>%
          mutate(titles = case_when(
            titles == "Number of Adults with Total Income" ~
              "Number of Adults with Any Income (i.e., Total Income)",
            titles == "Average Change in Total Income" ~
              "Average Change in Overall Income",
            TRUE ~ titles)) %>%
          rename(Had.Income.Category.at.Start.and.Did.Not.Have.It.at.Annual.Assessment = lost_source,
                 Retained.Income.Category.But.Had.Less.at.Annual.Assessment.Than.at.Start = retained_decreased,
                 Retained.Income.Category.and.Same.at.Annual.Assessment.as.at.Start = retained_same,
                 Retained.Income.Category.and.Increased.at.Annual.Assessment = retained_increased,
                 Did.Not.Have.the.Income.Category.at.Start.and.Gained.the.Income.Category.at.Annual.Assessment = gained_source,
                 Did.Not.Have.the.Income.Category.at.Start.or.at.Annual.Assessment = did_not_have_source,
                 Total.Adults.including.those.with.No.Income = total_adults,
                 Performance.Measure.Adults.who.Gained.or.Increased.Income.from.Start.to.Annual.Assessment.Average.Gain = gained_or_increased,
                 Performance.measure.Percent.of.persons.who.accomplished.this.measure = percent_accomplished
          )
      }
      
      # Q19a2
      {
        for(period in entry_annual_exit[c(1, 3)]) {
          income_for_changes <- get(paste0(period, "_income")) %>%
            keep_adults_only() %>%
            filter(PersonalID %in% intersect(
              entry_income$PersonalID[entry_income$IncomeFromAnySource %in% c(0, 1)],
              exit_income$PersonalID[exit_income$IncomeFromAnySource %in% c(0, 1)])) %>%
            determine_total_income(., annual = period == "annual") %>%
            mutate(earned_amount = if_else(calculated_total_income > 0 &
                                             earned_income > 0, 
                                           earned_income, 0),
                   other_amount = if_else(calculated_total_income > 0 &
                                            earned_income < calculated_total_income,
                                          calculated_total_income - earned_income, 0)) %>%
            select(c(PersonalID, all_of(needed_columns))) %>%
            `colnames<-`(c("PersonalID", paste0(period, "_", needed_columns[1:2]), 
                           paste0(period, "_total_amount"))) %>%
            ifnull(., 0)
          
          assign(paste0(period, "_income_for_changes"), income_for_changes)
        }
        
        
        Q19a2_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns)) %>%
          keep_adults_only() %>%
          filter(PersonalID %in% intersect(
            entry_income$PersonalID[entry_income$IncomeFromAnySource %in% c(0, 1)],
            exit_income$PersonalID[exit_income$IncomeFromAnySource %in% c(0, 1)])) %>%
          left_join(entry_income_for_changes, by = "PersonalID")%>%
          left_join(exit_income_for_changes, by = "PersonalID")
        
        for(row in c("earned", "other", "total")){
          titles <- paste(c("Number of Adults with", "Average Change in"), 
                          str_to_title(row), "Income")
          data <- entry_income_for_changes %>%
            get_income_type_changes(., row, "exit") %>%
            cbind(titles, .)
          
          data[2, 1:9] <- decimal_format(data[2, 1:9])
          data[1, 10] <- decimal_format(data[1, 10], 4)
          
          if(row == "earned") {
            Q19a2 <- data
          } else {
            Q19a2 <- Q19a2 %>%
              union(data)
          }
          rm(data)
        }
        
        Q19a2 <- Q19a2 %>%
          mutate(titles = case_when(
            titles == "Number of Adults with Total Income" ~
              "Number of Adults with Any Income (i.e., Total Income)",
            titles == "Average Change in Total Income" ~
              "Average Change in Overall Income",
            TRUE ~ titles)) %>%
          rename(Had.Income.Category.at.Start.and.Did.Not.Have.It.at.Exit = lost_source,
                 Retained.Income.Category.But.Had.Less.at.Exit.Than.at.Start = retained_decreased,
                 Retained.Income.Category.and.Same.at.Exit.as.at.Start = retained_same,
                 Retained.Income.Category.and.Increased.at.Exit = retained_increased,
                 Did.Not.Have.the.Income.Category.at.Start.and.Gained.the.Income.Category.at.Exit = gained_source,
                 Did.Not.Have.the.Income.Category.at.Start.or.at.Exit = did_not_have_source,
                 Total.Adults.including.those.with.No.Income = total_adults,
                 Performance.Measure.Adults.who.Gained.or.Increased.Income.from.Start.to.Exit.Average.Gain = gained_or_increased,
                 Performance.measure.Percent.of.persons.who.accomplished.this.measure = percent_accomplished
          )
      }
      
      # Q19b
      {
        Q19b_detail <- Q17_detail %>%
          filter(EnrollmentID %in% exit_income$EnrollmentID[exit_income$IncomeFromAnySource %in% c(0, 1)]) %>%
          inner_join(recent_program_enrollment %>% 
                       select(EnrollmentID, DisablingCondition) %>%
                       filter(DisablingCondition %in% c(0, 1)),
                     by = "EnrollmentID")
        
        Q19b <- exit_income %>%
          income_hh_type_disabling_condition_table(.)
        
        Q19b[, c(5, 9, 13)] <- decimal_format(Q19b[, c(5, 9, 13)], 4)
      }
      
      # Q20a
      {
        Q20a_detail <- recent_program_enrollment %>%
          keep_adults_only() %>%
          select(all_of(standard_detail_columns), new_veteran_status, chronic)
        
        for(period in entry_annual_exit) {
          data <- get(paste0(period, "_income")) %>%
            select(EnrollmentID, all_of(benefit_list), BenefitsFromAnySource)
          
          Q20a_detail <- Q20a_detail %>%
            left_join(data %>%
                        `colnames<-`(paste0(period, "_", colnames(data))),
                      by = c("EnrollmentID" = paste0(period, "_EnrollmentID")))
        }
        
        Q20a <- create_benefit_groups(recent_program_enrollment)
      }
      
      # Q20b
      {
        Q20b_detail <- "See Q20a_detail.csv"
        for(period in entry_annual_exit) {
          
          data <- get(paste0(period, "_income")) %>%
            keep_adults_only() %>%
            mutate(benefit_count = case_when(
              BenefitsFromAnySource == 0 &
                (is.na(SNAP) | SNAP == 0) &
                (is.na(WIC) | WIC == 0) &
                (is.na(TANFChildCare) | TANFChildCare == 0) &
                (is.na(TANFTransportation) | TANFTransportation == 0) &
                (is.na(OtherTANF) | OtherTANF == 0) &
                (is.na(OtherBenefitsSource) | OtherBenefitsSource == 0) ~ "No sources",
              BenefitsFromAnySource == 1 |
                SNAP == 1 |
                WIC == 1 |
                TANFChildCare == 1 |
                TANFTransportation == 1 |
                OtherTANF == 1 |
                OtherBenefitsSource == 1 ~ "One or more source(s)",
              BenefitsFromAnySource %in% c(8, 9) ~ "Unknown or refused",
              TRUE ~ "Not collected/annual assessment not due"))
          
          if(period == "annual"){
            data <- data %>%
              left_join(annual_assessment_dates, by = "HouseholdID") %>%
              mutate(benefit_count = if_else(
                is.na(annual_due), "Not collected/annual assessment not due",
                benefit_count))
          }
          
          data <- data %>%
            group_by(benefit_count) %>%
            summarise(!!paste0(period, "_people") := n_distinct(PersonalID, na.rm = TRUE))
          
          assign(paste0(period, "_benefit_counts"), data)
        }
        
        benefit_count <- c("No sources", "One or more source(s)", "Client.Does.Not.Know.or.Refused",
                           "Not collected/annual assessment not due")
        
        Q20b <- as.data.frame(benefit_count) %>%
          left_join(entry_benefit_counts, by = "benefit_count") %>%
          left_join(annual_benefit_counts, by = "benefit_count") %>%
          left_join(exit_benefit_counts, by = "benefit_count") %>%
          rename(BenefitGroup = benefit_count,
                 Benefit.at.Start = entry_people,
                 Benefit.at.Latest.Annual.Assessment.for.Stayers = annual_people,
                 Benefit.at.Exit.for.Leavers = exit_people) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      
      # Q21
      {
        Q21_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns))
        
        insurance_list <- c("Medicaid", "Medicare", "SCHIP", "VAMedicalServices", 
                            "EmployerProvided", "COBRA", "PrivatePay", "StateHealthIns",
                            "IndianHealthServices", "OtherInsurance")
        
        for(period in entry_annual_exit) {
          
          data <- get(paste0(period, "_income")) %>%
            mutate(insurance_count = ifnull(Medicaid == 1, 0) + ifnull(Medicare == 1, 0) +
                     ifnull(SCHIP == 1, 0) + ifnull(VAMedicalServices == 1, 0) +
                     ifnull(EmployerProvided == 1, 0) + ifnull(COBRA == 1, 0) +
                     ifnull(PrivatePay == 1, 0) + ifnull(StateHealthIns == 1, 0) +
                     ifnull(IndianHealthServices == 1, 0) + ifnull(OtherInsurance == 1, 0)
            )
          
          insurance_type_detail <- data %>%
            filter(period != "annual" |
                     HouseholdID %in% annual_assessment_dates$HouseholdID) %>%
            select(EnrollmentID, all_of(insurance_list)) 
          
          Q21_detail <- Q21_detail %>%
            left_join(insurance_type_detail %>%
                        `colnames<-`(c("EnrollmentID",
                                       paste0(period, "_",
                                              colnames(insurance_type_detail)[2:length(insurance_type_detail)]))), 
                      by = "EnrollmentID")
          
          insurance_types <- insurance_type_detail %>%
            select(-EnrollmentID) %>%
            pivot_existing_only(., "InsuranceType", period) %>%
            left_join(InsuranceTypes, by = c("InsuranceType" = "InsuranceGroup")) %>%
            mutate(InsuranceType = OfficialInsuranceName) %>%
            select(-OfficialInsuranceName)
          
          insurance_present <- data %>%
            select(PersonalID, HouseholdID, InsuranceFromAnySource, insurance_count) %>%
            mutate(InsuranceType = case_when(
              period == "annual" &
                HouseholdID %nin% annual_assessment_dates$HouseholdID ~ 
                "Annual assessment not required",
              insurance_count == 0 ~ 
                case_when(
                  InsuranceFromAnySource %in% c(1, 0) ~ "No health insurance",
                  InsuranceFromAnySource %in% c(8, 9) ~ "Client.Does.Not.Know.or.Refused",
                  TRUE ~ "Data.Not.Collected"),
              insurance_count == 1 ~ "One source of insurance",
              TRUE ~ "More than one source of insurance")) %>%
            group_by(InsuranceType) %>%
            summarize(!!paste0(period, "Clients") := n_distinct(PersonalID, na.rm = TRUE))
          
          assign(paste0(period, "_insurance_types"), insurance_types %>%
                   rbind(insurance_present))
        }
        
        full_insurance_list <- c(InsuranceTypes$OfficialInsuranceName, 
                                 c("No health insurance", "Client.Does.Not.Know.or.Refused", "Data.Not.Collected", 
                                   "Annual assessment not required", 
                                   "One source of insurance",
                                   "More than one source of insurance"))
        
        Q21 <- as.data.frame(full_insurance_list) %>%
          rename(InsuranceType = full_insurance_list) %>%
          left_join(entry_insurance_types, by = "InsuranceType") %>%
          left_join(annual_insurance_types, by = "InsuranceType") %>%
          left_join(exit_insurance_types, by = "InsuranceType") %>%
          rename(At.Start = entryClients,
                 At.Annual.Assessment.for.Stayers = annualClients,
                 At.Exit.for.Leavers = exitClients) 
        
        Q21[is.na(Q21) & Q21$InsuranceType != "Annual assessment not required"] <- 0
      }
      
      # Q22a1
      {
        # groups in specs include Data.Not.Collected, what does that mean in length of stay?
        # it's not defined in the reporting glossary
        Q22a1_detail <- create_lot_table(recent_program_enrollment)  
        
        Q22a1_total <- Q22a1_detail %>%
          group_by(APR_enrollment_length_group) %>%
          summarise(Total = n_distinct(PersonalID, na.rm = TRUE))
        
        Q22a1_leavers <- Q22a1_detail %>%
          filter(!is.na(ExitDate)) %>%
          group_by(APR_enrollment_length_group) %>%
          summarise(Leavers = n_distinct(PersonalID, na.rm = TRUE))
        
        Q22a1_stayers <- Q22a1_detail %>%
          filter(is.na(ExitDate)) %>%
          group_by(APR_enrollment_length_group) %>%
          summarise(Stayers = n_distinct(PersonalID, na.rm = TRUE))
        
        Q22a1 <- length_of_time_groups("APR", "APR_enrollment_length_group") %>%
          left_join(Q22a1_total, by = "APR_enrollment_length_group") %>%
          left_join(Q22a1_leavers, by = "APR_enrollment_length_group") %>%
          left_join(Q22a1_stayers, by = "APR_enrollment_length_group") %>%
          full_join(data.frame(APR_enrollment_length_group = c("Data Not Collected")),
                    by = "APR_enrollment_length_group") %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q22a2
      {
        Q22a2_detail <- Q22a1_detail
        
        Q22a2_total <- Q22a1_detail %>%
          group_by(CAPER_enrollment_length_group) %>%
          summarise(Total = n_distinct(PersonalID, na.rm = TRUE))
        
        Q22a2_leavers <- Q22a1_detail %>%
          filter(!is.na(ExitDate)) %>%
          group_by(CAPER_enrollment_length_group) %>%
          summarise(Leavers = n_distinct(PersonalID, na.rm = TRUE))
        
        Q22a2_stayers <- Q22a1_detail %>%
          filter(is.na(ExitDate)) %>%
          group_by(CAPER_enrollment_length_group) %>%
          summarise(Stayers = n_distinct(PersonalID, na.rm = TRUE))
        
        Q22a2 <- length_of_time_groups("CAPER", "CAPER_enrollment_length_group") %>%
          left_join(Q22a2_total, by = "CAPER_enrollment_length_group") %>%
          left_join(Q22a2_leavers, by = "CAPER_enrollment_length_group") %>%
          left_join(Q22a2_stayers, by = "CAPER_enrollment_length_group") %>%
          full_join(data.frame(CAPER_enrollment_length_group = c("Data Not Collected")),
                    by = "CAPER_enrollment_length_group") %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q22b
      {
        Q22b_detail <- "See Q22a1_detail.csv"
        
        Q22b <- Q22a1_detail %>%
          ungroup() %>%
          summarise(summary = "Average Length",
                    Leavers = round(
                      mean(days_enrolled[!is.na(ExitDate)]), 4),
                    Stayers = round(
                      mean(days_enrolled[is.na(ExitDate)]), 4)) %>%
          rbind(Q22a1_detail %>%
                  ungroup() %>%
                  summarise(summary = "Median Length",
                            Leavers = median(days_enrolled[!is.na(ExitDate)]),
                            Stayers = median(days_enrolled[is.na(ExitDate)])))
      }
      
      # Q22c
      {
        Q22c_detail <- create_time_to_move_in(recent_program_enrollment)
        
        average_time_to_house <- Q22c_detail %>%
          summarise(housing_length_group = "Average length of time to housing",
                    Total = round(mean(days_to_house, na.rm = TRUE), 0),
                    Without.Children = round(mean(
                      days_to_house[household_type == "AdultsOnly"],
                      na.rm = TRUE), 0),
                    With.Children.And.Adults = round(mean(
                      days_to_house[household_type == "AdultsAndChildren"],
                      na.rm = TRUE), 0),
                    With.Only.Children = round(mean(
                      days_to_house[household_type == "ChildrenOnly"],
                      na.rm = TRUE), 0),
                    Unknown.Household.Type = round(mean(
                      days_to_house[household_type == "Unknown"],
                      na.rm = TRUE), 0))
        
        exited_without_move_in_data <- recent_program_enrollment %>%
          filter(ProjectType %in% c(3, 13) &
                   is.na(HoH_HMID) &
                   !is.na(ExitDate)) %>%
          mutate(housing_length_group = "Persons who were exited without move-in") %>%
          select(c(intersect(colnames(recent_program_enrollment),
                             colnames(Q22c_detail)),
                   "housing_length_group"))
        
        exited_without_move_in <- exited_without_move_in_data %>%
          return_household_groups(., housing_length_group, "Persons who were exited without move-in") 
        
        time_to_house_groups <- length_of_time_groups("days_prior_to_housing", "housing_length_group") %>%
          filter(housing_length_group %nin% c("731 days or more", 
                                              "Not yet moved into housing",
                                              "Data.Not.Collected"))
        
        Q22c <- data.frame(time_to_house_groups) %>%
          left_join(Q22c_detail %>%
                      return_household_groups(., housing_length_group, time_to_house_groups$housing_length_group),
                    by = "housing_length_group") %>%
          adorn_totals("row") %>%
          mutate(housing_length_group = case_when(
            housing_length_group == "Total" ~ "Total (persons moved into housing)",
            TRUE ~ housing_length_group)) %>%
          union(average_time_to_house) %>%
          union(exited_without_move_in) %>%
          union(Q22c_detail %>%
                  mutate(housing_length_group = "Total persons") %>%
                  return_household_groups(., housing_length_group, "Total persons")) %>%
          ifnull(., 0)
        
        Q22c_detail <- Q22c_detail %>%
          full_join(exited_without_move_in_data,
                    by = colnames(exited_without_move_in_data))
      }
      
      # Q22d
      {
        Q22d_detail <- "See Q22a2_detail.csv"
        
        Q22d <- Q22a1_detail %>%
          return_household_groups(., CAPER_enrollment_length_group, 
                                  length_of_participation$enrollment_length_group) %>%
          full_join(data.frame(CAPER_enrollment_length_group = c("Data Not Collected")),
                    by = "CAPER_enrollment_length_group") %>%
          adorn_totals("row") %>%
          ifnull(., 0) 
      }
      
      #Q22e
      {
        Q22e_detail <- recent_program_enrollment %>%
          select(c("ProjectType", all_of(housing_program_detail_columns), "age",
                   "HoH_EntryDate", "DateToStreetESSH", "HoH_ADHS")) %>%
          create_time_prior_to_housing()
        
        homeless_to_housed_groups <- length_of_time_groups("days_prior_to_housing", "days_prior_to_housing")
        
        Q22e_groups <- Q22e_detail %>%
          return_household_groups(., days_prior_to_housing,
                                  homeless_to_housed_groups$days_prior_to_housing) %>%
          ifnull(., 0) %>%
          adorn_totals("row")
        
        Q22e <- Q22e_groups %>%
          filter(days_prior_to_housing %nin% c("Not yet moved into housing", "Data.Not.Collected", "Total")) %>%
          untabyl() %>%
          adorn_totals("row") %>%
          mutate(days_prior_to_housing = case_when(
            days_prior_to_housing == "Total" ~ "Total (persons moved into housing)",
            TRUE ~ days_prior_to_housing)) %>%
          union(Q22e_groups %>%
                  filter(days_prior_to_housing %in% c("Not yet moved into housing", "Data.Not.Collected", "Total")))
      }
      
      # Q23c
      {
        Q23c_detail <- recent_program_enrollment %>%
          filter(!is.na(ExitDate)) %>%
          select(ProjectType, all_of(standard_detail_columns), Destination,
                 new_veteran_status, youth)
        
        Q23c <- create_destination_groups(Q23c_detail) %>%
          mutate(across(everything(), as.character))
        
        Q23c[45, 2:6] <- as.list(decimal_format(as.numeric(Q23c[45, 2:6]), 4))
      }
      
      # Q24
      {
        Q24_detail <- recent_program_enrollment %>%
          filter(!is.na(ExitDate) &
                   ProjectType == 12) %>%
          select(ProjectType, all_of(standard_detail_columns), 
                 HousingAssessment, SubsidyInformation) %>%
          mutate(assessment_at_exit = case_when(
            HousingAssessment == 1 ~
              case_when(SubsidyInformation == 1 ~
                          assessment_outcomes[1],
                        SubsidyInformation == 2 ~
                          assessment_outcomes[2],
                        SubsidyInformation == 3 ~
                          assessment_outcomes[3],
                        SubsidyInformation == 4 ~
                          assessment_outcomes[4]),
            HousingAssessment == 2 ~
              case_when(SubsidyInformation == 1 ~
                          assessment_outcomes[5],
                        SubsidyInformation == 2 ~
                          assessment_outcomes[6]),
            HousingAssessment == 3 ~ assessment_outcomes[7],
            HousingAssessment == 4 ~ assessment_outcomes[8],
            HousingAssessment == 5 ~ assessment_outcomes[9],
            HousingAssessment == 6 ~ assessment_outcomes[10],
            HousingAssessment == 7 ~ assessment_outcomes[11],
            HousingAssessment == 10 ~ assessment_outcomes[12],
            HousingAssessment %in% c(8, 9) ~ assessment_outcomes[13],
            HousingAssessment == 99 ~ assessment_outcomes[14])) 
        
        Q24 <- Q24_detail %>%
          return_household_groups(., assessment_at_exit, assessment_outcomes) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
        
      }
      
      #-------------------------------------------------------------
      #------------------- Veteran Questions -----------------------
      #-------------------------------------------------------------
      
      recent_veteran_enrollment <- recent_program_enrollment %>%
        filter(new_veteran_status == 1)
      
      # Q25a
      {
        
        ##  check this and Q25b specifically against when to apply chronic logic
        ##  APR specs say 25a uses the same chronic logic as the rest of the report,
        ##  while 25b uses chronic data for all adults present in the report
        ##  range, whether or not that stay was their most recent stay...which seem
        ##  like difference measures initially, except that the report instructions
        ##  say to refer to the glossary for the most up-to-date chronic calculation
        ## to use, which ALSO says "Note that reporting on clients’ CH-at-start 
        ##  status in a specific report date range may require the examination of 
        ##  data on clients who were in the household but left prior to the report 
        ##  date range," and that seems like the same intent as looking at folks who
        ##  were in the household but not as their most recent stay. Right?
        
        Q25a_detail <- recent_program_enrollment %>%
          keep_adults_only() %>%
          mutate(category = case_when(
            new_veteran_status == 1 &
              chronic == "Y" ~ vet_chronic_categories[1],
            new_veteran_status == 1 ~ vet_chronic_categories[2],
            new_veteran_status == 0 ~ vet_chronic_categories[3],
            new_veteran_status %in% c(8, 9) ~ vet_chronic_categories[4],
            TRUE ~ vet_chronic_categories[5])) 
        
        Q25a <- Q25a_detail %>%
          return_household_groups(., category, vet_chronic_categories) %>%
          select(-With.Only.Children) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
        }
      
      # Q25b
      {
        Q25b_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns)) %>%
          filter(RelationshipToHoH == 1 & 
                   household_type != "ChildrenOnly") %>%
          mutate(category = case_when(
            HouseholdID %in%
              Q25a_detail$HouseholdID[Q25a_detail$category == vet_chronic_categories[1]] ~
              vet_chronic_categories[1],
            HouseholdID %in%
              Q25a_detail$HouseholdID[Q25a_detail$category == vet_chronic_categories[2]] ~
              vet_chronic_categories[2],
            HouseholdID %in%
              Q25a_detail$HouseholdID[Q25a_detail$category == vet_chronic_categories[3]] ~
              vet_chronic_categories[3],
            HouseholdID %in%
              Q25a_detail$HouseholdID[Q25a_detail$category == vet_chronic_categories[4]] ~
              vet_chronic_categories[4],
            TRUE ~ vet_chronic_categories[5])) 
        
        Q25b <- Q25b_detail %>%
          return_household_groups(., category, vet_chronic_categories) %>%
          select(-With.Only.Children) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q25c
      {
        Q25c_detail <- recent_veteran_enrollment %>%
          select(c(all_of(standard_detail_columns), "gender_combined")) %>%
          left_join(Client %>%
                      select(PersonalID, Female, Male, NoSingleGender, 
                             Transgender, Questioning, GenderNone),
                    by = "PersonalID") 
        
        Q25c <- Q25c_detail %>%
          create_gender_groups(.) %>%
          select(-With.Only.Children)
      }
      
      # Q25d
      {
        Q25d_detail <- Q11_detail %>%
          filter(PersonalID %in% recent_veteran_enrollment$PersonalID)
        
        Q25d <- Q25d_detail %>%
          create_age_groups(.) %>%
          select(-With.Only.Children) %>%
          filter(detailed_age_group %nin% detailed_age_group_list[1:3])
      }
      
      # Q25e
      {
        Q25e_detail <- "See Q13a1.csv, Q13b1.csv, and Q13c1.csv respectively"
        Q25e.1.and.2 <- recent_veteran_enrollment %>%
          inner_join(disability_table %>%
                       filter(DataCollectionStage %in% c(1, 3)), by ="EnrollmentID",
                     multiple = "all") %>%
          group_by(disability_name) %>%
          summarise(Conditions.At.Start = n_distinct(PersonalID[DataCollectionStage == 1], 
                                                     na.rm = TRUE),
                    Conditions.At.Exit.for.Leavers = n_distinct(
                      PersonalID[DataCollectionStage == 3 & !is.na(ExitDate)], 
                      na.rm = TRUE))
        
        Q25e.3 <- Q13c1_detail %>%
          filter(EnrollmentID %in% recent_veteran_enrollment$EnrollmentID) %>%
          group_by(disability_name) %>%
          summarise(Conditions.At.Latest.Assessment.for.Stayers = n_distinct(PersonalID, 
                                                                             na.rm = TRUE))
        
        Q25e <- as.data.frame(disability_list)  %>%
          `colnames<-`(c("disability_name")) %>%
          full_join(Q25e.1.and.2 %>%
                      select(disability_name, Conditions.At.Start), 
                    by = "disability_name") %>%
          full_join(Q25e.3, by = "disability_name") %>%
          full_join(Q25e.1.and.2 %>%
                      select(disability_name, Conditions.At.Exit.for.Leavers), 
                    by = "disability_name") %>%
          ifnull(0)
      }
      
      # Q25f
      # Q26f specifies that rows 11 and 12 are excluded intentionally, Q25f does
      # not specify this but it also does not show these rows in the table
      {
        Q25f_detail <- Q16_detail %>%
          filter(new_veteran_status == 1)
        
        Q25f <- recent_veteran_enrollment %>%
          create_income_categories(.) %>%
          adorn_totals("row")
      }
      
      # Q25g
      {
        Q25g_detail <- Q17_detail %>%
          filter(new_veteran_status == 1)
        
        Q25g <- recent_veteran_enrollment %>% 
          create_income_sources(.)
      }
      
      # Q25h
      {
        Q25h_detail <- Q20a_detail %>%
          filter(new_veteran_status == 1)
        
        Q25h <- create_benefit_groups(recent_veteran_enrollment)
      }
      
      # Q25i
      {
        Q25i_detail <- Q23c_detail %>%
          filter(new_veteran_status == 1)
        
        Q25i <- create_destination_groups(recent_veteran_enrollment) %>%
          mutate(across(everything(), as.character))
        
        Q25i[45, 2:6] <- as.list(decimal_format(as.numeric(Q25i[45, 2:6]), 4))
      }
      
      #-------------------------------------------------------------
      #------------------- Chronic Questions -----------------------
      #-------------------------------------------------------------
      
      recent_chronic_enrollment <- recent_program_enrollment %>%
        filter(chronic == "Y")
      
      # Q26a
      {
        Q26b_detail <- recent_program_enrollment %>%
          mutate(category = case_when(
            chronic == "Y" ~ chronic_categories[1],
            chronic == "N" ~ chronic_categories[2],
            chronic == "Client.Does.Not.Know.or.Refused" ~ chronic_categories[3],
            TRUE ~ chronic_categories[4])) 
        
        Q26a_detail <- Q26b_detail %>%
          filter(RelationshipToHoH == 1)
        
        Q26a <- Q26a_detail %>%
          return_household_groups(., category, chronic_categories) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
        }
      
      # Q26b
      {
        Q26b <- Q26b_detail %>%
          return_household_groups(., category, chronic_categories) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q26c
      {
        Q26c_detail <- recent_chronic_enrollment %>%
          select(c(all_of(standard_detail_columns), "gender_combined")) %>%
          left_join(Client %>%
                      select(PersonalID, Female, Male, NoSingleGender, 
                             Transgender, Questioning, GenderNone),
                    by = "PersonalID") 
        
        Q26c <- Q26c_detail %>%
          create_gender_groups(.)
      }
      
      # Q26d
      {
        Q26d_detail <- Q11_detail %>%
          filter(PersonalID %in% recent_chronic_enrollment$PersonalID)
        
        Q26d <- Q26d_detail %>%
          create_age_groups(., chronic = TRUE) 
      }
      
      # Q26e
      {
        Q26e_detail <- "See Q13a1.csv, Q13b1.csv, and Q13c1.csv respectively"
        Q26e.1.and.2 <- recent_chronic_enrollment %>%
          inner_join(disability_table %>%
                       filter(DataCollectionStage %in% c(1, 3)), by ="EnrollmentID",
                     multiple = "all") %>%
          group_by(disability_name) %>%
          summarise(Conditions.At.Start = n_distinct(PersonalID[DataCollectionStage == 1], 
                                                     na.rm = TRUE),
                    Conditions.At.Exit.for.Leavers = n_distinct(
                      PersonalID[DataCollectionStage == 3 & !is.na(ExitDate)], 
                      na.rm = TRUE))
        
        Q26e.3 <- Q13c1_detail %>%
          filter(EnrollmentID %in% recent_chronic_enrollment$EnrollmentID) %>%
          group_by(disability_name) %>%
          summarise(Conditions.At.Latest.Assessment.for.Stayers = n_distinct(PersonalID, 
                                                                             na.rm = TRUE))
        
        Q26e <- as.data.frame(disability_list)  %>%
          `colnames<-`(c("disability_name")) %>%
          full_join(Q26e.1.and.2 %>%
                      select(disability_name, Conditions.At.Start), 
                    by = "disability_name") %>%
          full_join(Q26e.3, by = "disability_name") %>%
          full_join(Q26e.1.and.2 %>%
                      select(disability_name, Conditions.At.Exit.for.Leavers), 
                    by = "disability_name") %>%
          ifnull(0)
      }
      
      # Q26f
      {
        Q26f_detail <- Q16_detail %>%
          filter(chronic == "Y")
        
        Q26f <- recent_chronic_enrollment %>%
          keep_adults_only() %>%
          create_income_categories(.) %>%
          adorn_totals("row")
      }
      
      # Q26g
      {
        Q26g_detail <- Q17_detail %>%
          filter(chronic == "Y")
        
        Q26g <- recent_chronic_enrollment %>%
          keep_adults_only() %>%
          create_income_sources(.)
      }
      
      # Q26h
      {
        Q26h_detail <- Q20a_detail %>%
          filter(chronic == "Y") 
        
        Q26h <- create_benefit_groups(recent_chronic_enrollment)
      }
      
      
      #-------------------------------------------------------------
      #-------------------- Youth Questions ------------------------
      #-------------------------------------------------------------
      
      recent_youth_enrollment <- recent_program_enrollment %>%
        filter(youth == 1)
      
      # Q27a
      {
        Q27a_detail <- Q11_detail %>%
          filter(PersonalID %in% recent_youth_enrollment$PersonalID) 
        
        Q27a <- Q27a_detail %>%
          create_age_groups(.) %>%
          filter(detailed_age_group %nin% detailed_age_group_list[c(1:2, 5:9)])
        }
      
      # Q27b
      {
        Q27b_headers <- as.data.frame(c("Parent youth < 18", "Parent youth 18 to 24"))  %>%
          `colnames<-`(c("household_type"))
        
        Q27b_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), youth_household,
                 has_children, age_group) %>%
          filter(youth_household == 1 &
                   has_children == 1) %>%
          mutate(household_member_type = case_when(
            RelationshipToHoH == 1 |
              age_group == "Adults" ~ "parenting_youth",
            RelationshipToHoH == 2 ~ "child_of_parenting_youth"),
            household_type = if_else(
              household_type == "AdultsAndChildren", "Parent youth 18 to 24",
              "Parent youth < 18"
            ))
        
        Q27b <- Q27b_headers %>%
          full_join(Q27b_detail %>%
                      group_by(household_type) %>%
                      summarize(Total.Parenting.Youth = n_distinct(PersonalID[household_member_type == "parenting_youth"], 
                                                                   na.rm = TRUE),
                                Total.Children.of.Parenting.Youth = n_distinct(PersonalID[household_member_type == "child_of_parenting_youth"], 
                                                                               na.rm = TRUE),
                                Total.Persons = n_distinct(PersonalID, na.rm = TRUE),
                                Total.Households = n_distinct(PersonalID[RelationshipToHoH == 1], 
                                                              na.rm = TRUE)),
                    by = "household_type") %>%
          ifnull(., 0)
      }
      
      # Q27c
      {
        Q27c_detail <- recent_youth_enrollment %>%
          select(c(all_of(standard_detail_columns), "gender_combined")) %>%
          left_join(Client %>%
                      select(PersonalID, Female, Male, NoSingleGender, 
                             Transgender, Questioning, GenderNone),
                    by = "PersonalID") 
        
        Q27c <- Q27c_detail %>%
          create_gender_groups(.)
      }
      
      # Q27d
      {
        Q27d_detail <- Q15_detail %>%
          filter(PersonalID %in% recent_youth_enrollment$PersonalID &
                   RelationshipToHoH == 1) 
        
        Q27d <- Q27d_detail %>%
          create_prior_residence_groups(.)
      }
      
      # Q27e
      {
        Q27e_detail <- create_lot_table(recent_youth_enrollment)  
        
        Q27e_total <- Q27e_detail %>%
          group_by(APR_enrollment_length_group) %>%
          summarise(Total = n_distinct(PersonalID, na.rm = TRUE))
        
        Q27e_leavers <- Q27e_detail %>%
          filter(!is.na(ExitDate)) %>%
          group_by(APR_enrollment_length_group) %>%
          summarise(Leavers = n_distinct(PersonalID, na.rm = TRUE))
        
        Q27e_stayers <- Q27e_detail %>%
          filter(is.na(ExitDate)) %>%
          group_by(APR_enrollment_length_group) %>%
          summarise(Stayers = n_distinct(PersonalID, na.rm = TRUE))
        
        Q27e <- length_of_time_groups("APR", "APR_enrollment_length_group") %>%
          left_join(Q27e_total, by = "APR_enrollment_length_group") %>%
          left_join(Q27e_leavers, by = "APR_enrollment_length_group") %>%
          left_join(Q27e_stayers, by = "APR_enrollment_length_group") %>%
          full_join(data.frame(APR_enrollment_length_group = c("Data Not Collected")),
                    by = "APR_enrollment_length_group") %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q27f
      {
        Q27f_detail <- Q23c_detail %>%
          filter(youth == 1)
        
        Q27f <- create_destination_groups(recent_youth_enrollment)  %>%
          mutate(across(everything(), as.character))
        
        Q27f[45, 2:6] <- as.list(decimal_format(as.numeric(Q27f[45, 2:6]), 4))
      }
      
      # Q27g
      # label for A17 specifies adults like the other uses of this logic, but
      # this question is unique in that it includes HoHs who are minors
      {
        Q27g_detail <- Q17_detail %>%
          filter(youth == 1)
        
        Q27g <- recent_youth_enrollment %>%
          keep_adults_and_hoh_only() %>%
          create_income_sources(.)
      }
      
      # Q27h
      {
        Q27h_detail <- Q16_detail %>%
          filter(youth == 1)
        
        Q27h_data <- recent_youth_enrollment %>%
          keep_adults_and_hoh_only() %>%
          create_income_categories(.) %>%
          adorn_totals("row")
        
        
        has_income <- Q27h_data %>%
          filter(income_category %in% c("Adults with Only Earned Income (i.e., Employment Income)", 
                                        "Adults with Only Other Income", 
                                        "Adults with Both Earned and Other Income")) %>%
          select(-income_category) %>% 
          colSums()
        
        Q27h <- Q27h_data %>%
          mutate(income_category = as.character(income_category)) %>%
          rbind(., c("1 or more source of income", has_income)) %>%
          rbind(., c("Youth with Income Information at Start and Annual Assessment/Exit",
                     income_information_present(recent_youth_enrollment)[2:4]))
      }
      
      # Q27i
      {
        Q27i_detail <- Q19b_detail %>%
          filter(PersonalID %in% recent_youth_enrollment$PersonalID)
        
        Q27i <- exit_income %>%
          income_hh_type_disabling_condition_table(., youth = TRUE)
        
        Q27i[, c(5, 9, 13, 17)] <- decimal_format(Q27i[, c(5, 9, 13, 17)], 4)
      }
      
      # Q27j
      {
        Q27j_detail <- "See Q27e_detail.csv"
        Q27j <- Q27e_detail %>%
          ungroup() %>%
          summarise(summary = "Average Length",
                    Leavers = round(
                      mean(days_enrolled[!is.na(ExitDate)]), 4),
                    Stayers = round(
                      mean(days_enrolled[is.na(ExitDate)]), 4)) %>%
          rbind(Q27e_detail %>%
                  ungroup() %>%
                  summarise(summary = "Median Length",
                            Leavers = median(days_enrolled[!is.na(ExitDate)]),
                            Stayers = median(days_enrolled[is.na(ExitDate)])))
      }
      
      # Q27k
      {
        Q27k_detail <- create_time_to_move_in(recent_youth_enrollment)
        
        average_time_to_house <- Q27k_detail %>%
          summarise(housing_length_group = "Average length of time to housing",
                    Total = round(mean(days_to_house, na.rm = TRUE), 0),
                  Without.Children = round(mean(
                    days_to_house[household_type == "AdultsOnly"],
                    na.rm = TRUE), 0),
                  With.Children.And.Adults = round(mean(
                    days_to_house[household_type == "AdultsAndChildren"],
                    na.rm = TRUE), 0),
                  With.Only.Children = round(mean(
                    days_to_house[household_type == "ChildrenOnly"],
                    na.rm = TRUE), 0),
                  Unknown.Household.Type = round(mean(
                    days_to_house[household_type == "Unknown"],
                    na.rm = TRUE), 0))
        
        exited_without_move_in_data <- recent_youth_enrollment %>%
          filter(ProjectType %in% c(3, 13) &
                   is.na(HoH_HMID) &
                   !is.na(ExitDate)) %>%
          mutate(housing_length_group = "Persons who were exited without move-in") %>%
          select(c(intersect(colnames(recent_youth_enrollment),
                             colnames(Q27k_detail)),
                   "housing_length_group"))
        
        Q27k_detail <- Q27k_detail %>%
          filter(EnrollmentID %nin% exited_without_move_in_data$EnrollmentID)
        
        exited_without_move_in <- exited_without_move_in_data %>%
          return_household_groups(., housing_length_group, "Persons who were exited without move-in") 
        
        time_to_house_groups <- length_of_time_groups("days_prior_to_housing", "housing_length_group") %>%
          filter(housing_length_group %nin% c("731 days or more", 
                                              "Not yet moved into housing",
                                              "Data.Not.Collected"))
        
        Q27k <- Q27k_detail %>%
          return_household_groups(., housing_length_group, time_to_house_groups$housing_length_group) %>%
          adorn_totals("row") %>%
          mutate(housing_length_group = case_when(
            housing_length_group == "Total" ~ "Total (persons moved into housing)",
            TRUE ~ housing_length_group)) %>%
          union(average_time_to_house) %>%
          union(exited_without_move_in) %>%
          union(Q27k_detail %>%
                  mutate(housing_length_group = "Total persons") %>%
                  return_household_groups(., housing_length_group, "Total persons")) %>%
          ifnull(., 0)
        
        Q27k_detail <- Q27k_detail %>%
          full_join(exited_without_move_in_data,
                    by = colnames(exited_without_move_in_data))
      }
      
      # Q27l
      {
        Q27l_detail <- recent_youth_enrollment %>%
          select(c("ProjectType", all_of(housing_program_detail_columns), "age",
                   "HoH_EntryDate", "DateToStreetESSH", "HoH_ADHS")) %>%
          create_time_prior_to_housing()
        
        Q27l_groups <- Q27l_detail %>%
          return_household_groups(., days_prior_to_housing,
                                  homeless_to_housed_groups$days_prior_to_housing) %>%
          ifnull(., 0) %>%
          adorn_totals("row")
        
        Q27l <- Q27l_groups %>%
          filter(days_prior_to_housing %nin% c("Not yet moved into housing", "Data.Not.Collected", "Total")) %>%
          untabyl() %>%
          adorn_totals("row") %>%
          mutate(days_prior_to_housing = case_when(
            days_prior_to_housing == "Total" ~ "Total (persons moved into housing)",
            TRUE ~ days_prior_to_housing)) %>%
          union(Q27l_groups %>%
                  filter(days_prior_to_housing %in% c("Not yet moved into housing", "Data.Not.Collected", "Total")))
      }
    }
    
    #-------------------------------------------------------------
    #-------------------------------------------------------------
    #-------------------------------------------------------------
    
    
    projects_included <- max(case_when(length(project_list) > 1 ~ "Multiple Projects",
                                       TRUE ~ paste0(Project$ProjectName[Project$ProjectID %in% project_list])))
    
    APR_relevant <- max(length(intersect(c(1:7),
                                         unique(Funder$Funder[Funder$ProjectID %in% project_list]))) > 0 |
                          project_list == 1552)
    
    CAPER_relevant <- length(intersect(c(8:11),
                                       unique(Funder$Funder[Funder$ProjectID %in% project_list]))) > 0
    
    
    if (generate_new_kits) {
      
      folder_name <- paste("Test Kit", format(Sys.Date(), "%m.%d.%y"))
      
      if (!dir.exists(folder_name)) {
        dir.create(folder_name)
      }
      
      if (!dir.exists(paste0(folder_name, "/HMIS CSVs"))) {
        dir.create(paste0(folder_name, "/HMIS CSVs"))
      }
      
      if (!dir.exists(paste0(folder_name, "/Reports"))) {
        dir.create(paste0(folder_name, "/Reports"))
      }
      
      if (projects_included != "Multiple Projects") {
        write_csvs_for(project_list, zip_title = projects_included,
                       write_to <- paste0(folder_name, "/HMIS CSVs"))
      }
      
      if(APR_relevant) {
        for (question in APR_files) {
          if (exists(question)) {
            
            to_write <- get(question)
            
            if(question != "Q4a") {
              to_write <- to_write %>%
                set_hud_format()
            }
            
            write.csv(get(paste0(question, "_detail")),
                      file.path(paste0("created_files_2/", question, ".csv")),
                      row.names = FALSE)
            
            to_write <- to_write %>% 
              ifnull(., 0) 
            
            to_write[is.na(to_write)] <- ""
            
            write_csv(to_write, file.path(paste0("created_files/", question, ".csv")))
          } else {
            missing_files <- c(missing_files, paste("APR -", projects_included, "-", question))
          }
        }
        general_wd <- getwd()
        setwd(paste0(general_wd, "/", folder_name, "/Reports"))
        
        archive_write_dir(paste0("APR - ", projects_included, " (A).zip"),
                          paste0(general_wd, "/created_files"))
        archive_write_dir(paste0("APR - ", projects_included, " (D).zip"),
                          paste0(general_wd, "/created_files_2"))
        
        setwd(general_wd)
        
        unlink(paste0(getwd(), "/created_files/*"))
        unlink(paste0(getwd(), "/created_files_2/*"))
      }
      
      if(CAPER_relevant) {
        for (question in CAPER_files) {
          if (exists(question)) {
            
            to_write <- get(question)
            
            if(question != "Q4a") {
              to_write <- to_write %>% 
                set_hud_format()
            }
            
            write.csv(get(paste0(question, "_detail")),
                      file.path(paste0("created_files_2/", question, ".csv")),
                      row.names = FALSE)
            
            to_write <- to_write %>% 
              ifnull(., 0) 
            
            to_write[is.na(to_write)] <- ""
            
            write_csv(to_write, file.path(paste0("created_files/", question, ".csv")))
          } else {
            missing_files <- c(missing_files, paste("CAPER -", projects_included, "-", question))
          }
        }
        general_wd <- getwd()
        setwd(paste0(general_wd, "/", folder_name, "/Reports"))
        
        archive_write_dir(paste0("CAPER - ", projects_included, " (A).zip"),
                          paste0(general_wd, "/created_files"))
        archive_write_dir(paste0("CAPER - ", projects_included, " (D).zip"),
                          paste0(general_wd, "/created_files_2"))
        setwd(general_wd)
        
        unlink(paste0(getwd(), "/created_files/*"))
        unlink(paste0(getwd(), "/created_files_2/*"))
      }
    }
    
    rm(list = ls()[ls() %nin% items_to_keep])
    
  }
}

