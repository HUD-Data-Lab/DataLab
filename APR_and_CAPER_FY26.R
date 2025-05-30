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



# this bracket will run everything; use with care!
{
  
  # run this bracket to set up environment for test kit
  {
    # This is a global parameter that should be changed if desired
    generate_new_kits <- TRUE
    run_locally <- TRUE
    
    source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab.R")
  
  # used for building and testing APR on specific projects or groups of projects. To run full test kit use full_project_list
  {
    project_list <- c(
      # 234	#"DataLab - ES-EE ESG I",
      # 93	#"DataLab - ES-NbN ESG",
      # 1304	#"DataLab - HP ESG",
      # 1625	#"DataLab - PSH CoC I",
      # 1343	#"DataLab - RRH CoC I",
      # 1815	#"DataLab - RRH CoC II", # set this to RRH-SSO
      # 1051	#"DataLab - RRH ESG I",
      # 1647	#"DataLab - SO ESG",
      # 1615	#"DataLab - SSO CoC",
      # 389	#"DataLab - TH CoC" # update the funding source to 5
      1814, 1815
    )
    }
  
  
  # used for running all reports for all projects. Use project_list for running specific projects.
  {
    full_project_list <- c(
      1362,	#"DataLab - ES-EE ESG I",
      93,	#"DataLab - ES-NbN ESG",
      1304,	#"DataLab - HP ESG",
      1762,	#"DataLab - PSH CoC I",
      1814,	#"DataLab - RRH CoC I",
      1815,	#"DataLab - RRH CoC II", # set this to RRH-SSO
      1330,	#"DataLab - RRH ESG I",
      1647,	#"DataLab - SO ESG",
      1615,	#"DataLab - SSO CoC",
      389	#"DataLab - TH CoC" # update the funding source to 5
    )
  }
  
  Exit <- Exit %>%
    rename(exit_DateCreated = DateCreated)
  
  Enrollment <- Enrollment %>%
    rename(enroll_DateCreated = DateCreated)
  
  annual_assessment_dates <- Enrollment %>% #What is this trying to get?
    group_by(HouseholdID) %>%
    mutate(start_for_annual = max(EntryDate[RelationshipToHoH == 1]), #Max entry date for each head of household
           years_in_project = trunc((start_for_annual %--% report_end_date) / years(1))) %>% # What does %--% do?
    filter(years_in_project > 0) %>%
    mutate(annual_due = start_for_annual %m+% years(years_in_project)) %>%
    select(HouseholdID, annual_due) %>%
    distinct()
  
  items_to_keep <- c("items_to_keep", ls())
  
  } # End of metric-specific environment setup chunk
    
  
  # loop for running all
  for(project_id in full_project_list) {
    project_list <- c(project_id) 
    
    # run all table creation logic, including questions
    {
      
      # run this chunk to define global variables for use in individual metric testing (i.e. does not include all questions)
      {
      all_program_enrollments <- Enrollment %>% 
        filter(ProjectID %in% project_list) %>% #filter by projectID in Project list.
        left_join(Project %>%
                    select(ProjectID, ProjectType, ProjectName),
                  by = "ProjectID") %>%
        left_join(Exit %>%
                    select(-PersonalID), # Are we removing personalID to avoid the duplication problem? (i.e., PersonalID.X and PersonalID.Y)
                  by = "EnrollmentID")
      
      recent_program_enrollment <- all_program_enrollments %>%
        group_by(PersonalID) %>%
        arrange(desc(EntryDate)) %>% #arrange by most recent entry date  
        slice(1L) %>% #what does 1L do?
        ungroup() 
      
      # get additional client information (age for reporting)
      client_plus <- add_client_info(recent_program_enrollment)  #View(add_client_info) Getting a warning about mac(age, na.rm=TRUE) is returning -inf
      
      household_info <- get_household_info(all_program_enrollments,
                                           return_type = "household")
      
      recent_program_enrollment <- recent_program_enrollment %>%
        left_join(client_plus, by = "PersonalID") %>%
        left_join(household_info, by = "HouseholdID") %>%
        mutate(MoveInDateAdj = case_when(
          !is.na(HoH_HMID) &
            HoH_HMID >= DOB &
            HoH_HMID >= EntryDate &
            (HoH_HMID <= ExitDate |
               is.na(ExitDate)) ~ HoH_HMID,
          !is.na(HoH_HMID) &
            (HoH_HMID <= ExitDate |
               is.na(ExitDate)) ~ EntryDate),
          leaver = ExitDate >= report_start_date &
            ExitDate <= report_end_date & 
            !is.na(ExitDate)) %>%
        add_chronicity_data(.)
      }
      
      
      # CEParticipation <- CEParticipation %>% 
      #   mutate(ProjectID = as.character(ProjectID)) #Changed data type because I was getting a join error in View(Program_information_table)
      
      # Q4a
      # Q4a checked
      {
        Q4a_detail <- "See Q5a_detail.csv"
        
        Q4a <- program_information_table(project_list, #View(program_information_table) #Should RRH Subtype and CES Access be 0=No? or leave as NA?
                                         recent_program_enrollment)
        }
      
      # Q5
      # Q5a
      # Q5a checked
      {
        recent_program_enrollment_dq <- recent_program_enrollment %>%
          filter(ProjectType != 4 |     # This removes enrollments that are street outreach project type or
                   (!is.na(DateOfEngagement) & # if date of engagement is not NA
                      DateOfEngagement <= report_end_date)) # date of engagement <= report end date.
        
        Q5a_detail <- recent_program_enrollment %>%
          add_length_of_time_groups(., EntryDate, #View(add_length_of_time_groups)
                                    ifnull(ExitDate, ymd(report_end_date) + days(1)),  #For end date use ExitDate, unless null then use report_end_date
                                    "APR") %>%
          select(c(all_of(standard_detail_columns), #standard_detail_columns are created in DataLab_Lists.R
                   all_of(demographic_detail_columns),
                          number_of_days)) %>%
          mutate(IncludedInDQ = EnrollmentID %in% recent_program_enrollment_dq$EnrollmentID)
        
        Q5a <- create_summary_table(recent_program_enrollment_dq, "Count.of.Clients.for.DQ") %>% #View(create_summary_table)
          left_join(create_summary_table(recent_program_enrollment, "Count.of.Clients"), 
                    by = "Group")
      }
      
      
      # Q6
      # Q6a
      #Q6a checked
      {
        Q6a_data <- create_dq_Q1(recent_program_enrollment_dq) #View(create_dq_Q1) in datalab_functions.R 
        Q6a <- Q6a_data[[1]]
        Q6a_detail <- Q6a_data[[2]]
      }
      
      # Q6b
      # Q6b checked
      {
        
        Q6b_earlier_enrollment <- recent_program_enrollment_dq %>% #use the DQ dataframe 
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
          select(c(all_of(standard_detail_columns),
                   all_of(demographic_detail_columns),
                   "EnrollmentCoC")) %>%
          mutate(CoC_Valid = EnrollmentCoC %in% valid_cocs,
                 Disability_Check = EnrollmentID %in% recent_disability_check$EnrollmentID) %>%
          group_by(PersonalID) %>%
          slice(1L) %>%
          ungroup() 
        
        Q6b_setup <- Q6b_detail %>%
          mutate(
            Veteran.Status.3.07 = case_when(
              age_group == "Adults" &
                (VeteranStatus == 99 |
                   is.na(VeteranStatus)) ~ "Missing",
              VeteranStatus %in% c(8, 9) & 
                age_group == "Adults" ~ "DK/PNTA",
              age_group == "Children" &
                VeteranStatus == 1 ~ "Data.Issues"),
            Project.Start.Date.3.10 = case_when(
              EnrollmentID %in% Q6b_earlier_enrollment$EnrollmentID |
                (EntryDate > ExitDate &
                   !is.na(ExitDate)) ~ "Data.Issues"),
            Relationship.to.Head.of.Household.3.15 = case_when(
              is.na(RelationshipToHoH) ~ "Missing",
              RelationshipToHoH %nin% 1:5 |
                HouseholdID %in% Q6b_hoh_count$HouseholdID ~ "Data.Issues"),
            EnrollmentCoC.3.16 = case_when(
              RelationshipToHoH == 1 & is.na(EnrollmentCoC) ~ "Missing",
              RelationshipToHoH == 1 & !CoC_Valid ~ "Data.Issues"),
            Disabling.Condition.3.08 = case_when(
              DisablingCondition == 99 |
                is.na(DisablingCondition) ~ "Missing",
              DisablingCondition %in% c(8, 9) ~ "DK/PNTA",
              (DisablingCondition == 0 & Disability_Check) ~ "Data.Issues")) %>%
          select(PersonalID, Veteran.Status.3.07, Project.Start.Date.3.10,
                 Relationship.to.Head.of.Household.3.15, EnrollmentCoC.3.16,
                 Disabling.Condition.3.08) %>%
          pivot_longer(!PersonalID, names_to = "Group", values_to = "DQ_flag") 
        
        Q6b <- Q6b_setup %>%
          select(Group) %>%
          distinct() %>%
          full_join(Q6b_setup %>%
                      full_join(data.frame(DQ_flag = c("DK/PNTA", "Missing", "Data.Issues")),
                                by = "DQ_flag") %>%
                      group_by(Group, DQ_flag) %>%
                      summarise(count = n_distinct(PersonalID, na.rm = TRUE)) %>%
                      ungroup() %>%
                      pivot_wider(names_from = DQ_flag, values_from = count) %>%
                      filter(!is.na(Group)) %>%
                      select(c("Group", "DK/PNTA", "Missing", "Data.Issues")) %>%
                      adorn_totals("col") %>%
                      mutate(Percent.of.Issue.Rate = decimal_format(
                        case_when(
                          Group == "Veteran.Status.3.07" ~ Total / (Q5a$Count.of.Clients.for.DQ[2] +
                                                                      Q5a$Count.of.Clients.for.DQ[3]),
                          Group == "EnrollmentCoC.3.16" ~ Total / (Q5a$Count.of.Clients.for.DQ[14] + 
                                                                     Q5a$Count.of.Clients.for.DQ[15]),
                          TRUE ~ Total / Q5a$Count.of.Clients.for.DQ[1]), 4)), #Refer to DQ check on Q5a
                    by = "Group")  %>%
          ifnull(., 0)
          
      }
      
      # Q6c
      # Q6c checked
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
          mutate(
            Destination.3.12 = case_when(  
              !is.na(ExitDate) ~ case_when(
                is.na(Destination) |
                  Destination %in% c(30, 99) ~ "Missing",
                Destination %in% c(8, 9) ~ "DK/PNTA")),
            Income.and.Sources.4.02.at.Start = case_when(
              (RelationshipToHoH == 1 |
                 age_group == "Adults") ~ case_when(
                   is.na(enroll_IncomeFromAnySource) |
                     enroll_IncomeFromAnySource == 99 ~ "Missing",
                   enroll_IncomeFromAnySource %in% c(8, 9) ~ "DK/PNTA", 
                   (enroll_IncomeFromAnySource == 0 &
                      !is.na(enroll_number_of_sources) &
                      enroll_number_of_sources > 0) |
                     (enroll_IncomeFromAnySource == 1 &
                        (is.na(enroll_number_of_sources) |
                           enroll_number_of_sources == 0)) ~ "Data.Issues")),
            Income.and.Sources.4.02.at.Annual.Assessment = case_when(
              !is.na(annual_due) &
                is.na(ExitDate) &
                (RelationshipToHoH == 1 |
                   age_group == "Adults") ~ case_when(
                     is.na(annual_IncomeFromAnySource) |
                       annual_IncomeFromAnySource == 99 ~ "Missing",
                     annual_IncomeFromAnySource %in% c(8, 9) ~ "DK/PNTA", 
                     (annual_IncomeFromAnySource == 0 &
                        !is.na(annual_number_of_sources) &
                        annual_number_of_sources > 0) |
                       (annual_IncomeFromAnySource == 1 &
                          (is.na(annual_number_of_sources) |
                             annual_number_of_sources == 0)) ~ "Data.Issues")),
            Income.and.Sources.4.02.at.Exit = case_when(
              !is.na(ExitDate) &
                (RelationshipToHoH == 1 |
                   age_group == "Adults") ~ case_when(
                     is.na(exit_IncomeFromAnySource) |
                       exit_IncomeFromAnySource == 99 ~ "Missing",
                     exit_IncomeFromAnySource %in% c(8, 9) ~ "DK/PNTA",
                     (exit_IncomeFromAnySource == 0 &
                        !is.na(exit_number_of_sources) &
                        exit_number_of_sources > 0) |
                       (exit_IncomeFromAnySource == 1 &
                          (is.na(exit_number_of_sources) |
                             exit_number_of_sources == 0)) ~ "Data.Issues"))) %>%
          group_by(PersonalID) %>%
          slice(1L) %>%
          ungroup() 
        
        Q6c_setup <- Q6c_detail %>%
          select(PersonalID, Destination.3.12, Income.and.Sources.4.02.at.Start,
                 Income.and.Sources.4.02.at.Annual.Assessment,
                 Income.and.Sources.4.02.at.Exit) %>%
          mutate(across(everything(), as.character)) %>%
          pivot_longer(!PersonalID, names_to = "Group", values_to = "DQ_flag") 
        
        Q6c <- Q6c_setup %>%
          select(Group) %>%
          distinct() %>%
          full_join(Q6c_setup %>%
                      full_join(data.frame(DQ_flag = c("DK/PNTA", "Missing", "Data.Issues")),
                                by = "DQ_flag") %>%
                      group_by(Group, DQ_flag) %>%
                      summarise(count = n_distinct(PersonalID, na.rm = TRUE)) %>%
                      ungroup() %>%
                      pivot_wider(names_from = DQ_flag, values_from = count) %>%
                      filter(!is.na(Group)) %>%
                      select(c("Group", "DK/PNTA", "Missing", "Data.Issues")) %>%
                      adorn_totals("col") %>%
                      mutate(Percent.of.Issue.Rate = decimal_format(
                        case_when(
                          Group == "Destination.3.12" ~ Total / Q5a$Count.of.Clients.for.DQ[5],
                          Group == "Income.and.Sources.4.02.at.Start" ~ Total / (Q5a$Count.of.Clients.for.DQ[2] + 
                                                                                   Q5a$Count.of.Clients.for.DQ[15]),
                          Group == "Income.and.Sources.4.02.at.Annual.Assessment" ~ Total / Q5a$Count.of.Clients.for.DQ[16],
                          Group == "Income.and.Sources.4.02.at.Exit" ~ Total / Q5a$Count.of.Clients.for.DQ[7]), 4)), #Refer to DQ check on Q5a
                    by = "Group")%>%
          ifnull(., 0)
      }
      
      # Q6d
      {
        Entering.into.project.type <- c("ES-EE.ES-NbN.SH.Street.Outreach", "TH", 
                                        "PH.all", "SSO.Day.Shelter.HP", "CE")
        
        Q6d_detail <- recent_program_enrollment_dq %>% 
          filter(EntryDate >= mdy("10/1/2016") & #Just making a note that this date is hard coded ----
                   ProjectType %in% c(0, 1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13, 14)) %>%
          keep_adults_and_hoh_only() %>% #View(keep_adults_and_hoh_only)
          select(c("ProjectType", all_of(standard_detail_columns), 
                   all_of(lot_homeless_detail_columns))) %>%
          mutate(Entering.into.project.type = case_when(
            ProjectType %in% c(0, 1, 4, 8) ~ "ES-EE.ES-NbN.SH.Street.Outreach",
            ProjectType == 2 ~ "TH",
            ProjectType %in% c(3, 9, 10, 13) ~ "PH.all",
            ProjectType %in% c(6, 11, 12) ~ "SSO.Day.Shelter.HP",
            ProjectType == 14 ~ "CE"),
            missing_institution = Entering.into.project.type != "ES-EE.ES-NbN.SH.Street.Outreach" &
              LivingSituation %in% 200:299 &
              (LengthOfStay %in% c(8, 9, 99) |
                 is.na(LengthOfStay)),
            missing_housing = Entering.into.project.type != "ES-EE.ES-NbN.SH.Street.Outreach" &
              (LivingSituation %in% c(0:99, 300:499) |
                 is.na(LivingSituation)) &
              (LengthOfStay %in% c(8, 9, 99) |
                 is.na(LengthOfStay)),
            include_for_EFG = Entering.into.project.type == "ES-EE.ES-NbN.SH.Street.Outreach" |
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
        
        
        Q6d[Q6d$Entering.into.project.type == "ES-EE.ES-NbN.SH.Street.Outreach",  #What is this section doing? It doesn't seem to be writing back into Q6d.
            c("Missing.time.in.institution.3.917.2",
              "Missing.time.in.housing.3.917.2")] <- NA
      }
      
      # Q6e checked
      {
        Q6e_detail <- recent_program_enrollment_dq %>%
          select(c(all_of(standard_detail_columns)), "enroll_DateCreated",
                 "exit_DateCreated") %>%
          mutate(days_for_entry = case_when(
            EntryDate >= report_start_date &
              EntryDate <= report_end_date ~ floor(
                interval(EntryDate, floor_date(enroll_DateCreated, "day")) / days(1))),
            TimeForEnrollmentEntry = case_when(
              days_for_entry < 0 ~ "< 0 days",
              days_for_entry == 0 ~ "0 days",
              days_for_entry <= 3 ~ "1-3 days",
              days_for_entry <= 6 ~ "4-6 days",
              days_for_entry <= 10 ~ "7-10 days",
              !is.na(days_for_entry) ~ "11+ days"),
            days_for_exit = case_when(!is.na(ExitDate) ~ floor(
              interval(ExitDate, floor_date(exit_DateCreated, "day")) / days(1))),
            TimeForExitEntry = case_when(
              days_for_exit < 0 ~ "< 0 days",
              days_for_exit == 0 ~ "0 days",
              days_for_exit <= 3 ~ "1-3 days",
              days_for_exit <= 6 ~ "4-6 days",
              days_for_exit <= 10 ~ "7-10 days",
              !is.na(days_for_exit) ~ "11+ days"))
        
        Q6e <- data.frame(TimeForEntry = c("< 0 days", "0 days", "1-3 days", 
                                           "4-6 days", "7-10 days", "11+ days")) %>%
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
      
      # Q6f checked
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
                   (ProjectType %in% c(0, 1, 4))) %>%
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
      # Q7a checked
      {
        Q7a_detail <- recent_program_enrollment %>%
          select(all_of(housing_program_detail_columns),
                 all_of(demographic_detail_columns))
        
        Q7a_all <- Q7a_detail %>%
          mutate(client_group = "Total") %>%
          return_household_groups(., client_group, c("Total")) 
        
        Q7a_moved_in <- Q7a_detail %>%
          filter(MoveInDateAdj <= report_end_date) %>% 
          mutate(client_group = "For PSH & RRH - the total persons served who moved into housing") %>%
          return_household_groups(., client_group, "For PSH & RRH - the total persons served who moved into housing") 
        
        Q7a <- Q7a_detail %>%
          return_household_groups(., age_group, age_groups) %>%
          rename(client_group = age_group) %>%
          union(Q7a_all) %>%
          union(Q7a_moved_in)
        
      }
      
      # Q7b checked
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
            left_join(recent_program_enrollment %>%
                        select(PersonalID, household_type),
                      by = "PersonalID") %>%
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
      
      # Q8a checked
      {
        Q8a_data <- households_served_table(recent_program_enrollment) #View(households_served_table)
        Q8a <- Q8a_data[[1]]
        Q8a_detail <- Q8a_data[[2]]
      }
      
      # Q8b checked
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
                        select(HouseholdID, HoH_HMID),
                      by = "HouseholdID") %>%
            left_join(recent_program_enrollment %>%
                        select(PersonalID, household_type),
                      by = "PersonalID") %>%
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
      
      # Q9a checked
      {
        # same question as before--is this the most recent CLS for the *person* or the *enrollment*?
        recent_CLS_for_Q9 <- recent_program_enrollment %>%
          left_join(CurrentLivingSituation %>%
                      select(-c(PersonalID, ExportID)) %>%
                      rename(CLS_InformationDate = InformationDate,
                             CLS = CurrentLivingSituation), by = "EnrollmentID",
                    multiple = "all") %>%
          filter(CLS_InformationDate >= report_start_date &
                   CLS_InformationDate <= report_end_date &
                   (CLS_InformationDate <= DateOfEngagement |
                      is.na(DateOfEngagement)))
        
        # this one specifies enrollment as directed in the programming specifications
        only_CLS_for_Q9 <- recent_program_enrollment %>%
          left_join(CurrentLivingSituation %>%
                      select(-ExportID) %>%
                      rename(CLS_InformationDate = InformationDate,
                             CLS = CurrentLivingSituation), by = "EnrollmentID",
                    multiple = "all") %>%
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
      
      # Q9b checked
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

      
      # Q11 checked
      {
        Q11_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "age", "detailed_age_group") %>%
          left_join(Client %>%
                      select(PersonalID, DOB),
                    by = "PersonalID")
        
        Q11 <- Q11_detail %>%
          create_age_groups(.)
      }
       
      # Q12 checked
      {
        Q12_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), age_group, HoH_HMID) %>%
          left_join(Client %>%
                      select(PersonalID, all_of(unname(race_columns)), RaceNone),
                    by = "PersonalID")
        
        Q12 <- Q12_detail %>%
          ifnull(., 0) %>%
          left_join(race_info, #Race_info created from DataLab_lists.R line 261
                    by = (unname(race_columns))) %>% 
          mutate(across(
            all_of(unname(race_columns)),
            ~ as.numeric(.)),
            race_count = rowSums(across(all_of(unname(race_columns))),
                                   na.rm = TRUE),
            race_tabulation = case_when(
              race_count %in% 1:2 ~ race_name_list,
              race_count > 2 &
                HispanicLatinaeo == 1 ~ "Multiracial – more than 2 races/ethnicity, with one being Hispanic/Latina/e/o",
              race_count > 2 ~ "Multiracial – more than 2 races, where no option is Hispanic/Latina/e/o",
              RaceNone %in% c(8, 9) ~ "Client Doesn’t Know/Prefers Not to Answer",
              TRUE ~ "Data Not Collected"
            )
          ) %>%
          return_household_groups(., race_tabulation, 
                                  c(race_info$race_name_list, 
                                    "Multiracial – more than 2 races/ethnicity, with one being Hispanic/Latina/e/o",
                                    "Multiracial – more than 2 races, where no option is Hispanic/Latina/e/o",
                                    "Client Doesn’t Know/Prefers Not to Answer",
                                    "Data Not Collected")) %>%
          adorn_totals("row")
        
      }
      
      # Q13a1 checked
      {
        Q13a1_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "age_group", 
                 "new_veteran_status", "chronic") %>%
          inner_join(disability_table %>%
                       filter(DataCollectionStage == 1 &
                                DisabilityResponse != 0), by ="EnrollmentID",
                     multiple = "all")
        
        Q13a1 <- Q13a1_detail %>%
          return_household_groups(., disability_name, disability_list,
                                  split_by_age = TRUE)
      }
      
      # Q13b1 checked
      {
        Q13b1_detail<- recent_program_enrollment %>%
          select(all_of(standard_detail_columns), "age_group", 
                 "new_veteran_status", "chronic") %>%
          filter(!is.na(ExitDate)) %>%
          inner_join(disability_table %>%
                       filter(DataCollectionStage == 3 &
                                DisabilityResponse != 0), by ="EnrollmentID",
                     multiple = "all")
        
        Q13b1 <- Q13b1_detail %>%
          return_household_groups(., disability_name, disability_list,
                                  split_by_age = TRUE)
      }
      
      # Q13c1 checked
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
                       inner_join(disability_table, 
                                 by = c("EnrollmentID", "last_date" = "InformationDate"),
                                 multiple = "all"), 
                     by ="EnrollmentID",
                     multiple = "all")
        
        Q13c1 <- Q13c1_detail  %>%
          filter(DisabilityResponse != 0) %>%
          return_household_groups(., disability_name, disability_list,
                                  split_by_age = TRUE)
      }
      
      # Q13a2 checked
      {
        Q13a2_detail <- "See Q13a1_detail.csv"
        Q13a2 <- recent_program_enrollment %>%
          left_join(Q13a1_detail %>%
                      group_by(PersonalID) %>%
                      summarise(disability_count = sum(disabilities)) %>%
                      ungroup(), 
                    by = "PersonalID") %>%
          condition_count_groups() %>%
          return_household_groups(., disability_count_group, disability_count_group_list,
                                  split_by_age = TRUE) %>%
          adorn_totals("row")
      }
      
      # Q13b2 checked
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
      
      # Q13c2 checked
      {
        Q13c2_detail <- "See Q13c1_detail.csv"
        Q13c2 <- recent_program_enrollment %>%
          filter(is.na(ExitDate)) %>%
          left_join(Q13c1_detail %>%
                      filter(DisabilityResponse != 0) %>%
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
      
      # Q14a checked
      {
        Q14 <- recent_program_enrollment %>%
          keep_adults_and_hoh_only() %>%
          left_join(HealthAndDV %>%
                      filter(InformationDate <= report_end_date) %>%
                      arrange(desc(InformationDate)) %>%
                      group_by(EnrollmentID) %>%
                      slice(1L) %>%
                      ungroup() %>%
                      select(EnrollmentID, DomesticViolenceSurvivor, WhenOccurred),
                    by = "EnrollmentID")
        
        Q14a_detail <- Q14 %>%
          select(all_of(standard_detail_columns), DomesticViolenceSurvivor) %>%
          mutate(dv_experience = case_when(DomesticViolenceSurvivor == 1 ~ "Yes",
                                           DomesticViolenceSurvivor == 0 ~ "No",
                                           DomesticViolenceSurvivor %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
                                           TRUE ~ "Data.Not.Collected")) 
        
        Q14a <- Q14a_detail %>%
          return_household_groups(., dv_experience, y_n_dkr_dnc_list) %>%
          adorn_totals("row")
      }
      
      # Q14b checked
      {
        when_occurred_options <- data.frame(
          WhenOccurred = c(1, 2, 3, 4, 8, 9, 99, NA),
          when_occurred = c(
            "Within the past three months",
            "Three to six months ago",
            "Six months to one year ago",
            "One year or more",
            "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
            "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
            "Data.Not.Collected",
            "Data.Not.Collected"
          )
        )
        
        Q14b_detail <- Q14 %>%
          select(all_of(standard_detail_columns), DomesticViolenceSurvivor,
                 WhenOccurred) %>%
          filter(DomesticViolenceSurvivor == 1) %>%
          inner_join(when_occurred_options,
                     by = "WhenOccurred")
        
        Q14b <- Q14b_detail %>%
          return_household_groups(., when_occurred, 
                                  when_occurred_options$when_occurred[c(1:5, 7)]) %>%
          adorn_totals("row")
      }
      
      # Q15 checked
      {
        ##  added a comment to the document, but putting here too--easier to
        ##  put in value order than an arbitrary one
        Q15_detail <- recent_program_enrollment %>%
          keep_adults_and_hoh_only() %>%
          select(all_of(standard_detail_columns), LivingSituation)
        
        Q15 <- Q15_detail %>%
          create_prior_residence_groups(.) %>%
          filter(LocationDescription != "Other Situations")
      }
      
      
      # Q16 checked
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
          adorn_totals("row", name = "Total adults")

        #Recode attempt: Error because Q16$total_income_group is an ordered factor
        #Q16.2 <- as.data.frame(Q16)
        #Q16.2[Q16.2 == "No Annual Required"] <- "Number of adult stayers not yet required to have an annual assessment"
        
       
      }
      
      # Q17 checked
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
      
      # Q18 checked
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
      
      # Q19a1 checked
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
            filter(!is.na(calculated_total_income)) %>%
            `colnames<-`(c("PersonalID", paste0(period, "_", needed_columns[1:2]), 
                           paste0(period, "_total_amount"))) %>%
            ifnull(., 0)
          
          assign(paste0(period, "_income_for_changes"), income_for_changes)
        }
        
        Q19a1_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns)) %>%
          keep_adults_only() %>%
          filter(PersonalID %in% intersect(
            entry_income_for_changes$PersonalID,
            annual_income_for_changes$PersonalID)) %>%
          left_join(entry_income_for_changes, by = "PersonalID") %>%
          left_join(annual_income_for_changes, by = "PersonalID")
        
        for(row in c("earned", "other", "total")) {
          titles <- paste(c("Number of Adults with", "Average Change in"), 
                          str_to_title(row), "Income")
          data <- entry_income_for_changes %>%
            filter(PersonalID %in% Q19a1_detail$PersonalID) %>%
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
      
      # Q19a2 checked
      {
        for(period in entry_annual_exit[c(1, 3)]) {
          income_for_changes <- get(paste0(period, "_income")) %>%
            keep_adults_only() %>%
            determine_total_income(., annual = period == "annual") %>%
            mutate(earned_amount = if_else(calculated_total_income > 0 &
                                             earned_income > 0, 
                                           earned_income, 0),
                   other_amount = if_else(calculated_total_income > 0 &
                                            earned_income < calculated_total_income,
                                          calculated_total_income - earned_income, 0)) %>%
            select(c(PersonalID, all_of(needed_columns))) %>%
            filter(!is.na(calculated_total_income)) %>%
            `colnames<-`(c("PersonalID", paste0(period, "_", needed_columns[1:2]), 
                           paste0(period, "_total_amount"))) %>%
            ifnull(., 0)
          
          assign(paste0(period, "_income_for_changes"), income_for_changes)
        }
        
        
        Q19a2_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns)) %>%
          keep_adults_only() %>%
          filter(PersonalID %in% intersect(
            entry_income_for_changes$PersonalID,
            exit_income_for_changes$PersonalID)) %>%
          left_join(entry_income_for_changes, by = "PersonalID")%>%
          left_join(exit_income_for_changes, by = "PersonalID")
        
        for(row in c("earned", "other", "total")){
          titles <- paste(c("Number of Adults with", "Average Change in"), 
                          str_to_title(row), "Income")
          data <- entry_income_for_changes %>%
            filter(PersonalID %in% Q19a2_detail$PersonalID) %>%
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
      
      # Q19b checked
      {
        Q19b_detail <- Q17_detail %>%
          filter(EnrollmentID %in% exit_income$EnrollmentID[exit_income$IncomeFromAnySource %in% c(0, 1)]) %>%
          inner_join(recent_program_enrollment %>% 
                       select(EnrollmentID, DisablingCondition) %>%
                       filter(DisablingCondition %in% c(0, 1)),
                     by = "EnrollmentID")
        
        Q19b <- exit_income %>%
          income_hh_type_disabling_condition_table(.)
      }
      
      # Q20a checked
      {
        benefit_entry_annual_exit <- paste0(c("", "benefit_", ""), entry_annual_exit)
        
        recent_income <- recent_program_enrollment %>%
          filter(is.na(ExitDate) &
                   HouseholdID %in% annual_income$HouseholdID[annual_income$RelationshipToHoH == 1 &
                                                                !is.na(annual_income$IncomeBenefitsID)] &
                   RelationshipToHoH != 1) %>%
          left_join(IncomeBenefits %>%
                      group_by(EnrollmentID) %>%
                      arrange(desc(InformationDate)) %>%
                      slice(1L) %>%
                      ungroup() %>%
                      select(-c(PersonalID, ExportID)),
                    by = "EnrollmentID")
        
        benefit_annual_income <- annual_income %>%
          filter(RelationshipToHoH == 1 |
                   HouseholdID %nin% annual_income$HouseholdID[annual_income$RelationshipToHoH == 1 &
                                                                 !is.na(annual_income$IncomeBenefitsID)]) %>%
          full_join(recent_income,
                    by = intersect(colnames(recent_income),
                                   colnames(annual_income)))
        
        Q20a_detail <- recent_program_enrollment %>%
          keep_adults_only() %>%
          select(all_of(standard_detail_columns), new_veteran_status, chronic)
        
        for(period in benefit_entry_annual_exit) {
          data <- get(paste0(period, "_income")) %>%
            filter(period != "exit" |
                     paste(HouseholdID, ExitDate) %in% paste(
                       all_program_enrollments$HouseholdID[all_program_enrollments$RelationshipToHoH == 1],
                       all_program_enrollments$ExitDate[all_program_enrollments$RelationshipToHoH == 1])) %>%
          select(EnrollmentID, all_of(benefit_list), BenefitsFromAnySource)
          
          Q20a_detail <- Q20a_detail %>%
            left_join(data %>%
                        `colnames<-`(paste0(period, "_", colnames(data))),
                      by = c("EnrollmentID" = paste0(period, "_EnrollmentID")))
        }
        
        Q20a <- create_benefit_groups(recent_program_enrollment)
      }
      
      # Q20b checked
      #Note row header one or more source(s) is different from specs (1 + Source(s)). How exact do we want to be?
      {
        Q20b_detail <- "See Q20a_detail.csv"
        for(period in benefit_entry_annual_exit) {
          
          data <- get(paste0(period, "_income")) %>%
            keep_adults_only() %>%
            filter(period != "exit" |
                     paste(HouseholdID, ExitDate) %in% paste(
                       all_program_enrollments$HouseholdID[all_program_enrollments$RelationshipToHoH == 1],
                       all_program_enrollments$ExitDate[all_program_enrollments$RelationshipToHoH == 1])) %>%
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
          
          if(period == "benefit_annual"){
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
        
        benefit_count <- c("No sources", "One or more source(s)", "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
                           "Not collected/annual assessment not due")
        
        Q20b <- as.data.frame(benefit_count) %>%
          left_join(entry_benefit_counts, by = "benefit_count") %>%
          left_join(benefit_annual_benefit_counts, by = "benefit_count") %>%
          left_join(exit_benefit_counts, by = "benefit_count") %>%
          rename(BenefitGroup = benefit_count,
                 Benefit.at.Start = entry_people,
                 Benefit.at.Latest.Annual.Assessment.for.Stayers = benefit_annual_people,
                 Benefit.at.Exit.for.Leavers = exit_people) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      
      # Q21 checked
      {
        Q21_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns))
        
        insurance_list <- c("Medicaid", "Medicare", "SCHIP", "VHAServices", 
                            "EmployerProvided", "COBRA", "PrivatePay", "StateHealthIns",
                            "IndianHealthServices", "OtherInsurance")
        
        for(period in entry_annual_exit) {
          
          data <- get(paste0(period, "_income")) %>%
            mutate(insurance_count = ifnull(Medicaid == 1, 0) + ifnull(Medicare == 1, 0) +
                     ifnull(SCHIP == 1, 0) + ifnull(VHAServices == 1, 0) +
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
                  InsuranceFromAnySource %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
                  TRUE ~ "Data.Not.Collected"),
              insurance_count == 1 ~ "One source of insurance",
              TRUE ~ "More than one source of insurance")) %>%
            group_by(InsuranceType) %>%
            summarize(!!paste0(period, "Clients") := n_distinct(PersonalID, na.rm = TRUE))
          
          assign(paste0(period, "_insurance_types"), insurance_types %>%
                   rbind(insurance_present))
        }
        
        full_insurance_list <- c(InsuranceTypes$OfficialInsuranceName, 
                                 c("No health insurance", "Client.Does.Not.Know.or.Prefers.Not.to.Answer", "Data.Not.Collected", 
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
      
      # Q22a1 checked
      {
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
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q22a2 checked
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
        
        Q22a2 <- length_of_time_groups(column_name = "CAPER_enrollment_length_group") %>%
          left_join(Q22a2_total, by = "CAPER_enrollment_length_group") %>%
          left_join(Q22a2_leavers, by = "CAPER_enrollment_length_group") %>%
          left_join(Q22a2_stayers, by = "CAPER_enrollment_length_group") %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q22b checked
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
      
      # Q22c checked
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
          filter(EnrollmentID %nin% exited_without_move_in_data$EnrollmentID) %>%
          full_join(exited_without_move_in_data,
                    by = colnames(exited_without_move_in_data))
      }
      
      # Q22d checking
      {
        Q22d_detail <- "See Q22a2_detail.csv"
        
        Q22d <- Q22a1_detail %>%
          mutate(
            CAPER_enrollment_length_group = case_when(
              days_enrolled >= 731 ~ "731 days or more",
              TRUE ~ CAPER_enrollment_length_group)) %>%
          return_household_groups(., CAPER_enrollment_length_group, 
                                  length_of_participation$enrollment_length_group) %>%
          adorn_totals("row") %>%
          ifnull(., 0) 
      }
      
      #Q22e checked
      {
        Q22e_detail <- recent_program_enrollment %>%
          select(c("ProjectType", all_of(housing_program_detail_columns), "age",
                   "HoH_EntryDate", "DateToStreetESSH", "HoH_ADHS")) %>%
          create_time_prior_to_housing() %>%
        mutate(days_prior_to_housing = case_when(is.na(housing_date) ~ "Not yet moved into housing",
                                                TRUE ~ days_prior_to_housing))
        
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
      
      # Q22f checked
      {
        Q22f_detail <- recent_program_enrollment  %>%
          select(c("ProjectType", "ProjectID", all_of(housing_program_detail_columns), "age",
                   "HoH_EntryDate", "DateToStreetESSH", "HoH_ADHS")) %>%
          create_time_to_move_in() %>%
          #either move-in w/in report or exit w/in report
          filter(((MoveInDateAdj >= report_start_date & 
                     MoveInDateAdj <= report_end_date) | 
                    leaver) & 
                   ProjectType %in% c(3, 13)) %>%
          left_join(Client %>%
                      select(PersonalID, all_of(unname(race_columns)), RaceNone),
                    by = "PersonalID") %>%
          left_join(race_info, #Race_info created from DataLab_lists.R line 261
                    by = all_of(unname(race_columns))) %>% 
          mutate(across(
            all_of(unname(race_columns)),
            ~ as.numeric(.)),
            race_count = rowSums(across(all_of(unname(race_columns))),
                                 na.rm = TRUE),
            race_tabulation = case_when(
              race_count == 1 ~ race_name_list,
              race_count > 1 &
                HispanicLatinaeo == 1 ~ "At Least 1 Race and Hispanic/Latina/e/o",
              race_count > 1 ~ "Multi-racial (does not include Hispanic/Latina/e/o)",
              TRUE ~ "Unknown (Doesn’t Know, Prefers not to Answer, Data not Collected)"),
            include_type = case_when(
              MoveInDateAdj >= report_start_date & 
                MoveInDateAdj <= report_end_date ~ "moved_in",
              leaver ~ "exit_only"))
        
        Q22f_calcs <- Q22f_detail %>%
          group_by(race_tabulation) %>%
          summarise(
            Persons.Moved.Into.Housing = n_distinct(PersonalID[include_type == "moved_in"], 
                                                    na.rm = TRUE),
            Persons.Exited.Without.Move.In = n_distinct(PersonalID[include_type == "exit_only"], 
                                                        na.rm = TRUE),
            Average.time.to.Move.In =  mean(days_to_house[include_type == "moved_in"], 
                                            na.rm = TRUE), 
            Median.time.to.Move.In = median(days_to_house[include_type == "moved_in"], 
                                            na.rm = TRUE)) %>% 
          ungroup() 
        
        Q22f <- race_list_expanded %>% 
          as.data.frame(nm = "race_tabulation") %>% 
          left_join(Q22f_calcs,
                    by = "race_tabulation") %>% 
          ifnull(., 0) %>%
          mutate(Average.time.to.Move.In = sprintf('%.4f', Average.time.to.Move.In),
                 Median.time.to.Move.In = sprintf('%.4f', Median.time.to.Move.In),
                 across(everything(), as.character)) %>%
          pivot_longer(!race_tabulation, names_to = "measure", values_to = "value") %>% 
          pivot_wider(names_from = "race_tabulation", values_from = "value")
      
      }
      
      # Q22g - checked
      {
        Q22g_detail <- recent_program_enrollment  %>%
          select(c("ProjectType", "ProjectID", all_of(housing_program_detail_columns), "age",
                   "HoH_EntryDate", "DateToStreetESSH", "HoH_ADHS")) %>%
          create_time_prior_to_housing() %>%
          filter(ProjectType %in% c(0, 1, 2, 3, 8, 9, 13) &
                   days_prior_to_housing != "Data.Not.Collected") %>%
          left_join(Client %>%
                      select(PersonalID, all_of(unname(race_columns)), RaceNone),
                    by = "PersonalID") %>%
          left_join(race_info, #Race_info created from DataLab_lists.R line 261
                    by = all_of(unname(race_columns))) %>% 
          mutate(across(
            all_of(unname(race_columns)),
            ~ as.numeric(.)),
            race_count = rowSums(across(all_of(unname(race_columns))),
                                 na.rm = TRUE),
            race_tabulation = case_when(
              race_count == 1 ~ race_name_list,
              race_count > 1 &
                HispanicLatinaeo == 1 ~ "At Least 1 Race and Hispanic/Latina/e/o",
              race_count > 1 ~ "Multi-racial (does not include Hispanic/Latina/e/o)",
              TRUE ~ "Unknown (Doesn’t Know, Prefers not to Answer, Data not Collected)"),
            include_type = case_when(
              !is.na(MoveInDateAdj) &
                MoveInDateAdj <= report_end_date ~ "moved_in",
              TRUE ~ "not_yet_moved_in"))
        
      Q22g_calcs <- Q22g_detail %>% 
        group_by(race_tabulation) %>%
        summarise(
          Persons.Moved.Into.Housing = n_distinct(PersonalID[include_type == "moved_in"], 
                                                  na.rm = TRUE),
          Persons.Not.Yet.Moved.Into.Housing = n_distinct(PersonalID[include_type == "not_yet_moved_in"], 
                                                      na.rm = TRUE),
          Average.time.to.Move.In =  mean(number_of_days[include_type == "moved_in"], 
                                          na.rm = TRUE), 
          Median.time.to.Move.In = median(number_of_days[include_type == "moved_in"], 
                                          na.rm = TRUE)) %>% 
        ungroup() 
      
      Q22g <- race_list_expanded %>% 
        as.data.frame(nm = "race_tabulation") %>% 
        left_join(Q22g_calcs,
                  by = "race_tabulation") %>% 
        ifnull(., 0) %>%
        mutate(Average.time.to.Move.In = sprintf('%.4f', Average.time.to.Move.In),
               Median.time.to.Move.In = sprintf('%.4f', Median.time.to.Move.In),
               across(everything(), as.character)) %>%
        pivot_longer(!race_tabulation, names_to = "measure", values_to = "value") %>% 
        pivot_wider(names_from = "race_tabulation", values_from = "value")
      }
      
      # Q23c checked
      {
        Q23c_detail <- recent_program_enrollment %>%
          filter(!is.na(ExitDate)) %>%
          select(ProjectType, all_of(standard_detail_columns), Destination,
                 new_veteran_status, youth)
        
        Q23c <- create_destination_groups(Q23c_detail) %>%
          mutate(across(everything(), as.character))
        
        Q23c[42, 2:6] <- c(decimal_format(as.numeric(Q23c[42, 2:6]), 4))
      }
      
      # Q23d checked
      {
        Q23d_detail <- recent_program_enrollment %>%
          select(c("ProjectType", "ProjectID", all_of(housing_program_detail_columns),
                   "Destination", "DestinationSubsidyType", "new_veteran_status",
                   "youth")) %>%
          filter(ProjectType != 12 & 
                   leaver & 
                   Destination == 435) %>% 
          left_join(subsidy_list, join_by(DestinationSubsidyType == Field))
        
        Q23d <- Q23d_detail %>% 
          return_household_groups(., Response, subsidy_list$Response) %>% 
          adorn_totals("row") %>% 
          ifnull(., 0)
      }
      
      # Q23e checked
      {
      Q23e_detail <- recent_program_enrollment %>% 
        # leavers in range
        filter(leaver) %>% 
        select(., c(all_of(housing_program_detail_columns), "Destination")) %>%
        # get APR destination categories 
        left_join(select(ResidenceUses, c("Location", "APR_LocationOrder", "APR_LocationGroup")) ,
                  join_by(Destination == Location)) %>%
        # get race data 
        left_join(Client %>%
                    select(PersonalID, all_of(unname(race_columns)), RaceNone),
                  by = "PersonalID") %>% 
        left_join(race_info, #Race_info created from DataLab_lists.R line 261
                  by = all_of(unname(race_columns))) %>% 
        mutate(across(
          all_of(unname(race_columns)),
          ~ as.numeric(.)),
          race_count = rowSums(across(all_of(unname(race_columns))),
                               na.rm = TRUE),
          race_tabulation = case_when(
            race_count == 1 ~ race_name_list,
            race_count > 1 &
              HispanicLatinaeo == 1 ~ "At Least 1 Race and Hispanic/Latina/e/o",
            race_count > 1 ~ "Multi-racial (does not include Hispanic/Latina/e/o)",
            TRUE ~ "Unknown (Doesn’t Know, Prefers not to Answer, Data not Collected)")
        )
      
      # Gets counts by destination and race/ethn category
      Q23e_calcs <- race_list_expanded %>% 
        as.data.frame(nm = "race_tabulation") %>% 
        left_join(Q23e_detail,
                  by = "race_tabulation",
                  multiple = "all") %>%
        group_by(race_tabulation, APR_LocationGroup) %>%
          summarise(measure = n_distinct(PersonalID, na.rm = TRUE)) %>%
        ungroup()  %>% 
        pivot_wider(names_from = "race_tabulation", values_from = "measure") %>%
        filter(!is.na(APR_LocationGroup)) %>%
        mutate(APR_LocationGroup = paste(APR_LocationGroup, "Situations"))
      
      Q23e <- Q23c %>%
        select(LocationDescription, Total) %>%
        filter(str_detect(LocationDescription, "Subtotal")) %>%
        mutate(LocationDescription = str_replace(LocationDescription, 
                                                 "Subtotal", "Situations"),
               Total = as.numeric(Total)) %>%
        left_join(Q23e_calcs, 
                  by = c("LocationDescription" = "APR_LocationGroup")) %>% 
        untabyl() %>%
        adorn_totals("row") %>%
        ifnull(., 0) %>% 
        select(c("LocationDescription", "Total", all_of(race_list_expanded)))
      
      }
      
      # Q24a checked
      {
        Q24a_detail <- recent_program_enrollment %>%
          filter(leaver &
                   ProjectType == 12) %>%
          select(ProjectType, all_of(standard_detail_columns), 
                 HousingAssessment, SubsidyInformation) %>%
          mutate(assessment_at_exit = case_when(
            HousingAssessment == 1 ~
              case_when(SubsidyInformation %in% 1:4 ~
                          assessment_outcomes[SubsidyInformation]),
            HousingAssessment == 2 ~
              case_when(SubsidyInformation == 11 ~
                          assessment_outcomes[5],
                        SubsidyInformation == 12 ~
                          assessment_outcomes[6]),
            HousingAssessment == 3 ~ assessment_outcomes[7],
            HousingAssessment == 4 ~ assessment_outcomes[8],
            HousingAssessment == 5 ~ assessment_outcomes[9],
            HousingAssessment == 6 ~ assessment_outcomes[10],
            HousingAssessment == 7 ~ assessment_outcomes[11],
            HousingAssessment == 10 ~ assessment_outcomes[12],
            HousingAssessment %in% c(8, 9) ~ assessment_outcomes[13],
            HousingAssessment == 99 ~ assessment_outcomes[14])) 
        
        Q24a <- Q24a_detail %>%
          filter(!is.na(assessment_at_exit)) %>%
          return_household_groups(., assessment_at_exit, assessment_outcomes) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
        
      }
      
      #Q24b checked
      {
        Q24b_detail <- recent_program_enrollment %>%
          filter(RelationshipToHoH == 1 &
                   ProjectType == 3) %>%
          select(all_of(housing_program_detail_columns)) %>%
          inner_join(Services %>% 
                       filter(DateProvided >= report_start_date &
                                DateProvided <= report_end_date &
                                RecordType == 300) %>%
                       select(EnrollmentID, TypeProvided) %>%
                       distinct(),
                     by = "EnrollmentID") 
        
        Q24b <- Q24b_detail %>%
          full_join(moving_on_assistance,
                    by = c("TypeProvided" = "Field")) %>%
          return_household_groups(., Response, moving_on_assistance$Response) %>%
          ifnull(., 0)
      }

      
      #Q24d checked
      {
        Q24d_detail <- recent_program_enrollment %>%
          filter(RelationshipToHoH == 1) %>%
          select(c(all_of(housing_program_detail_columns), 
                   "PreferredLanguage")) %>%
          left_join(possible_languages, 
                     by = c("PreferredLanguage" = "Response Option Number"),
                    multiple = "all") %>%
          filter(PreferredLanguage == 21 |
                   !is.na(`Response Option Name`))
        
        preferred_languages <- Q24d_detail %>%
          filter(!is.na(`Response Option Name`)) %>%
          group_by(`Response Option Name`, PreferredLanguage) %>%
          summarise(count = n_distinct(PersonalID)) %>%
          ungroup() %>%
          arrange(desc(count), `Response Option Name`) %>%
          slice_head(n = 20)
        
        different_language <- Q24d_detail %>%
          filter(PreferredLanguage == 21) %>%
          summarise(count = n_distinct(PersonalID)) %>%
          mutate(`Response Option Name` = "Different Preferred Language",
                 PreferredLanguage = 21)
        
        Q24d <- preferred_languages %>%
            full_join(different_language,
                      by = colnames(preferred_languages)) %>%
          rename(
            # "Language Response (Top 20 Languages Selected)" = `Response Option Name`,
            "Language Response (Top 20 Languages Selected)" = PreferredLanguage,
            "Total Persons Requiring Translation Assistance" = count) %>%
          # remove below line when Sage is updated
          select(-`Response Option Name`) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q24e
      {
        Q24e_detail <- recent_program_enrollment %>%
          select(all_of(standard_detail_columns)) %>%
          left_join(Client %>%
                      select(PersonalID, Sex),
                    by = "PersonalID") %>%
          mutate(sex_label = case_when(Sex == 0 ~ sex_categories[1],
                                       Sex == 1 ~ sex_categories[2],
                                       Sex %in% c(8, 9) ~ sex_categories[3],
                                       Sex == 99 ~ sex_categories[4])) %>%
          filter(!is.na(sex_label))
        
        Q24e <- Q24e_detail %>% 
          return_household_groups(., sex_label, sex_categories) %>% 
          adorn_totals("row") %>% 
          ifnull(., 0)
        

      }
      
      
      #-------------------------------------------------------------
      #------------------- Veteran Questions -----------------------
      #-------------------------------------------------------------
      
      recent_veteran_enrollment <- recent_program_enrollment %>%
        filter(new_veteran_status == 1)
      
      # Q25a checked
      # Updated categories - NOTE did not follow up on the comment below
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
      
      # Q25b checked
      # Refer to comment above for Q25a
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
      
      
      # Q25d checked
      {
        Q25d_detail <- Q11_detail %>%
          filter(PersonalID %in% recent_veteran_enrollment$PersonalID)
        
        Q25d <- Q25d_detail %>%
          create_age_groups(.) %>%
          select(-With.Only.Children) %>%
          filter(detailed_age_group %nin% detailed_age_group_list[1:3])
      }
      
      # Q25i checked
      {
        Q25i_detail <- Q23c_detail %>%
          filter(new_veteran_status == 1)
        
        Q25i <- create_destination_groups(recent_veteran_enrollment) %>%
          mutate(across(everything(), as.character))
        
        Q25i[42, 2:6] <- c(decimal_format(as.numeric(Q25i[42, 2:6]), 4))
      }
      
      #Q25j checked
      
      {
        Q25j_detail <- Q23d_detail %>%
          filter(new_veteran_status == 1)
        
        Q25j <- Q25j_detail %>% 
          return_household_groups(., Response, subsidy_list$Response) %>% 
          select(-With.Only.Children) %>%
          adorn_totals("row") %>% 
          ifnull(., 0)
      }
      

      #-------------------------------------------------------------
      #------------------- Chronic Questions -----------------------
      #-------------------------------------------------------------
      
      recent_chronic_enrollment <- recent_program_enrollment %>%
        # rename()
        filter(chronic == "Y")
      
      # Q26a checked
      {
        Q26b_detail <- recent_program_enrollment %>%
          select(c(all_of(standard_detail_columns), "chronic")) %>%
          mutate(category = case_when(
            chronic == "Y" ~ chronic_categories[1],
            chronic == "N" ~ chronic_categories[2],
            chronic == "Client.Does.Not.Know.or.Prefers.Not.to.Answer" ~ chronic_categories[3],
            TRUE ~ chronic_categories[4])) 
        
        Q26a_detail <- Q26b_detail %>%
          keep_adults_and_hoh_only(.)
        
        Q26a <- Q26a_detail %>%
          group_by(HouseholdID) %>%
          mutate(
            category = case_when(
              chronic_categories[1] %in% category ~ chronic_categories[1],
              chronic_categories[3] %in% category ~ chronic_categories[3],
              chronic_categories[4] %in% category ~ chronic_categories[4],
              chronic_categories[2] %in% category ~ chronic_categories[2])) %>%
          ungroup() %>%
          filter(RelationshipToHoH == 1) %>%
          return_household_groups(., category, chronic_categories) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
        }
      
      # Q26b checked
      {
        Q26b <- Q26b_detail %>%
          return_household_groups(., category, chronic_categories) %>%
          adorn_totals("row") %>%
          ifnull(., 0)
      }

      
      # Q26d checked
      {
        Q26d_detail <- Q11_detail %>%
          filter(PersonalID %in% recent_chronic_enrollment$PersonalID)
        
        Q26d <- Q26d_detail %>%
          create_age_groups(., chronic = TRUE) 
      }
      
      # Q26e checked
      {
        Q26e_detail <- "See Q13a1.csv, Q13b1.csv, and Q13c1.csv respectively"
        Q26e.1.and.2 <- recent_chronic_enrollment %>%
          inner_join(disability_table %>%
                       filter(DataCollectionStage %in% c(1, 3) &
                                DisabilityResponse != 0), by ="EnrollmentID",
                     multiple = "all") %>%
          group_by(disability_name) %>%
          summarise(Conditions.At.Start = n_distinct(PersonalID[DataCollectionStage == 1], 
                                                     na.rm = TRUE),
                    Conditions.At.Exit.for.Leavers = n_distinct(
                      PersonalID[DataCollectionStage == 3 & !is.na(ExitDate)], 
                      na.rm = TRUE))
        
        Q26e.3 <- Q13c1_detail %>%
          filter(DisabilityResponse != 0 &
                   EnrollmentID %in% recent_chronic_enrollment$EnrollmentID) %>%
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
      
      
      #-------------------------------------------------------------
      #-------------------- Youth Questions ------------------------
      #-------------------------------------------------------------
      
      recent_youth_enrollment <- recent_program_enrollment %>%
        filter(youth == 1)
      
      # Q27a checked
      {
        Q27a_detail <- Q11_detail %>%
          filter(PersonalID %in% recent_youth_enrollment$PersonalID) 
        
        Q27a <- Q27a_detail %>%
          create_age_groups(.) %>%
          filter(detailed_age_group %nin% detailed_age_group_list[c(1:2, 5:9)])
        }
      
      # Q27b checked
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
      
      
      # Q27d checked
      {
        Q27d_detail <- Q15_detail %>%
          filter(PersonalID %in% recent_youth_enrollment$PersonalID &
                   RelationshipToHoH == 1) 
        
        Q27d <- Q27d_detail %>%
          create_prior_residence_groups(.) %>%
          filter(LocationDescription != "Other Situations")
      }
      
      # Q27e checked
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
          adorn_totals("row") %>%
          ifnull(., 0)
      }
      
      # Q27f1 checked
      {
        Q27f1_detail <- Q23c_detail %>%
          filter(youth == 1)
        
        Q27f1 <- create_destination_groups(recent_youth_enrollment)  %>%
          mutate(across(everything(), as.character))
        
        Q27f1[42, 2:6] <- c(decimal_format(as.numeric(Q27f1[42, 2:6]), 4))
      }
        
        # Q27f2 checked
        {
          Q27f2_detail <- Q23d_detail %>%
            filter(youth == 1)
          
          Q27f2 <- Q27f2_detail %>%
            return_household_groups(., Response, subsidy_list$Response) %>% 
            adorn_totals("row") %>% 
            ifnull(., 0)
        }
        
        
      
      # Q27g checked
      # label for A17 specifies adults like the other uses of this logic, but
      # this question is unique in that it includes HoHs who are minors
      {
        Q27g_detail <- Q17_detail %>%
          filter(youth == 1)
        
        Q27g <- recent_youth_enrollment %>%
          keep_adults_and_hoh_only() %>%
          create_income_sources(.) %>%
          mutate(IncomeGroup = str_replace(IncomeGroup, 'Adults', 'Youth'))
      }
      
      # Q27h checked
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
          mutate(income_category = str_replace(income_category, 'Adults', 'Youth')) %>%
          rbind(., c("1 or more source of income", has_income)) %>%
          rbind(., c("Youth with Income Information at Start and Annual Assessment/Exit",
                     income_information_present(recent_youth_enrollment)[2:4]))
      }
      
      # Q27i checked
      {
        Q27i_detail <- Q19b_detail %>%
          filter(PersonalID %in% recent_youth_enrollment$PersonalID)
        
        Q27i <- exit_income %>%
          income_hh_type_disabling_condition_table(., youth = TRUE) %>% 
          mutate(name = case_when(
            name == "Unduplicated.Total.Adults" ~ "Unduplicated.Total.Youth", #Resolution to Testkit Issue 92. Rename to be youth 
            TRUE ~ name
          ))
        
        Q27i[, c(5, 9, 13, 17)] <- decimal_format(Q27i[, c(5, 9, 13, 17)], 4)
      }
      
      # Q27j checked
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
      
      # Q27k checked
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
        
        exited_without_move_in <- exited_without_move_in_data %>%
          return_household_groups(., housing_length_group, "Persons who were exited without move-in") 
        
        time_to_house_groups <- length_of_time_groups("days_prior_to_housing", "housing_length_group") %>%
          filter(housing_length_group %nin% c("731 days or more", 
                                              "Not yet moved into housing",
                                              "Data.Not.Collected"))
        
        Q27k <- data.frame(time_to_house_groups) %>%
          left_join(Q27k_detail %>%
                      return_household_groups(., housing_length_group, time_to_house_groups$housing_length_group),
                    by = "housing_length_group") %>%
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
          filter(EnrollmentID %nin% exited_without_move_in_data$EnrollmentID) %>%
          full_join(exited_without_move_in_data,
                    by = colnames(exited_without_move_in_data))
      }
      
      # Q27l checked
      {
        Q27l_detail <- recent_youth_enrollment %>%
          select(c("ProjectType", all_of(housing_program_detail_columns), "age",
                   "HoH_EntryDate", "DateToStreetESSH", "HoH_ADHS")) %>%
          create_time_prior_to_housing() %>%
          mutate(days_prior_to_housing = case_when(is.na(housing_date) ~ "Not yet moved into housing",
                                                   TRUE ~ days_prior_to_housing))
        
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
      
      # Q27m
      {
        Q27m_projects <- Funder %>%
          filter(Funder == 43 &
                   StartDate <= report_end_date &
                   (is.na(EndDate) |
                      EndDate >= report_start_date)) %>%
          select(ProjectID) %>%
          distinct()
        
        Q27m_setup <- recent_youth_enrollment %>%
          filter(leaver & 
                   RelationshipToHoH == 1 &
                   ProjectID %in% Q27m_projects$ProjectID) 
        
        youth_education_27m <- YouthEducationStatus %>%
          filter(DataCollectionStage %in% c(1, 3) &
                   EnrollmentID %in% Q27m_setup$EnrollmentID) %>%
          select(EnrollmentID, InformationDate, DataCollectionStage,
                 CurrentSchoolAttend, MostRecentEdStatus, CurrentEdStatus) %>%
          mutate(across(
            c(CurrentSchoolAttend, MostRecentEdStatus, CurrentEdStatus),
            ~ case_when(. == 8 ~ 9, TRUE ~ .))) %>%
          group_by(EnrollmentID, DataCollectionStage) %>%
          arrange(desc(InformationDate)) %>%
          slice(1L) %>%
          ungroup()
        
        Q27m_detail <- Q27m_setup %>%
          select(all_of(standard_detail_columns)) %>%
          left_join(youth_education_27m %>%
                      filter(DataCollectionStage == 1) %>%
                      setNames(paste0("entry_", colnames(youth_education_27m))),
                    by = c("EnrollmentID" = "entry_EnrollmentID")) %>%
          left_join(youth_education_27m %>%
                      filter(DataCollectionStage == 3) %>%
                      setNames(paste0("exit_", colnames(youth_education_27m))),
                    by = c("EnrollmentID" = "exit_EnrollmentID"))
       
        current_school_attend <- youth_education_27m %>%
          group_by(CurrentSchoolAttend) %>%
          summarise(
            At.Project.Start = n_distinct(EnrollmentID[DataCollectionStage == 1],
                                          na.rm = TRUE),
            At.Project.Exit = n_distinct(EnrollmentID[DataCollectionStage == 3],
                                         na.rm = TRUE),
            Question = "C3.2") %>%
          ungroup()
        
        most_recent_ed <- youth_education_27m %>%
          group_by(MostRecentEdStatus) %>%
          summarise(
            At.Project.Start = n_distinct(EnrollmentID[DataCollectionStage == 1],
                                          na.rm = TRUE),
            At.Project.Exit = n_distinct(EnrollmentID[DataCollectionStage == 3],
                                         na.rm = TRUE),
            Question = "C3.A") %>%
          ungroup()
        
        current_ed <- youth_education_27m %>%
          group_by(CurrentEdStatus) %>%
          summarise(
            At.Project.Start = n_distinct(EnrollmentID[DataCollectionStage == 1],
                                          na.rm = TRUE),
            At.Project.Exit = n_distinct(EnrollmentID[DataCollectionStage == 3],
                                         na.rm = TRUE),
            Question = "C3.B") %>%
          ungroup()
        
        total_ed <- youth_education_27m %>%
          summarise(
            At.Project.Start = n_distinct(EnrollmentID[DataCollectionStage == 1],
                                          na.rm = TRUE),
            At.Project.Exit = n_distinct(EnrollmentID[DataCollectionStage == 3],
                                         na.rm = TRUE),
            Response = "Total persons") %>%
          ungroup()
        
        Q27m <- youth_education_labels %>%
          left_join(bind_rows(current_school_attend, most_recent_ed) %>%
                      bind_rows(., current_ed) %>%
                      mutate(Value = case_when(
                        !is.na(CurrentSchoolAttend) ~ CurrentSchoolAttend,
                        !is.na(MostRecentEdStatus) ~ MostRecentEdStatus,
                        !is.na(CurrentEdStatus) ~ CurrentEdStatus
                      )), 
                    by = c("Value", "Question")) %>%
          full_join(total_ed,
                by = colnames(total_ed)) %>%
          select(Response, At.Project.Start, At.Project.Exit) %>%
          ifnull(., 0)
        
        Q27m[c(6, 17), 2:3] <- NA
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

