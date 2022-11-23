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

# library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
# library(dashboardthemes)
# library(DT)
# library(shinydashboardPlus)
# library(colourpicker)

generate_new_kits <- FALSE
compare_to_last <- FALSE
if (compare_to_last) {
  compare_to_dir <- choose.dir()}

{
  source("datalab_functions.R")
  source("DataLab_Lists.R")
  
  
  # combines all files
  {

    datalab_zips <- list.files(paste0(getwd(), "/HMIS Test Kit 2.0/HMIS CSVs"),
                               full.names = TRUE)
    
    for (zip in datalab_zips) {
      for (file in names(hmis_csvs)){
        
        data <- read_csv(unzip(zip, paste0(file, ".csv")),
                         col_types = get(file, hmis_csvs))
        
        if (exists(file)) {
          data <- get(file) %>%
            full_join(data, by = intersect(colnames(get(file)),
                                           colnames(data)))
        } 
        
        assign(file, data)
        
        file.remove(paste0(file, ".csv"))
      }
    }
  }
  
  # set variables
  report_start_date <- ymd(Export[1,]$ExportStartDate)
  report_end_date <- ymd(Export[1,]$ExportEndDate)
  
  # remove deleted records exportID colummns before proceeding with processing
  for (file in names(hmis_csvs)){
    
    data <- get(file) %>%
      select(-ExportID) %>%
      distinct()
    
    if ("DateDeleted" %in% colnames(get(file))) {
      data <- data %>%
        filter(is.na(DateDeleted) |
                 DateDeleted > report_end_date)
    }
    
    if (file == "Enrollment") {
      data <- data %>%
        rename(enroll_DateCreated = DateCreated) %>%
        mutate(MoveInDate = case_when(
          MoveInDate <= report_end_date &
            MoveInDate >= EntryDate ~ MoveInDate)) %>%
        filter(EntryDate <= report_end_date &
                 EnrollmentID %nin% Exit$EnrollmentID[Exit$ExitDate < report_start_date])
    }
    
    if (file == "Exit") {
      data <- data %>%
        rename(exit_DateCreated = DateCreated) %>%
        filter(ExitDate >= report_start_date &
                 ExitDate <= report_end_date) %>%
        select(-PersonalID)
    }
    
    if (file == "Funder") {
      data$Funder[data$ProjectID == 1552] <- 2
      data$Funder[data$ProjectID == 1554] <- 3
      data$Funder[data$ProjectID == 1565] <- 4
    }
    
    assign(file, data)
    
  }
  
  disability_table <- Disabilities %>%
    filter(DisabilityResponse == 1 |
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
           disabilities = if_else(disability_name == "Both Alcohol and Drug Use Disorders", 2, 1),
           indefinite_and_impairs = (DisabilityType %in% c(6, 8) |
                                       (DisabilityType %in% c(5, 7, 9, 10) &
                                          IndefiniteAndImpairs == 1))) %>%
    select(EnrollmentID, DataCollectionStage, InformationDate, disability_name, 
           indefinite_and_impairs, disabilities)
  
  additional_disability_check <- disability_table %>%
    filter(DataCollectionStage == 1 &
             indefinite_and_impairs) %>%
    group_by(EnrollmentID) %>%
    summarise() %>%
    ungroup() %>%
    mutate(has_disability = 1)
  
  chronic_individual <- Enrollment %>%
    select(EnrollmentID, DisablingCondition, ProjectID, EntryDate,
           LivingSituation, LOSUnderThreshold, PreviousStreetESSH,
           DateToStreetESSH, TimesHomelessPastThreeYears,
           MonthsHomelessPastThreeYears) %>%
    left_join(additional_disability_check, by = "EnrollmentID") %>%
    filter() %>%
    left_join(Project %>%
                select(ProjectID, ProjectType),
              by = "ProjectID") %>%
    mutate(disabling_condition_for_chronic = case_when(
      DisablingCondition == 1 |
        has_disability == 1 ~ "Y",
      DisablingCondition == 0 ~ "N",
      DisablingCondition %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
      TRUE ~ "M"),
      homeless_year_prior = trunc((DateToStreetESSH %--% EntryDate) / years(1)) >= 1 &
        !is.na(DateToStreetESSH),
      four_or_more_times = case_when(
        TimesHomelessPastThreeYears == 4 ~ "Y",
        TimesHomelessPastThreeYears %in% c(1, 2, 3) ~ "N",
        TimesHomelessPastThreeYears %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
        TRUE ~ "M"),
      twelve_or_more_months = case_when(
        MonthsHomelessPastThreeYears >= 112 ~ "Y",
        MonthsHomelessPastThreeYears %in% c(101, 102, 103, 104, 
                                            105, 106, 107, 108, 
                                            109, 110, 111) ~ "N",
        MonthsHomelessPastThreeYears %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
        TRUE ~ "M"
      ),
      chronic = case_when(
        disabling_condition_for_chronic != "Y" ~ disabling_condition_for_chronic,
        ProjectType %in% c(1, 4, 8) |
          LivingSituation %in% na.omit(ResidenceUses$Location[ResidenceUses$PriorResidenceType_Chronicity == "homeless"]) ~
          case_when(
            homeless_year_prior ~ "Y",
            four_or_more_times != "Y" ~ four_or_more_times,
            TRUE ~ twelve_or_more_months
          ),
        LivingSituation %in% na.omit(ResidenceUses$Location[ResidenceUses$PriorResidenceType_Chronicity == "institution"]) ~
          case_when(
            LOSUnderThreshold == 0 |
              DateToStreetESSH == 0 ~ "N",
            homeless_year_prior ~ "Y",
            four_or_more_times != "Y" ~ four_or_more_times,
            TRUE ~ twelve_or_more_months
          ),
        LivingSituation %in% na.omit(ResidenceUses$Location[ResidenceUses$PriorResidenceType_Chronicity == "other"]) ~
          case_when(
            LOSUnderThreshold == 0 |
              DateToStreetESSH == 0 ~ "N",
            homeless_year_prior ~ "Y",
            four_or_more_times != "Y" ~ four_or_more_times,
            TRUE ~ twelve_or_more_months
          )
      )
    ) %>%
    select(EnrollmentID, chronic)
  
  chronic_household <- Enrollment %>%
    select(PersonalID, EnrollmentID, HouseholdID, EntryDate, RelationshipToHoH) %>%
    left_join(Client, by = "PersonalID") %>%
    mutate(date_for_age = (if_else(
      EntryDate <= report_start_date,
      report_start_date,
      EntryDate)),
      age = trunc((DOB %--% date_for_age) / years(1)),
      chronic_age_group = case_when(age >= 18 ~ "adult",
                                    age < 18 ~ "child",
                                    TRUE ~ "unknown")) %>%
    left_join(chronic_individual %>%
                mutate(numeric_chronic = case_when(
                  chronic == "Y" ~ 1,
                  chronic == "N" ~ 2,
                  chronic == "Does.Not.Know.or.Refused" ~ 3,
                  chronic == "M" ~ 4
                )), 
              by = "EnrollmentID") %>%
    group_by(HouseholdID) %>%
    mutate(first_entry_date = min(EntryDate),
           min_chronicity = min(numeric_chronic),
           HoH_chronicity = min(case_when(RelationshipToHoH == 1 ~ numeric_chronic), 
                                na.rm = TRUE),
           HoH_or_adult_chronicity = min(case_when(RelationshipToHoH == 1 |
                                                     chronic_age_group == "adult" ~ numeric_chronic), 
                                         na.rm = TRUE),
           new_chronic = factor(
             case_when(
               min_chronicity == 1 ~ 1,
               HoH_or_adult_chronicity %in% c(3, 4) ~ HoH_chronicity,
               TRUE ~ numeric_chronic
             ), 
             levels = c(1, 2, 3, 4),
             labels = c("Y", "N", "Does.Not.Know.or.Refused", "M"))) %>%
    ungroup() %>%
    filter(EntryDate == first_entry_date) %>%
    select(EnrollmentID, new_chronic)
  
  # make sure the chronicity change applies correctly
  # test <- chronic_household %>%
  #   filter(HouseholdID %in% chronic_household$HouseholdID[chronic != new_chronic])
  
  chronicity_data <- chronic_household %>%
    full_join(chronic_individual %>%
                select(EnrollmentID, chronic),
              by = "EnrollmentID") %>%
    mutate(chronic = if_else(is.na(new_chronic), chronic, as.character(new_chronic))) %>%
    select(-new_chronic)
  
  # used for building
  {
    project_list <- c(
      # "942",    # DataLab - Coordinated Entry 
      # "1546",    # DataLab - ES-EE ESG I    
      # "1547",    # DataLab - ES-EE ESG II (with CE elements) 
      # "1544",    # DataLab - ES-EE RHY   
      # "1548",    # DataLab - ES-NbN ESG
      # "1564",    # DataLab - HP ESG
      "1552"#,    # DataLab - PSH CoC I
      # "1550",    # DataLab - PSH HOPWA  
      # "1551",    # DataLab - PSH VASH  
      # "1554",    # DataLab - RRH CoC I
      # "1555",    # DataLab - RRH CoC II
      # "1556",    # DataLab - RRH ESG I
      # "1553",    # Datalab - RRH VA  
      # "1557",    # DataLab - SH VA-HCHV     
      # "1565",    # DataLab - SO CoC
      # "1566",    # DataLab - SO ESG  
      # "1568",    # Datalab - SO PATH   
      # "1567",    # DataLab - SSO CoC  
      # "1561",    # DataLab - TH CoC
      # "1560",    # DataLab - TH ESG    
      # "1558",    # DataLab - TH HOPWA   
      # "1559",    # Datalab - TH RHY   
      # "1563",    # DataLab - TH VA  
      # "1562"    # Datalab - TH YHDP  
    )
    }
  
  # used for running all reports
  # {
  # full_project_list <- c(1552, Project$ProjectID[Project$ProjectID %in% Funder$ProjectID[Funder$Funder %in% 1:11]])
  # }
  
  items_to_keep <- c("items_to_keep", ls())
  
  # loop for running all
  # for(project_id in full_project_list) {
    # project_list <- c(project_id)
    
    all_program_enrollments <- Enrollment %>%
      filter(ProjectID %in% project_list) %>%
      left_join(Project %>%
                  select(ProjectID, ProjectType, TrackingMethod),
                by = "ProjectID") %>%
      left_join(Exit, by = "EnrollmentID")
    
    recent_program_enrollment <- all_program_enrollments %>%
      group_by(PersonalID) %>%
      arrange(desc(EntryDate)) %>%
      slice(1L) %>%
      ungroup() 
    
    # get additional client information (age for reporting)
    {
      
      client_plus <- Client %>%
        inner_join(recent_program_enrollment %>%
                     mutate(date_for_age = (if_else(
                       EntryDate <= report_start_date,
                       report_start_date,
                       EntryDate))) %>%
                     select(PersonalID, date_for_age, HouseholdID, RelationshipToHoH) %>%
                     distinct(),
                   by = "PersonalID") %>%
        mutate(age = trunc((DOB %--% date_for_age) / years(1)),
               detailed_age_group = case_when(age < 5 ~ "Under 5",
                                              age <= 12 ~ "5-12",
                                              age <= 17 ~ "13-17",
                                              age <= 24 ~ "18-24",
                                              age <= 34 ~ "25-34",
                                              age <= 44 ~ "35-44",
                                              age <= 54 ~ "45-54",
                                              age <= 61 ~ "55-61",
                                              age >= 62 ~ "62+",
                                              !is.na(DOBDataQuality) &
                                                DOBDataQuality %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
                                              TRUE ~ "Data.Not.Collected"), 
               age_group = case_when(age >= 18 ~ "Adults",
                                     age < 18 ~ "Children",
                                     TRUE ~ detailed_age_group),
               new_veteran_status = if_else(
                 age_group == "Children", as.integer(0), VeteranStatus),
               gender_combined = case_when(
                 Questioning == 1 ~ "Questioning",
                 NoSingleGender == 1 |
                   (Female == 1 &
                      Male == 1) ~ "No Single Gender",
                 Transgender == 1 ~ "Transgender",
                 Female == 1 ~ "Female",
                 Male == 1 ~ "Male",
                 GenderNone %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
                 TRUE ~ "Data.Not.Collected"),
               race_combined = case_when(
                 AmIndAKNative + Asian + BlackAfAmerican +
                   NativeHIPacific + White > 1 ~ "Multiple Races",
                 White == 1 ~ "White",
                 BlackAfAmerican == 1 ~ "Black, African American, or African",
                 Asian == 1 ~ "Asian or Asian American",
                 AmIndAKNative == 1 ~ "American Indian, Alaska Native, or Indigenous",
                 NativeHIPacific == 1 ~ "Native Hawaiian or Pacific Islander",
                 RaceNone %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
                 TRUE ~ "Data.Not.Collected")) %>%
        # get a second opinion on when to apply the household type calcs
        group_by(HouseholdID) %>%
        mutate(oldest_age = max(age, na.rm = TRUE),
               youth_household = if_else(oldest_age <= 24 &
                                           oldest_age >= 0, 1, 0),
               youth = if_else(youth_household & age >= 12, 1, 0),
               has_children = max(
                 if_else(age >= 0 &
                           age < 18 &
                           RelationshipToHoH == 2,
                         1, 0)
               ),
        ) %>%
        ungroup() %>%
        select(PersonalID, age, age_group, detailed_age_group, VeteranStatus, 
               youth_household, youth, has_children, new_veteran_status,
               gender_combined, race_combined)
      }
    
    annual_assessment_dates <- Enrollment %>%
      group_by(HouseholdID) %>%
      mutate(start_for_annual = max(EntryDate[RelationshipToHoH == 1]),
             years_in_project = trunc((start_for_annual %--% report_end_date) / years(1))) %>%
      filter(years_in_project > 0) %>%
      mutate(annual_due = start_for_annual %m+% years(years_in_project)) %>%
      select(HouseholdID, annual_due) %>%
      distinct()
    
    household_info <- recent_program_enrollment %>%
      left_join(client_plus, by = "PersonalID") %>%
      group_by(HouseholdID) %>%
      mutate(adults = max(if_else(age_group == "Adults", 1, 0)),
             children = max(if_else(age_group == "Children", 1, 0)),
             unknown = max(if_else(age_group %in% c("Does.Not.Know.or.Refused", "Data.Not.Collected"), 1, 0)),
             HoH_HMID = ifnull(suppressWarnings(min(case_when(
               RelationshipToHoH == 1 &
                 ProjectType %in% c(3, 13) ~ MoveInDate
             ), na.rm = TRUE)), NA),
             HoH_ADHS = suppressWarnings(min(case_when(
               RelationshipToHoH == 1 ~ DateToStreetESSH
             ), na.rm = TRUE)),
             HoH_EntryDate = suppressWarnings(min(case_when(
               RelationshipToHoH == 1 ~ EntryDate
             ), na.rm = TRUE))) %>%
      ungroup() %>%
      mutate(household_type = case_when(
        adults == 1 &
          children == 1 ~ "AdultsAndChildren",
        unknown == 1 ~ "Unknown",
        adults == 1 ~ "AdultsOnly",
        TRUE ~ "ChildrenOnly"
      )) %>%
      select(PersonalID, household_type, HoH_HMID, HoH_ADHS, HoH_EntryDate)
    
    recent_household_enrollment <- recent_program_enrollment %>%
      left_join(client_plus, by = "PersonalID") %>%
      left_join(household_info, by = "PersonalID")
    
    # Q4a
    {
      Q4a <- Project %>%
        filter(ProjectID %in% project_list) %>%
        select(OrganizationID, ProjectName, ProjectID, ProjectType,
               TrackingMethod, ResidentialAffiliation) %>%
        left_join(Organization %>%
                    select(OrganizationID, OrganizationName,
                           VictimServiceProvider), 
                  by = "OrganizationID") %>%
        left_join(Affiliation %>%
                    select(ProjectID, ResProjectID) %>%
                    group_by(ProjectID) %>%
                    summarise(affiliated_with = toString(ResProjectID)) %>%
                    ungroup(),
                  by = "ProjectID") %>%
        left_join(ProjectCoC %>%
                    select(ProjectID, CoCCode, Geocode) %>%
                    group_by(ProjectID) %>%
                    summarise(coc_codes = toString(CoCCode),
                              geocodes = toString(Geocode)) %>%
                    ungroup(),
                  by = "ProjectID") %>%
        left_join(recent_program_enrollment %>%
                    group_by(ProjectID) %>%
                    summarise(active_clients = n_distinct(PersonalID),
                              active_households = n_distinct(HouseholdID)),
                  by = "ProjectID") %>%
        mutate(software_name = "generated by Data Lab",
               report_start_date = report_start_date,
               report_end_date = report_end_date) %>%
        select(OrganizationName, OrganizationID, ProjectName, ProjectID,
               ProjectType, TrackingMethod, ResidentialAffiliation,
               affiliated_with, coc_codes, geocodes, VictimServiceProvider,
               software_name, report_start_date, report_end_date,
               active_clients, active_households) %>%
        `colnames<-`(c("Organization Name", "Organization ID",
                       "Project Name", "Project ID",
                       "HMIS Project Type", "Method for Tracking ES",
                       "Affiliated with a residential project",
                       "Project IDs of affiliations",
                       "CoC Number", "Geocode", "Victim Service Provider",
                       "HMIS Software Name", "Report Start Date",
                       "Report End Date", "Total Active Clients",
                       "Total Active Households"))
      }
    
    # Q5
    # Q5a
    {
      recent_program_enrollment_dq <- recent_program_enrollment %>%
        filter(ProjectType != 4 |
                 (!is.na(DateOfEngagement) &
                    DateOfEngagement <= report_end_date))
      
      Q5a_all <- recent_program_enrollment %>%
        left_join(client_plus, by = "PersonalID") %>%
        left_join(chronicity_data, by = "EnrollmentID") %>%
        summarise(persons_served = n_distinct(PersonalID),
                  adults_served = uniqueN(PersonalID[age_group == "Adults"]),
                  children_served = uniqueN(PersonalID[age_group == "Children"]),
                  unknown_served = uniqueN(PersonalID[age_group %in% c("Does.Not.Know.or.Refused", "Data.Not.Collected")]),
                  leavers = uniqueN(PersonalID[!is.na(ExitDate)]),
                  adult_leavers = uniqueN(PersonalID[age_group == "Adults" &
                                                       !is.na(ExitDate)]),
                  adult_and_hoh_leavers = uniqueN((PersonalID[(age_group == "Adults" |
                                                                 RelationshipToHoH == 1) &
                                                                !is.na(ExitDate)])),
                  stayers = uniqueN(PersonalID[is.na(ExitDate)]),
                  adult_stayers = uniqueN(PersonalID[age_group == "Adults" &
                                                       is.na(ExitDate)]),
                  veterans = uniqueN(PersonalID[new_veteran_status == 1]),
                  chronic = uniqueN(PersonalID[chronic == "Y"]),
                  youth = uniqueN(PersonalID[youth == 1]),
                  # can't figure out why this one isn't working
                  parenting_youth = uniqueN(PersonalID[has_children == 1 & youth == 1]),
                  adult_hoh = uniqueN(PersonalID[age_group == "Adults" &
                                                   RelationshipToHoH == 1]),
                  other_hoh = uniqueN(PersonalID[age_group != "Adults" &
                                                   RelationshipToHoH == 1]),
                  long_stayers = uniqueN(PersonalID[is.na(ExitDate) &
                                                      trunc((EntryDate %--% report_end_date) / days(1)) >= 365 &
                                                      (age_group == "Adults" |
                                                         RelationshipToHoH == 1)])
        ) %>% 
        mutate(rowname = "All_Clients") %>% 
        pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
        pivot_wider(names_from = "rowname", values_from = "values")
      
      Q5a_dq <- recent_program_enrollment_dq %>%
        left_join(client_plus, by = "PersonalID") %>%
        left_join(chronicity_data, by = "EnrollmentID") %>%
        summarise(persons_served = n_distinct(PersonalID),
                  adults_served = uniqueN(PersonalID[age_group == "Adults"]),
                  children_served = uniqueN(PersonalID[age_group == "Children"]),
                  unknown_served = uniqueN(PersonalID[age_group %in% c("Does.Not.Know.or.Refused", "Data.Not.Collected")]),
                  leavers = uniqueN(PersonalID[!is.na(ExitDate)]),
                  adult_leavers = uniqueN(PersonalID[age_group == "Adults" &
                                                       !is.na(ExitDate)]),
                  adult_and_hoh_leavers = uniqueN((PersonalID[(age_group == "Adults" |
                                                                 RelationshipToHoH == 1) &
                                                                !is.na(ExitDate)])),
                  stayers = uniqueN(PersonalID[is.na(ExitDate)]),
                  adult_stayers = uniqueN(PersonalID[age_group == "Adults" &
                                                       is.na(ExitDate)]),
                  veterans = uniqueN(PersonalID[new_veteran_status == 1]),
                  chronic = uniqueN(PersonalID[chronic == "Y"]),
                  youth = uniqueN(PersonalID[youth == 1]),
                  # can't figure out why this one isn't working
                  parenting_youth = uniqueN(PersonalID[has_children == 1 & youth == 1]),
                  adult_hoh = uniqueN(PersonalID[age_group == "Adults" &
                                                   RelationshipToHoH == 1]),
                  other_hoh = uniqueN(PersonalID[age_group != "Adults" &
                                                   RelationshipToHoH == 1]),
                  long_stayers = uniqueN(PersonalID[is.na(ExitDate) &
                                                      trunc((EntryDate %--% report_end_date) / days(1)) >= 365 &
                                                      (age_group == "Adults" |
                                                         RelationshipToHoH == 1)])
        ) %>% 
        mutate(rowname = "DQ_Clients") %>% 
        pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
        pivot_wider(names_from = "rowname", values_from = "values")
      
      Q5a <- Q5a_dq %>%
        left_join(Q5a_all, by = "Group")
      
      rm(Q5a_all, Q5a_dq)
    }
    
    # Q6
    # Q6a
    {
      Q6a_name <- recent_program_enrollment_dq %>%
        inner_join(Client, by = "PersonalID") %>%
        mutate(dq_flag = case_when(
          NameDataQuality %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
          NameDataQuality == 99 |
            is.na(FirstName) |
            is.na(LastName) ~ "M",
          NameDataQuality == 2 ~ "DI",
          TRUE ~ "OK")) 
      
      Q6a_ssn <- recent_program_enrollment_dq %>%
        inner_join(Client, by = "PersonalID") %>%
        mutate(sequential = lapply(SSN, sequential_ssn),
               dq_flag = case_when(
                 SSNDataQuality %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
                 SSNDataQuality == 99 |
                   is.na(SSNDataQuality) ~ "M",
                 SSNDataQuality == 2 |
                   suppressWarnings(is.na(as.numeric(SSN))) |
                   nchar(SSN) != 9 |
                   substr(SSN, 1, 3) == "000" |
                   substr(SSN, 1, 3) == "666" |
                   substr(SSN, 1, 1) == "9" |
                   substr(SSN, 4, 5) == "00" |
                   substr(SSN, 6, 9) == "0000" |
                   SSN %in% c("111111111", "222222222", "333333333",
                              "444444444", "555555555", "666666666",
                              "777777777", "888888888", "999999999") |
                   sequential == TRUE
                 ~ "DI",
                 TRUE ~ "OK")) 
      
      Q6a_dob <- recent_program_enrollment_dq %>%
        inner_join(Client, by = "PersonalID") %>%
        left_join(client_plus, by = "PersonalID") %>%
        mutate(dq_flag = case_when(
          DOBDataQuality %in% c(8, 9) &
            is.na(DOB) ~ "Does.Not.Know.or.Refused",
          is.na(DOBDataQuality) |
            (DOBDataQuality == 99 &
               is.na(DOB)) ~ "M",
          DOBDataQuality == 2 |
            (DOBDataQuality %in% c(8, 9, 99) &
               !is.na(DOB)) |
            DOB < mdy("1/1/1915") |
            DOB > DateCreated |
            (DOB >= EntryDate &
               (age_group == "Adults" |
                  RelationshipToHoH == 1))
          
          ~ "DI",
          TRUE ~ "OK")) 
      
      Q6a_race <- recent_program_enrollment_dq %>%
        inner_join(Client, by = "PersonalID") %>%
        mutate(dq_flag = case_when(
          RaceNone %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
          RaceNone == 99 |
            (AmIndAKNative == 0 &
               Asian == 0 &
               BlackAfAmerican == 0 &
               NativeHIPacific == 0 &
               White == 0) ~ "M",
          TRUE ~ "OK"))
      
      Q6a_ethnicity <- recent_program_enrollment_dq %>%
        inner_join(Client, by = "PersonalID") %>%
        mutate(dq_flag = case_when(
          Ethnicity %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
          Ethnicity == 99 ~ "M",
          TRUE ~ "OK")) 
      
      Q6a_gender <- recent_program_enrollment_dq %>%
        inner_join(Client, by = "PersonalID") %>%
        mutate(dq_flag = case_when(
          GenderNone %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
          GenderNone == 99 |
            (Female == 0 &
               Male == 0 &
               NoSingleGender == 0 &
               Transgender == 0 &
               Questioning == 0) ~ "M",
          TRUE ~ "OK")) 
      
      columns <- c("DataElement", "Does.Not.Know.or.Refused", "M", "DI", "OK")
      
      Q6a <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), columns) %>%
        mutate(across(DataElement, factor)) 
      
      elements <- list("Name", "SSN", "DOB", "Race", "Ethnicity", "Gender")
      
      for (element in elements) {
        table <- get(paste0("Q6a_", tolower(element))) %>%
          group_by(dq_flag) %>%
          summarise(Clients = n()) %>% 
          pivot_wider(names_from = "dq_flag", values_from = "Clients") %>%
          mutate(DataElement = element)
        
        Q6a <- Q6a %>%
          full_join(table, by = intersect(columns, colnames(table)))
        
        rm(table)
        
        if (exists("error_clients")) {
          error_clients <- error_clients %>%
            union(get(paste0("Q6a_", tolower(element))) %>%
                    filter(dq_flag != "OK") %>%
                    select(PersonalID))
          
        } else {
          error_clients <- get(paste0("Q6a_", tolower(element))) %>%
            filter(dq_flag != "OK") %>%
            select(PersonalID)
        }
      }
      
      Q6a <- Q6a %>%
        select(-OK) %>%
        mutate(`Does.Not.Know.or.Refused` = if_else(is.na(`Does.Not.Know.or.Refused`), 0, as.double(`Does.Not.Know.or.Refused`)),
               M = if_else(is.na(M), 0, as.double(M)),
               DI = if_else(is.na(DI), 0, as.double(DI)),
               Total = `Does.Not.Know.or.Refused` + M + DI) %>%
        add_row(DataElement = "Overall Score", 
                `Does.Not.Know.or.Refused` = 0, M = 0, DI = 0, 
                Total = nrow(unique(error_clients))) %>%
        mutate(ErrorRate = Total / Q5a$DQ_Clients[1])
      
      rm(list=ls(pattern="^Q6a_"))
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
                         "ProjectID" = "EarlierProjectID")) %>%
        filter(EntryDate > EarlierEntryDate &
                 EntryDate < EarlierExitDate)
      
      # want to talk through this one--if the HoH has two enrollments and one with
      # their partner is older but their partner doesn't have a more recent one, their
      # partner will get flagged here. Technically correct from the specs, I think,
      # but seems wrong
      Q6b_hoh_count <- recent_program_enrollment_dq %>%
        group_by(HouseholdID) %>%
        summarise(hohs = uniqueN(PersonalID[RelationshipToHoH == 1])) %>%
        filter(hohs != 1)
      
      # accounts for test kit CoC codes, remove XX- values for real data
      valid_cocs <- c(valid_cocs, "XX-500", "XX-501")
      
      Q6b <- recent_program_enrollment_dq %>%
        left_join(client_plus, by = "PersonalID") %>%
        left_join(EnrollmentCoC %>%
                    filter(DataCollectionStage == 1) %>%
                    select(EnrollmentID, CoCCode),
                  by = "EnrollmentID") %>%
        mutate(veteran_dq = (VeteranStatus %in% c(8, 9, 99) &
                               age_group == "Adults") |
                 (age_group == "Children" &
                    VeteranStatus == 1),
               project_start_dq = EnrollmentID %in% Q6b_earlier_enrollment$EnrollmentID,
               relationship_dq = is.na(RelationshipToHoH) |
                 RelationshipToHoH %nin% 1:5 |
                 HouseholdID %in% Q6b_hoh_count$HouseholdID,
               location_dq = RelationshipToHoH == 1 &
                 (is.na(CoCCode) |
                    CoCCode %nin% valid_cocs),
               disabling_condition_dq = DisablingCondition %in% c(8, 9, 99) |
                 is.na(DisablingCondition) |
                 (DisablingCondition == 0 &
                    EnrollmentID %in% additional_disability_check$EnrollmentID)) %>%
        summarise(veteran_dq = n_distinct(PersonalID[veteran_dq]),
                  project_start_dq = n_distinct(PersonalID[project_start_dq]),
                  relationship_dq = n_distinct(PersonalID[relationship_dq]),
                  location_dq = n_distinct(PersonalID[location_dq]),
                  disabling_condition_dq = n_distinct(PersonalID[disabling_condition_dq]),
        ) %>% 
        mutate(rowname = "DQ_Clients") %>% 
        pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
        pivot_wider(names_from = "rowname", values_from = "values") %>%
        mutate(ErrorRate = case_when(
          Group == "veteran_dq" ~ DQ_Clients / Q5a$DQ_Clients[2],
          Group == "location_dq" ~ DQ_Clients / (Q5a$DQ_Clients[14] + Q5a$DQ_Clients[15]),
          TRUE ~ DQ_Clients / Q5a$DQ_Clients[1])
        )
      
    }
    
    # Q6c
    {
      income_sources <- IncomeBenefits %>%
        # select(EnrollmentID, DataCollectionStage, Earned, Unemployment,
        #        SSI, SSDI, VADisabilityService, VADisabilityNonService,
        #        PrivateDisability, WorkersComp, TANF, GA, SocSecRetirement,
        #        Pension, ChildSupport, Alimony, OtherIncomeSource,
        #        InformationDate) %>%
        mutate(number_of_sources = 
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
      
      
      Q6c <- recent_program_enrollment_dq %>%
        left_join(client_plus, by = "PersonalID") %>%
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
        mutate(destination_dq = !is.na(ExitDate) &
                 (is.na(Destination) |
                    Destination %in% c(8, 9, 99, 30)),
               income_start_dq = (RelationshipToHoH == 1 |
                                    age_group == "Adults") &
                 (enroll_IncomeFromAnySource %in% c(8, 9, 99) |
                    is.na(enroll_IncomeFromAnySource) |
                    (enroll_IncomeFromAnySource == 0 &
                       !is.na(enroll_number_of_sources) &
                       enroll_number_of_sources > 0) |
                    (enroll_IncomeFromAnySource == 1 &
                       (is.na(enroll_number_of_sources) |
                          enroll_number_of_sources == 0))),
               income_annual_dq = (RelationshipToHoH == 1 |
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
               income_exit_dq = (RelationshipToHoH == 1 |
                                   age_group == "Adults") &
                 !is.na(ExitDate) &
                 (exit_IncomeFromAnySource %in% c(8, 9, 99) |
                    is.na(exit_IncomeFromAnySource) |
                    (exit_IncomeFromAnySource == 0 &
                       !is.na(exit_number_of_sources) &
                       exit_number_of_sources > 0) |
                    (exit_IncomeFromAnySource == 1 &
                       (is.na(exit_number_of_sources) |
                          exit_number_of_sources == 0)))
        )%>%
        summarise(destination_dq = n_distinct(PersonalID[destination_dq]),
                  income_start_dq = n_distinct(PersonalID[income_start_dq]),
                  income_annual_dq = n_distinct(PersonalID[income_annual_dq]),
                  income_exit_dq = n_distinct(PersonalID[income_exit_dq])
        ) %>% 
        mutate(rowname = "DQ_Clients") %>% 
        pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
        pivot_wider(names_from = "rowname", values_from = "values") %>%
        mutate(ErrorRate = case_when(
          Group == "destination_dq" ~ DQ_Clients / Q5a$DQ_Clients[5],
          Group == "income_start_dq" ~ DQ_Clients / (Q5a$DQ_Clients[2] + Q5a$DQ_Clients[15]),
          Group == "income_annual_dq" ~ DQ_Clients / Q5a$DQ_Clients[16],
          Group == "income_exit_dq" ~ DQ_Clients / Q5a$DQ_Clients[7])
        )
    }
    
    # Q6d
    {
      Q6d <- recent_program_enrollment_dq %>%
        left_join(client_plus, by = "PersonalID") %>%
        filter(EntryDate >= mdy("10/1/2016")) %>%
        keep_adults_and_hoh_only() %>%
        mutate(project_type_group = case_when(
          ProjectType %in% c(1, 4, 8) ~ "ES_SH_SO",
          ProjectType == 2 ~ "TH",
          ProjectType %in% c(3, 9, 10, 13) ~ "PH"),
          missing_institution = LivingSituation %in% c(15, 6, 7, 25, 4, 5) &
            (LengthOfStay %in% c(8, 9, 99) |
               is.na(LengthOfStay)),
          missing_housing = (LivingSituation %in% c(29, 14, 2, 32, 36, 35, 
                                                    28, 19, 3, 31, 33, 34, 10, 
                                                    20, 21, 11, 8, 9, 99) |
                               is.na(LivingSituation)) &
            (LengthOfStay %in% c(8, 9, 99) |
               is.na(LengthOfStay)),
          include_for_EFG = project_type_group == "ES_SH_SO" |
            (PreviousStreetESSH == 1 &
               (LivingSituation %in% c(16, 1, 18) |
                  (LivingSituation %in% c(15, 6, 7, 25, 4, 5) &
                     LengthOfStay %in% c(10, 11, 2, 3)) |
                  ((LivingSituation %in% c(29, 14, 2, 32, 36, 35, 28, 19, 3, 31, 
                                           33, 34, 10, 20, 21, 11, 8, 9, 99) |
                      is.na(LivingSituation)) &
                     LengthOfStay %in% c(10, 11)))),
          missing_date = include_for_EFG &
            is.na(DateToStreetESSH),
          missing_times = is.na(TimesHomelessPastThreeYears) |
            TimesHomelessPastThreeYears %in% c(8, 9, 99),
          missing_months = is.na(MonthsHomelessPastThreeYears) |
            MonthsHomelessPastThreeYears %in% c(8, 9, 99)
        ) %>% 
        group_by(project_type_group) %>%
        summarise(all_records = n_distinct(PersonalID),
                  institution_dq = n_distinct(PersonalID[missing_institution]),
                  housing_dq = n_distinct(PersonalID[missing_housing]),
                  date_dq = n_distinct(PersonalID[missing_date]),
                  times_dq = n_distinct(PersonalID[missing_times]),
                  months_dq = n_distinct(PersonalID[missing_months]),
                  all_errors = n_distinct(PersonalID[missing_institution |
                                                       missing_housing |
                                                       missing_date |
                                                       missing_times |
                                                       missing_months])) %>% 
        pivot_longer(!project_type_group, names_to = "Group", values_to = "values") %>% 
        pivot_wider(names_from = "Group", values_from = "values") %>%
        mutate(ErrorRate = all_errors / all_records) %>%
        select(-all_errors)
      
    }
    
    # Q6e
    {
      Q6e <- data.frame(TimeForEntry = c("0 days", "1-3 days", "4-6 days",
                                         "7-10 days", "11+ days")) %>%
        full_join(recent_program_enrollment_dq %>%
                    mutate(days_for_entry = case_when(EntryDate >= report_start_date &
                                                        EntryDate <= report_end_date ~ trunc((EntryDate %--% enroll_DateCreated) / days(1))),
                           TimeForEntry = case_when(
                             days_for_entry < 0 ~ "Error",
                             days_for_entry == 0 ~ "0 days",
                             days_for_entry <= 3 ~ "1-3 days",
                             days_for_entry <= 6 ~ "4-6 days",
                             days_for_entry <= 10 ~ "7-10 days",
                             !is.na(days_for_entry) ~ "11+ days"
                           )) %>%
                    filter(!is.na(TimeForEntry)) %>%
                    group_by(TimeForEntry) %>%
                    summarise(StartRecords = n_distinct(PersonalID)),
                  by = "TimeForEntry")%>%
        full_join(recent_program_enrollment_dq %>%
                    mutate(days_for_exit = case_when(!is.na(ExitDate) ~ trunc((ExitDate %--% exit_DateCreated) / days(1))),
                           TimeForEntry = case_when(
                             days_for_exit < 0 ~ "Error",
                             days_for_exit == 0 ~ "0 days",
                             days_for_exit <= 3 ~ "1-3 days",
                             days_for_exit <= 6 ~ "4-6 days",
                             days_for_exit <= 10 ~ "7-10 days",
                             !is.na(days_for_exit) ~ "11+ days"
                           )) %>%
                    filter(!is.na(TimeForEntry)) %>%
                    group_by(TimeForEntry) %>%
                    summarise(ExitRecords = n_distinct(PersonalID)),
                  by = "TimeForEntry")
      
    }
    
    # Q6f
    {
      # programming specs are unclear--most recent for individual or enrollment?
      most_recent_CLS <- CurrentLivingSituation %>%
        filter(InformationDate >= report_start_date &
                 InformationDate <= report_end_date) %>%
        arrange(desc(InformationDate)) %>%
        group_by(PersonalID) %>%
        slice(1L) %>%
        ungroup()
      
      prior_CLS <- CurrentLivingSituation %>%
        filter(InformationDate %nin% most_recent_CLS$InformationDate) %>%
        arrange(desc(InformationDate)) %>%
        group_by(PersonalID) %>%
        slice(1L) %>%
        ungroup()
      
      # actually pausing on this whole piece--what on earth is bullet four trying to say??
      # Q6f <- recent_program_enrollment_dq %>%
      #   filter((ProjectType == 4 |
      #             (ProjectType == 1 &
      #                TrackingMethod == 3)) &
      #            is.na(ExitDate) &
      #            trunc((EntryDate %--% report_end_date) / days(1)) >= 90) %>%
      #   left_join(most_recent_CLS %>%
      #               select(PersonalID, InformationDate) %>%
      #               rename(recent_CLS_InformationDate = InformationDate),
      #             by = "PersonalID") %>%
      #   left_join(prior_CLS %>%
      #               select(PersonalID, InformationDate) %>%
      #               rename(prior_InformationDate = InformationDate),
      #             by = "PersonalID")
    }
    
    # Q7
    # Q7a
    {
      
      Q7a_all <- recent_household_enrollment %>%
        mutate(client_group = "Total") %>%
        return_household_groups(., client_group, c("Total")) 
      
      Q7a_moved_in <- recent_household_enrollment %>%
        filter(HoH_HMID <= report_end_date) %>% 
        mutate(client_group = "For PSH & RRH – the total persons served who moved into housing") %>%
        return_household_groups(., client_group, "For PSH & RRH – the total persons served who moved into housing") 
      
      Q7a <- recent_household_enrollment %>%
        return_household_groups(., age_group, age_groups) %>%
        rename(client_group = age_group) %>%
        union(Q7a_all) %>%
        union(Q7a_moved_in)
      
      # did this one with table(), want to talk through whether this is the best call
      # Q7a <- table(Q7a$age_group, Q7a$household_type)
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
        
        pit_enrollments <- all_program_enrollments %>%
          left_join(year_household_info, by = "EnrollmentID") %>%
          filter(EntryDate <= pit_date &
                   (ProjectType %nin% c(3, 13) |
                      (ProjectType %in% c(3, 13) &
                         HoH_HMID <= pit_date)) &
                   (is.na(ExitDate) |
                      (ProjectType %in% c(1, 2, 3, 8, 9, 10, 13) &
                         ExitDate > pit_date) |
                      (ProjectType %in% c(4, 6, 11) &
                         ExitDate >= pit_date))) %>%
          left_join(household_info %>%
                      select(PersonalID, household_type),
                    by = "PersonalID") %>%
          mutate(Month = pit_month) %>%
          return_household_groups(., Month, pit_month)
        
        if(pit_month == "January") {
          Q7b <- pit_enrollments
        } else {
          Q7b <- Q7b %>%
            union(pit_enrollments)
        }
      }
      # still need to add NbN logic here
    }
    
    # Q8a
    {
      Q8a_all <- recent_household_enrollment %>%
        filter(RelationshipToHoH == 1) %>% 
        mutate(client_group = "Total Households") %>%
        return_household_groups(., client_group, "Total Households") 
      
      Q8a_moved_in <- recent_household_enrollment %>%
        filter(RelationshipToHoH == 1 &
                 HoH_HMID <= report_end_date) %>% 
        mutate(client_group = "Moved In Households") %>%
        return_household_groups(., client_group, "Moved In Households") 
      
      Q8a <- Q8a_all %>%
        union(Q8a_moved_in)
      
    }
    
    # Q8b
    {
      for (pit_month in pit_months) {
        pit_date <- pit_dates[[which(pit_dates$month == pit_month), 2]]
        
        pit_hh_enrollments <- all_program_enrollments %>%
          left_join(year_household_info, by = "EnrollmentID") %>%
          filter(EntryDate <= pit_date &
                   (ProjectType %nin% c(3, 13) |
                      (ProjectType %in% c(3, 13) &
                         HoH_HMID <= pit_date)) &
                   (is.na(ExitDate) |
                      (ProjectType %in% c(1, 2, 3, 8, 9, 10, 13) &
                         ExitDate > pit_date) |
                      (ProjectType %in% c(4, 6, 11) &
                         ExitDate >= pit_date))) %>%
          select(HouseholdID) %>%
          distinct() %>%
          left_join(all_program_enrollments %>%
                      filter(RelationshipToHoH == 1),
                    by = "HouseholdID") %>%
          left_join(household_info %>%
                      select(PersonalID, household_type),
                    by = "PersonalID") %>%
          mutate(Month = pit_month) %>%
          return_household_groups(., Month, pit_month)
        
        if(pit_month == "January") {
          Q8b <- pit_hh_enrollments
        } else {
          Q8b <- Q8b %>%
            union(pit_hh_enrollments)
        }
      }
    }
    
    # Q9a
    {
      if(nrow(recent_household_enrollment %>%
              filter((ProjectType == 4 |
                      (ProjectType == 1 &
                       TrackingMethod == 3)))) > 0) {
        
        # same question as before--is this the most recent CLS for the *person* or the *enrollment*?
        recent_CLS_for_Q9 <- recent_household_enrollment %>%
          left_join(CurrentLivingSituation %>%
                      rename(CLS_InformationDate = InformationDate,
                             CLS = CurrentLivingSituation), by = "PersonalID") %>%
          filter(CLS_InformationDate >= report_start_date &
                   CLS_InformationDate <= report_end_date &
                   (CLS_InformationDate <= DateOfEngagement |
                      is.na(DateOfEngagement)))
        
        # this one specifies enrollment as directed in the programming specifications
        all_CLS_for_Q9 <- recent_household_enrollment %>%
          left_join(CurrentLivingSituation %>%
                      rename(CLS_InformationDate = InformationDate,
                             CLS = CurrentLivingSituation), by = "EnrollmentID") %>%
          filter(CLS_InformationDate >= EntryDate &
                   (CLS_InformationDate <= ExitDate |
                      is.na(ExitDate)) &
                   (CLS_InformationDate <= DateOfEngagement |
                      is.na(DateOfEngagement)) &
                   CLS_InformationDate <= report_end_date) %>%
          select(EnrollmentID, CLS_InformationDate, CLS) %>%
          union(recent_household_enrollment %>%
                  filter(DateOfEngagement <= report_end_date) %>%
                  rename(CLS_InformationDate = DateOfEngagement) %>%
                  mutate(CLS = 999) %>%
                  select(EnrollmentID, CLS_InformationDate, CLS)) %>%
          arrange(CLS) %>%
          group_by(EnrollmentID, CLS_InformationDate) %>%
          slice(1L) %>%
          ungroup()
        
        first_CLS_group <- all_CLS_for_Q9 %>%
          arrange(desc(CLS_InformationDate)) %>%
          group_by(EnrollmentID) %>%
          slice(1L) %>%
          ungroup %>%
          mutate(CLS_group = case_when(
            CLS %in% c(16, 1, 18) ~ "LH",
            CLS %in% c(37, 8, 9, 99, 999) ~ "Unknown",
            TRUE ~ "Not LH"
          ))
        
        Q9_logic <- function(Q9_data) {
          Q9_data %>%
            left_join(first_CLS_group, by = "EnrollmentID") %>%
            left_join(all_CLS_for_Q9 %>%
                        group_by(EnrollmentID) %>%
                        summarise(Contacts = n()) %>%
                        mutate(ContactGroup = case_when(
                          Contacts == 1 ~ "Once",
                          Contacts <= 5 ~ "2-5 times",
                          Contacts <= 9 ~ "6-9 times",
                          TRUE ~ "10+ times"),
                          ContactGroup = factor(ContactGroup, ordered = TRUE, 
                                                levels = c("Once", "2-5 times", 
                                                           "6-9 times", "10+ times"))),
                      by = "EnrollmentID") %>% 
            group_by(ContactGroup) %>%
            summarise(all_persons_contacted = n_distinct(PersonalID),
                      first_contact_not_LH = n_distinct(PersonalID[CLS_group == "Not LH"]),
                      first_contact_LH = n_distinct(PersonalID[CLS_group == "LH"]),
                      first_contact_unknown = n_distinct(PersonalID[CLS_group == "Unknown"])) %>%
            # pivot_longer(!ContactGroup, names_to = "Group", values_to = "values") %>%
            # pivot_wider(names_from = "Group", values_from = "values") %>%
            adorn_totals("row")
        }
        
        Q9a <- recent_household_enrollment %>%
          filter((ProjectType == 4 |
                    (ProjectType == 1 &
                       TrackingMethod == 3)) &
                   (PersonalID %in% recent_CLS_for_Q9$PersonalID |
                      (DateOfEngagement >= report_start_date &
                         DateOfEngagement <= report_end_date))) %>%
          keep_adults_and_hoh_only() %>%
          Q9_logic()
      }
    }
    
    # Q9b
    {
      if(nrow(recent_household_enrollment %>%
              filter((ProjectType == 4 |
                      (ProjectType == 1 &
                       TrackingMethod == 3)))) > 0) {
        
        Q9b <- recent_household_enrollment %>%
          filter((ProjectType == 4 |
                    (ProjectType == 1 &
                       TrackingMethod == 3)) &
                   (DateOfEngagement >= report_start_date &
                      DateOfEngagement <= report_end_date)) %>%
          keep_adults_and_hoh_only() %>%
          Q9_logic()
        
        rate_of_engagement <- c("EngagementRate")
        for (column in 2:5) {
          if (Q9a[[which(Q9a$ContactGroup == "Total"), column]] == 0) {
            rate <- 0
          } else {
            rate <- as.numeric(Q9b[[which(Q9b$ContactGroup == "Total"), column]]) / 
              Q9a[[which(Q9a$ContactGroup == "Total"), column]]
          }
          rate_of_engagement <- c(rate_of_engagement, rate)
        }
        
        Q9b <- rbind(Q9b, rate_of_engagement)
      }
    }
    
    # Q10a
    {
      
      Q10a <- recent_household_enrollment %>%
        keep_adults_only() %>%
        create_gender_groups(.)
    }
    
    # Q10b
    {
      Q10b <- recent_household_enrollment %>%
        filter(age_group == "Children") %>%
        create_gender_groups(.)
    }
    
    # Q10c
    {
      Q10c <- recent_household_enrollment %>%
        filter(age_group %in% c("Does.Not.Know.or.Refused", "Data.Not.Collected")) %>%
        create_gender_groups(.)
    }
    
    # Q10d
    {
      Q10d <- recent_household_enrollment %>%
        mutate(Q10d_age_group = case_when(
          detailed_age_group %in% c("Under 5", "5-12", "13-17") ~ "Under18",
          detailed_age_group %in% c("25-34", "35-44", "45-54", "55-61") ~ "25-61",
          TRUE ~ detailed_age_group)) %>%
        group_by(gender_combined) %>%
        summarise(Total = n_distinct(PersonalID),
                  Under.Age.18 = n_distinct(PersonalID[Q10d_age_group == "Under18"]),
                  Age.18.to.24 = n_distinct(PersonalID[Q10d_age_group == "18-24"]),
                  Age.25.to.61 = n_distinct(PersonalID[Q10d_age_group == "25-61"]),
                  Age.62.and.over = n_distinct(PersonalID[Q10d_age_group == "62+"]),
                  Does.Not.Know.or.Refused = n_distinct(PersonalID[Q10d_age_group == "Does.Not.Know.or.Refused"]),
                  Data.Not.Collected = n_distinct(PersonalID[Q10d_age_group == "Data.Not.Collected"])) %>%
        adorn_totals("row")
    }
    
    # Q11
    {
      Q11 <- recent_household_enrollment %>%
        create_age_groups(.)
    }
    
    # Q12a
    {
      
      Q12a <- recent_household_enrollment %>%
        return_household_groups(., race_combined, race_list) %>%
        adorn_totals("row")
    }
    
    # Q12b
    {
      
      Q12b <- recent_household_enrollment %>%
        left_join(Client %>%
                    select(PersonalID, Ethnicity), by = "PersonalID") %>%
        mutate(display_ethnicity = case_when(Ethnicity == 1 ~ "Hispanic/Latin(a)(o)(x)",
                                             Ethnicity == 0 ~ "Non-Hispanic/Non-Latin(a)(o)(x)",
                                             Ethnicity %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
                                             TRUE ~ "Data.Not.Collected")) %>%
        return_household_groups(., display_ethnicity, ethnicity_list) %>%
        adorn_totals("row")
    }
    
    # Q13a1
    {
      
      Q13a <- recent_household_enrollment %>%
        inner_join(disability_table %>%
                     filter(DataCollectionStage == 1), by ="EnrollmentID")
      
      Q13a1 <- Q13a %>%
        return_household_groups(., disability_name, disability_list)
    }
    
    # Q13b1
    {
      Q13b <- recent_household_enrollment %>%
        filter(!is.na(ExitDate)) %>%
        inner_join(disability_table %>%
                     filter(DataCollectionStage == 3), by ="EnrollmentID")
      
      Q13b1 <- Q13b %>%
        return_household_groups(., disability_name, disability_list)
    }
    
    # Q13c1
    {
      Q13c <- recent_household_enrollment %>%
        filter(is.na(ExitDate)) %>%
        inner_join(disability_table %>%
                     filter(InformationDate <= report_end_date) %>%
                     group_by(EnrollmentID) %>%
                     mutate(last_date = max(InformationDate)) %>%
                     ungroup() %>%
                     select(EnrollmentID, last_date) %>%
                     distinct() %>%
                     left_join(disability_table, 
                               by = c("EnrollmentID", "last_date" = "InformationDate")), 
                   by ="EnrollmentID")
      
      Q13c1 <- Q13c %>%
        return_household_groups(., disability_name, disability_list)
    }
    
    # Q13a2
    {
      
      Q13a2 <- recent_household_enrollment %>%
        left_join(Q13a %>%
                    # earlier data lab logic did not account for the following step
                    group_by(PersonalID) %>%
                    summarise(disability_count = sum(disabilities)) %>%
                    ungroup(), 
                  by = "PersonalID") %>%
        condition_count_groups() %>%
        return_household_groups(., disability_count_group, disability_count_group_list) %>%
        adorn_totals("row")
    }
    
    # Q13b2
    {
      Q13b2 <- recent_household_enrollment %>%
        filter(!is.na(ExitDate)) %>%
        left_join(Q13b %>%
                    # earlier data lab logic did not account for the following step
                    group_by(PersonalID) %>%
                    summarise(disability_count = sum(disabilities)) %>%
                    ungroup(), 
                  by = "PersonalID") %>%
        condition_count_groups() %>%
        return_household_groups(., disability_count_group, disability_count_group_list) %>%
        adorn_totals("row")
    }
    
    # Q13c2
    {
      Q13c2 <- recent_household_enrollment %>%
        filter(is.na(ExitDate)) %>%
        left_join(Q13c %>%
                    # earlier data lab logic did not account for the following step
                    group_by(PersonalID) %>%
                    summarise(disability_count = sum(disabilities)) %>%
                    ungroup(), 
                  by = "PersonalID") %>%
        condition_count_groups() %>%
        return_household_groups(., disability_count_group, disability_count_group_list) %>%
        adorn_totals("row")
    }
    
    # Q14a
    {
      
      Q14 <- recent_household_enrollment %>%
        left_join(HealthAndDV %>%
                    filter(InformationDate <= report_end_date) %>%
                    arrange(desc(InformationDate)) %>%
                    group_by(EnrollmentID) %>%
                    slice(1L) %>%
                    ungroup() %>%
                    select(EnrollmentID, DomesticViolenceVictim, CurrentlyFleeing),
                  by = "EnrollmentID")
      
      Q14a <- Q14 %>%
        mutate(dv_experience = case_when(DomesticViolenceVictim == 1 ~ "Yes",
                                         DomesticViolenceVictim == 0 ~ "No",
                                         DomesticViolenceVictim %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
                                         TRUE ~ "Data.Not.Collected")) %>%
        return_household_groups(., dv_experience, y_n_dkr_dnc_list) %>%
        adorn_totals("row")
    }
    
    # Q14b
    {
      Q14b <- Q14 %>%
        filter(DomesticViolenceVictim == 1) %>%
        mutate(currently_fleeing = case_when(CurrentlyFleeing == 1 ~ "Yes",
                                             CurrentlyFleeing == 0 ~ "No",
                                             CurrentlyFleeing %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
                                             TRUE ~ "Data.Not.Collected")) %>%
        return_household_groups(., currently_fleeing, y_n_dkr_dnc_list) %>%
        adorn_totals("row")
    }
    
    # Q15
    {
      for(residence_type in unique(na.omit(ResidenceUses$APR_PriorLocationGroup))) {
        residences_to_include <- ResidenceUses %>%
          filter(APR_PriorLocationGroup == residence_type &
                   !is.na(LocationDescription) &
                   !is.na(APR_PriorOrder)) %>%
          arrange(APR_PriorOrder) %>%
          mutate(LocationDescription = case_when(
            Location %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
            TRUE ~ LocationDescription))
        
        group_of_residences <- recent_household_enrollment %>%
          keep_adults_and_hoh_only() %>%
          inner_join(residences_to_include, by = c("LivingSituation" = "Location")) %>%
          return_household_groups(., LocationDescription, residences_to_include$LocationDescription) %>%
          # full_join(residences_to_include, by = "LocationDescription") %>%
          # arrange(APR_PriorOrder) %>%
          adorn_totals("row")
        
        if (exists(("Q15"))) {
          Q15 <- Q15 %>%
            union(group_of_residences)
        } else {
          Q15 <- group_of_residences
        }
      }
      
      Q15 <- Q15 %>%
        select(LocationDescription, Total, Without.Children,
               With.Children.And.Adults, With.Only.Children, Unknown.Household.Type) %>%
        mutate(LocationDescription = if_else(
          LocationDescription == "Total", "Subtotal", LocationDescription
        )) %>%
        ifnull(., 0)
      
      total_row <- Q15 %>%
        filter(LocationDescription == "Subtotal") %>%
        select(-LocationDescription) %>%
        colSums() %>%
        t() %>%
        as.data.frame() %>%
        mutate(LocationDescription = "Total") %>%
        select(colnames(Q15))
      
      Q15 <- rbind(Q15, total_row)
    }
    
    # Q16
    {
      entry_income <- recent_household_enrollment %>%
        keep_adults_only() %>%
        left_join(IncomeBenefits %>%
                    select(-PersonalID) %>%
                    filter(DataCollectionStage == 1),
                  by = c("EnrollmentID", "EntryDate" = "InformationDate"))
      
      annual_income <- recent_household_enrollment %>%
        keep_adults_only() %>%
        filter(is.na(ExitDate)) %>%
        get_annual_id(., IncomeBenefits, IncomeBenefitsID) %>%
        left_join(IncomeBenefits %>%
                    select(colnames(IncomeBenefits)[colnames(IncomeBenefits) %nin% colnames(recent_household_enrollment)]),
                  by = c("IncomeBenefitsID" = "IncomeBenefitsID"))
      
      exit_income <- recent_household_enrollment %>%
        keep_adults_only() %>%
        filter(!is.na(ExitDate)) %>%
        left_join(IncomeBenefits %>%
                    select(-PersonalID) %>%
                    filter(DataCollectionStage == 3),
                  by = c("EnrollmentID", "ExitDate" = "InformationDate"))
      
      for(period in entry_annual_exit) {
        
        data <- get(paste0(period, "_income")) %>%
          determine_total_income(., annual = period == "annual") %>%
          create_income_groups(., annual = period == "annual") %>%
          `colnames<-`(c("total_income_group", paste0(period, "Income")))
        
        assign(paste0(period, "_income_groups"), data)
      }
      
      Q16 <- entry_income_groups %>%
        full_join(annual_income_groups, by = "total_income_group") %>%
        left_join(exit_income_groups, by = "total_income_group") %>%
        rename(Income.at.Start = entryIncome,
               Income.at.Latest.Annual.Assessment.for.Stayers = annualIncome,
               Income.at.Exit.for.leavers = exitIncome) %>%
        adorn_totals("row") 
    }
    
    # Q17
    {
      Q17 <- create_income_sources(recent_household_enrollment)
    }
    
    # Q18
    {
      
      Q18_data <- recent_household_enrollment %>%
        create_income_categories(.) %>%
        adorn_totals("row")
      
      has_income <- Q18_data %>%
        filter(income_category %in% c("Adults with Only Earned Income (i.e., Employment Income)", 
                                      "Adults with Only Other Income", 
                                      "Adults with Both Earned and Other Income")) %>%
        select(-income_category) %>% 
        colSums()
      
      Q18 <- Q18_data %>%
        rbind(., c("1 or more source of income", has_income)) %>%
        rbind(., income_information_present(recent_household_enrollment))
    }
    
    # Q19a1
    {
      needed_columns <- c("earned_amount", "other_amount", "calculated_total_income")
      
      for(period in entry_annual_exit[1:2]) {
        income_for_changes <- get(paste0(period, "_income")) %>%
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
      
      for(row in c("earned", "other", "total")){
        titles <- paste(c("Number of Adults with", "Average Change in"), 
                        str_to_title(row), "Income")
        data <- entry_income_for_changes %>%
          get_income_type_changes(., row, "annual") %>%
          cbind(titles, .)
        
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
      
      for(row in c("earned", "other", "total")){
        titles <- paste(c("Number of Adults with", "Average Change in"), 
                        str_to_title(row), "Income")
        data <- entry_income_for_changes %>%
          get_income_type_changes(., row, "exit") %>%
          cbind(titles, .)
        
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
      income_rows_to_show <- c("Earned", "SSI", "SSDI", "VADisabilityService", 
                               "PrivateDisability", "WorkersComp", "TANF", 
                               "SocSecRetirement", "Pension", "ChildSupport",
                               "other_income_created")
      
      for(condition_presence in c("disabling_condition",
                                  "no_disabling_condition",
                                  "all")) {
        
        filtered_income_information <- exit_income %>%
          filter((condition_presence == "disabling_condition" &
                    DisablingCondition == 1) |
                   (condition_presence == "no_disabling_condition" &
                      DisablingCondition == 0) |
                   (condition_presence == "all" &
                      DisablingCondition %in% c(0, 1))) %>%
          mutate(other_income_created = if_else(
            Unemployment == 1 |
              VADisabilityNonService == 1 |
              GA == 1 |
              Alimony == 1 |
              OtherIncomeSource == 1, 1, 0)) %>%
          select(c(PersonalID, household_type, all_of(income_rows_to_show))) %>%
          pivot_longer(!c(PersonalID, household_type)) %>%
          mutate(row_name = "row")
        
        unduplicated_total <- cbind(name = "total_adults", 
                                    filtered_income_information %>%
                                      mutate(row_name = "total") %>%
                                      return_household_groups(., row_name, "total") %>%
                                      select(-row_name))
        
        
        filtered_has_income <- filtered_income_information %>%
          filter(value == 1)
        
        unduplicated_with_income <- filtered_has_income %>%
          mutate(row_name = "has income") %>%
          return_household_groups(., row_name, "has income")
        
        unduplicated_without_income <- as.data.frame(
          cbind(name = "no_income", unduplicated_total[2:6] - unduplicated_with_income[2:6]))
        
        Q19b_data <- as.data.frame(income_rows_to_show) %>%
          rename(name = income_rows_to_show) %>%
          left_join(filtered_has_income %>%
                      return_household_groups(., name, IncomeTypes$IncomeGroup),
                    by = "name") %>%
          rbind(., unduplicated_without_income) %>%
          rbind(., unduplicated_total) %>%
          `colnames<-`(c("name", paste0(condition_presence, "_", colnames(.)[2:6]))) %>%
          ifnull(., 0)
        
        if(condition_presence == "disabling_condition") {
          Q19b <- Q19b_data
        } else {
          Q19b <- Q19b %>%
            left_join(Q19b_data, by = "name")
        }
      }
      
      for(household_type in c("Without.Children", "With.Children.And.Adults", "Unknown.Household.Type")) {
        Q19b <- Q19b %>%
          mutate(!!paste0(household_type, "_percent") := ifnull(
            get(paste0("disabling_condition_", household_type)) / 
              get(paste0("all_", household_type)), 0)
          )
      }
      
      Q19b <- Q19b %>%
        select(name, disabling_condition_Without.Children,
               no_disabling_condition_Without.Children, all_Without.Children,
               Without.Children_percent, disabling_condition_With.Children.And.Adults,
               no_disabling_condition_With.Children.And.Adults, all_With.Children.And.Adults,
               With.Children.And.Adults_percent, disabling_condition_Unknown.Household.Type,
               no_disabling_condition_Unknown.Household.Type, all_Unknown.Household.Type, Unknown.Household.Type_percent) 
      
      no_income_row <- match(TRUE, Q19b$name == "no_income")
      total_row <- match(TRUE, Q19b$name == "total_adults")
      
      Q19b$Without.Children_percent[c(no_income_row, total_row)] <- NA
      Q19b$With.Children.And.Adults_percent[c(no_income_row, total_row)] <- NA
      Q19b$Unknown.Household.Type_percent[c(no_income_row, total_row)] <- NA
    }
    
    # Q20a
    {
      Q20a <- create_benefit_groups(recent_household_enrollment)
    }
    
    # Q20b
    {
      for(period in entry_annual_exit) {
        
        data <- get(paste0(period, "_income")) %>%
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
          summarise(!!paste0(period, "_people") := n_distinct(PersonalID))
        
        assign(paste0(period, "_benefit_counts"), data)
      }
      
      benefit_count <- c("No sources", "One or more source(s)", "Does.Not.Know.or.Refused",
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
      insurance_list <- c("Medicaid", "Medicare", "SCHIP", "VAMedicalServices", 
                          "EmployerProvided", "COBRA", "PrivatePay", "StateHealthIns",
                          "IndianHealthServices", "OtherInsurance")
      
      for(period in entry_annual_exit) {
        
        data <- get(paste0(period, "_income")) %>%
          mutate(insurance_count = ifnull(Medicaid, 0) + ifnull(Medicare, 0) +
                   ifnull(SCHIP, 0) + ifnull(VAMedicalServices, 0) +
                   ifnull(EmployerProvided, 0) + ifnull(COBRA, 0) +
                   ifnull(PrivatePay, 0) + ifnull(StateHealthIns, 0) +
                   ifnull(IndianHealthServices, 0) + ifnull(OtherInsurance, 0)
          )
        
        insurance_types <- data %>%
          filter(period != "annual" |
                   HouseholdID %in% annual_assessment_dates$HouseholdID) %>%
          select(all_of(insurance_list)) %>%
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
                InsuranceFromAnySource %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
                TRUE ~ "Data.Not.Collected"),
            insurance_count == 1 ~ "One source of insurance",
            TRUE ~ "More than one source of insurance")) %>%
          group_by(InsuranceType) %>%
          summarize(!!paste0(period, "Clients") := n_distinct(PersonalID))
        
        assign(paste0(period, "_insurance_types"), insurance_types %>%
                 rbind(insurance_present))
      }
      
      full_insurance_list <- c(InsuranceTypes$OfficialInsuranceName, 
                               c("No health insurance", "Does.Not.Know.or.Refused", "Data.Not.Collected", 
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
      
      # also still need to add bed night calculations
      Q22_data <- recent_household_enrollment %>%
        add_length_of_time_groups(., EntryDate, 
                                  ifnull(ExitDate, ymd(report_end_date) + days(1)),
                                  "APR") %>%
        rename(days_enrolled = number_of_days,
               enrollment_length_group = number_of_days_group) %>%
        group_by(enrollment_length_group)
      
      Q22a1_total <- Q22_data %>%
        summarise(Total = n_distinct(PersonalID))
      
      Q22a1_leavers <- Q22_data %>%
        filter(!is.na(ExitDate)) %>%
        summarise(Leavers = n_distinct(PersonalID))
      
      Q22a1_stayers <- Q22_data %>%
        filter(is.na(ExitDate)) %>%
        summarise(Stayers = n_distinct(PersonalID))
      
      Q22a1 <- length_of_time_groups("APR", "enrollment_length_group") %>%
        left_join(Q22a1_total, by = "enrollment_length_group") %>%
        left_join(Q22a1_leavers, by = "enrollment_length_group") %>%
        left_join(Q22a1_stayers, by = "enrollment_length_group") %>%
        adorn_totals("row") %>%
        ifnull(., 0)
    }
    
    # Q22a2
    {
      Q22a2_total <- Q22_data %>%
        summarise(Total = n_distinct(PersonalID))
      
      Q22a2_leavers <- Q22_data %>%
        filter(!is.na(ExitDate)) %>%
        summarise(Leavers = n_distinct(PersonalID))
      
      Q22a2_stayers <- Q22_data %>%
        filter(is.na(ExitDate)) %>%
        summarise(Stayers = n_distinct(PersonalID))
      
      Q22a2 <- length_of_time_groups("CAPER", "enrollment_length_group") %>%
        left_join(Q22a2_total, by = "enrollment_length_group") %>%
        left_join(Q22a2_leavers, by = "enrollment_length_group") %>%
        left_join(Q22a2_stayers, by = "enrollment_length_group") %>%
        adorn_totals("row") %>%
        ifnull(., 0)
    }
    
    # Q22b
    {
      Q22b <- Q22_data %>%
        ungroup() %>%
        summarise(summary = "Average Length",
                  Leavers = mean(days_enrolled[!is.na(ExitDate)]),
                  Stayers = mean(days_enrolled[is.na(ExitDate)])) %>%
        rbind(Q22_data %>%
                ungroup() %>%
                summarise(summary = "Median Length",
                          Leavers = median(days_enrolled[!is.na(ExitDate)]),
                          Stayers = median(days_enrolled[is.na(ExitDate)])))
    }
    
    # Q22c
    {
      Q22c_data <- recent_household_enrollment %>%
        filter((HoH_HMID >= report_start_date &
                  HoH_HMID <= report_end_date) |
                 (is.na(HoH_HMID) &
                    ExitDate <= report_end_date)) %>%
        mutate(move_in_date = case_when(
          EntryDate > HoH_HMID ~ EntryDate,
          TRUE ~ HoH_HMID)) %>%
        add_length_of_time_groups(., EntryDate, move_in_date, "detailed") %>%
        rename(days_to_house = number_of_days,
               housing_length_group = number_of_days_group)
      
      average_time_to_house <- Q22c_data %>%
        summarise(housing_length_group = "Average length of time to housing",
                  Total = mean(days_to_house),
                  Without.Children = mean(days_to_house[household_type == "AdultsOnly"]),
                  With.Children.And.Adults = mean(days_to_house[household_type == "AdultsAndChildren"]),
                  With.Only.Children = mean(days_to_house[household_type == "ChildrenOnly"]),
                  Unknown.Household.Type = mean(days_to_house[household_type == "Unknown"]))
      
      exited_without_move_in <- recent_household_enrollment %>%
        filter(is.na(HoH_HMID) &
                 !is.na(ExitDate)) %>%
        mutate(housing_length_group = "Persons who were exited without move-in") %>%
        return_household_groups(., housing_length_group, "Persons who were exited without move-in") 
      
      time_to_house_groups <- length_of_time_groups("detailed", "housing_length_group") %>%
        filter(housing_length_group %nin% c("731 - 1,095 days",
                                            "1,096 - 1,460 days",
                                            "1,461 - 1,825 days",
                                            "More than 1,825 days"))
      
      Q22c <- Q22c_data %>%
        return_household_groups(., housing_length_group, time_to_house_groups$housing_length_group) %>%
        adorn_totals("row") %>%
        mutate(housing_length_group = case_when(
          housing_length_group == "Total" ~ "Total (persons moved into housing)",
          TRUE ~ housing_length_group)) %>%
        union(average_time_to_house) %>%
        union(exited_without_move_in) %>%
        union(Q22c_data %>%
                mutate(housing_length_group = "Total persons") %>%
                return_household_groups(., housing_length_group, "Total persons")) %>%
        ifnull(., 0)
    }
    
    # Q22d
    {
      
      Q22d <- Q22_data %>%
        return_household_groups(., enrollment_length_group, 
                                length_of_participation$enrollment_length_group) %>%
        adorn_totals("row") %>%
        ifnull(., 0) 
    }
    
    #Q22e
    {
      Q22e_data <- recent_household_enrollment %>%
        mutate(housing_date = case_when(
          ProjectType %nin% c(3, 9, 13) |
            EntryDate > HoH_HMID ~ EntryDate,
          TRUE ~ HoH_HMID),
          homelessness_start_date = case_when(
            age < 18 &
              EntryDate == HoH_EntryDate ~ HoH_ADHS,
            (is.na(age) | age >= 18) &
              DateToStreetESSH <= EntryDate ~ DateToStreetESSH)) %>%
        add_length_of_time_groups(., homelessness_start_date, housing_date, "days_prior_to_housing") %>%
        mutate(number_of_days_group = case_when(is.na(housing_date) ~ "Not yet moved into housing",
                                                TRUE ~ number_of_days_group)) %>%
        rename(days_prior_to_housing = number_of_days_group)
      
      homeless_to_housed_groups <- length_of_time_groups("days_prior_to_housing", "days_prior_to_housing")
      
      Q22e_groups <- Q22e_data %>%
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
      Q23c <- create_destination_groups(recent_household_enrollment)
    }
    
    # Q24
    {
      
      Q24 <- recent_household_enrollment %>%
        filter(!is.na(ExitDate) &
                 ProjectType == 12) %>%
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
          HousingAssessment == 99 ~ assessment_outcomes[14])) %>%
        return_household_groups(., assessment_at_exit, assessment_outcomes) %>%
        adorn_totals("row") %>%
        ifnull(., 0)
        
    }
    
    #-------------------------------------------------------------
    #------------------- Veteran Questions -----------------------
    #-------------------------------------------------------------
    
    recent_veteran_enrollment <- recent_household_enrollment %>%
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
      
      Q25a <- recent_household_enrollment %>%
        left_join(chronicity_data, by = "EnrollmentID") %>%
        mutate(category = case_when(
          new_veteran_status == 1 &
            chronic == 1 ~ vet_chronic_categories[1],
          new_veteran_status == 1 ~ vet_chronic_categories[2],
          new_veteran_status == 0 ~ vet_chronic_categories[3],
          new_veteran_status %in% c(8, 9) ~ vet_chronic_categories[4],
          TRUE ~ vet_chronic_categories[5])) %>%
        return_household_groups(., category, vet_chronic_categories) %>%
        adorn_totals("row") %>%
        ifnull(., 0)
    }
    
    # Q25b
    {
      ## see note above
    }
    
    # Q25c
    {
      Q25c <- recent_veteran_enrollment %>%
        create_gender_groups(.)
    }
    
    # Q25d
    {
      Q25d <- recent_veteran_enrollment %>%
        create_age_groups(.) %>%
        select(-With.Only.Children) %>%
        filter(detailed_age_group %nin% detailed_age_group_list[1:3])
    }
    
    # Q25e
    {
      
      Q25e.1.and.2 <- recent_veteran_enrollment %>%
        inner_join(disability_table %>%
                     filter(DataCollectionStage %in% c(1, 3)), by ="EnrollmentID") %>%
        group_by(disability_name) %>%
        summarise(Conditions.At.Start = n_distinct(PersonalID[DataCollectionStage == 1]),
                  Conditions.At.Latest.Assessment.for.Stayers = n_distinct(
                    PersonalID[DataCollectionStage == 3 & !is.na(ExitDate)]))
      
      Q25e.3 <- Q13c %>%
        filter(EnrollmentID %in% recent_veteran_enrollment$EnrollmentID) %>%
        group_by(disability_name) %>%
        summarise(Conditions.At.Exit.for.Leavers = n_distinct(PersonalID))
      
      # for comparing purposes, the existing kit has stayers and leavers flipped
      Q25e <- as.data.frame(disability_list)  %>%
        `colnames<-`(c("disability_name")) %>%
        full_join(Q25e.1.and.2, by = "disability_name") %>%
        full_join(Q25e.3, by = "disability_name") %>%
        ifnull(0)
      
    }
    
    # Q25f
    # Q26f specifies that rows 10 and 11 are excluded intentionally, Q25f does
    # not specify this but it also does not show these rows in the table
    {
      Q25f <- recent_veteran_enrollment %>%
        create_income_categories(.) %>%
        adorn_totals("row")
    }
    
    # Q25g
    {
      Q25g <- create_income_sources(recent_veteran_enrollment)
    }
    
    # Q25h
    {
      Q25h <- create_benefit_groups(recent_veteran_enrollment)
    }
    
    # Q25i
    {
      Q25i <- create_destination_groups(recent_veteran_enrollment)
    }
    
    #-------------------------------------------------------------
    #-------------------------------------------------------------
    #-------------------------------------------------------------
 #use below bracket for testing   
}
    
    projects_included <- max(case_when(length(project_list) > 1 ~ "Multiple Projects",
                                       TRUE ~ paste0(Project$ProjectName[Project$ProjectID %in% project_list])))
    
    APR_relevant <- max(length(intersect(c(1:7),
                                         unique(Funder$Funder[Funder$ProjectID %in% project_list]))) > 0 |
                          project_list == 1552)
    
    CAPER_relevant <- length(intersect(c(8:11),
                                       unique(Funder$Funder[Funder$ProjectID %in% project_list]))) > 0
    
    
    if (generate_new_kits) {
      if(APR_relevant) {
        for (question in APR_files) {
          if (exists(question)) {
            write.csv(get(question), file.path(paste0("created_files/ICF - ", question, ".csv")), row.names=FALSE)
          } else {
            missing_files <- c(missing_files, paste("APR -", projects_included, "-", question))
          }
        }
        archive_write_dir(paste0("APR - ", projects_included, ".zip"),
                          paste0(getwd(), "/created_files"))
        unlink(paste0(getwd(), "/created_files/*"))
      }
      
      if(CAPER_relevant) {
        for (question in CAPER_files) {
          if (exists(question)) {
            write.csv(get(question), file.path(paste0("created_files/ICF - ", question, ".csv")), row.names=FALSE)
          } else {
            missing_files <- c(missing_files, paste("CAPER -", projects_included, "-", question))
          }
        }
        archive_write_dir(paste0("CAPER - ", projects_included, ".zip"),
                          paste0(getwd(), "/created_files"))
        unlink(paste0(getwd(), "/created_files/*"))
      }
    }
    
    
    #-------------------------------------------------------------
    #-------------------------------------------------------------
    #-------------------------------------------------------------
    
    if (compare_to_last) {
      if ("differences" %nin% items_to_keep) {
        items_to_keep <- c(items_to_keep, "differences")
      }
      
      if(APR_relevant) {
        
        zip <- paste0(compare_to_dir, "/APR - ", projects_included, ".zip")
        zip_contents <- unzip(zip, list = TRUE)
        
        for (question in intersect(APR_files, ls())) {
          if (paste0("ICF - ", question, ".csv") %in% zip_contents$Name) {
            
            new <- get(question) %>%
              select_if(., is.numeric)
            
            old <- read_csv(unzip(zip, paste0("ICF - ", question, ".csv")),
                            show_col_types = FALSE) %>%
              as.data.frame() %>%
              select_if(., is.numeric) 
            
            new_differences <- data.frame(report = paste0("APR - ", projects_included),
                                          question = question, 
                                          differences = all.equal(new, old))
            
            if (new_differences$differences[1] != TRUE) {
              if (exists("differences")) {
                differences <- differences %>%
                  union(new_differences)
              } else {
                differences <- new_differences
              }
            }
            
            unlink(paste0(getwd(), "/ICF - ", question, ".csv"))
          }
        }
      }
      
      if(CAPER_relevant) {
        zip <- paste0(compare_to_dir, "/CAPER - ", projects_included, ".zip")
        zip_contents <- unzip(zip, list = TRUE)
        
        for (question in intersect(CAPER_files, ls())) {
          if (paste0("ICF - ", question, ".csv") %in% zip_contents$Name) {
            
            new <- get(question) %>%
              select_if(., is.numeric)
            
            old <- read_csv(unzip(zip, paste0("ICF - ", question, ".csv")),
                            show_col_types = FALSE) %>%
              as.data.frame() %>%
              select_if(., is.numeric) 
            
            new_differences <- data.frame(report = paste0("CAPER - ", projects_included),
                                          question = question, 
                                          differences = all.equal(new, old))
            
            if (new_differences$differences[1] != TRUE) {
              if (exists("differences")) {
                differences <- differences %>%
                  union(new_differences)
              } else {
                differences <- new_differences
              }
            }
            unlink(paste0(getwd(), "/ICF - ", question, ".csv"))
          }
        }
      }
    }
    
    rm(list = ls()[ls() %nin% items_to_keep])
    
  }
  
  
  if (compare_to_last) {
    differences <- differences %>% 
      filter(str_detect(differences, "Mean") &
               question != "Q4a")
    
    write.csv(differences, 
              paste0("Change Tracker ", Sys.Date(), ".csv"), 
              row.names = FALSE)
  }
  
}
