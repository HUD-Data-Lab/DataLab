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

source("datalab_functions.R")
residences <- read_csv("Residences.csv",
                       col_types = "iccclllii")

# combines all files
{
  hmis_csvs <- list(Affiliation = "cccTTcTc", 
                    Assessment = "cccDciiiTTcTc", 
                    AssessmentQuestions = "ccccciccTTcTc", 
                    AssessmentResults = "ccccccTTcTc",
                    Client = "ccccciciDiiiiiiiiiiiiiciiiiiiiiiiiiiTTcTc", 
                    CurrentLivingSituation = "cccDiciiiiicTTcTc", 
                    Disabilities = "cccDiiiiiiiiiiiTTcTc", 
                    EmploymentEducation = "cccDiiiiiiTTcTc",
                    Enrollment = "cccDciiiiiDiiiDDDiiiicccciiiDiiiiciiiiiiiiiiiiciiiiiiiiiiiiiiiiiiiiTTcTc", 
                    EnrollmentCoC = "cccccDciTTcTc", 
                    Event = "cccDiiiciDTTcTc",
                    Exit = "cccDiciiiiiiiiiiiiiiiiiiiiiiiiiDiiiiiiTTcTc",
                    Export = "ciccccciiTTTccciii", 
                    Funder = "cciccDDTTcTc", 
                    HealthAndDV = "cccDiiiiiiiDiiiiiTTcTc", 
                    IncomeBenefits = "cccDididididididididididididididididciiiiiiiciiiiiiiiiiiiiiiiiiiiciiiiiiiiTTcTc", 
                    Inventory = "ccciiiiiiiiiiiiDDTTcTc", 
                    Organization = "ccncTTcTn", 
                    Project = "ccccDDnnnnnnnnnTTcTc", 
                    ProjectCoC = "ccccccccciTTcTc",
                    Services = "cccDiicciciTTcTc", 
                    User = "ccccccTTTc", 
                    YouthEducationStatus = "cccDiiiiTTcTc")
  
  datalab_zips <- list.files(paste0(getwd(), "/HMIS Test Kit 2.0/HMIS CSVs"),
                             full.names = TRUE)
  
  for (zip in datalab_zips) {
    for (file in names(hmis_csvs)){
      
      data <- read_csv(unzip(zip, paste0(file, ".csv")),
                       col_types = get(file, hmis_csvs))
      
      if (exists(file)) {
        data <- get(file) %>%
          full_join(data, by = colnames(data))
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
  
  assign(file, data)
  
}

disability_table <- Disabilities %>%
  filter(DisabilityResponse == 1 |
               (DisabilityType == 10 &
                  DisabilityResponse %in% c(2, 3))) %>%
  mutate(disability_name = case_when(DisabilityType == 5 ~ "physical",
                                     DisabilityType == 6 ~ "developmental",
                                     DisabilityType == 7 ~ "chronic",
                                     DisabilityType == 8 ~ "HIV/AIDS",
                                     DisabilityType == 9 ~ "mental_health",
                                     DisabilityResponse == 1 ~ "alcohol_use",
                                     DisabilityResponse == 2 ~ "drug_use",
                                     DisabilityResponse == 3 ~ "alcohol_and_drug_use"),
         disabilities = if_else(disability_name == "alcohol_and_drug_use", 2, 1),
         disability_name = factor(disability_name, ordered = TRUE, 
                                  levels = c("mental_health", "alcohol_use", "drug_use",
                                             "alcohol_and_drug_use", "chronic", "HIV/AIDS",
                                             "developmental", "physical")),
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
    DisablingCondition %in% c(8, 9) ~ "DK/R",
    TRUE ~ "M"),
    homeless_year_prior = trunc((DateToStreetESSH %--% EntryDate) / years(1)) >= 1 &
           !is.na(DateToStreetESSH),
    four_or_more_times = case_when(
      TimesHomelessPastThreeYears == 4 ~ "Y",
      TimesHomelessPastThreeYears %in% c(1, 2, 3) ~ "N",
      TimesHomelessPastThreeYears %in% c(8, 9) ~ "DK/R",
      TRUE ~ "M"),
    twelve_or_more_months = case_when(
      MonthsHomelessPastThreeYears >= 112 ~ "Y",
      MonthsHomelessPastThreeYears %in% c(101, 102, 103, 104, 
                                          105, 106, 107, 108, 
                                          109, 110, 111) ~ "N",
      MonthsHomelessPastThreeYears %in% c(8, 9) ~ "DK/R",
      TRUE ~ "M"
    ),
    chronic = case_when(
      disabling_condition_for_chronic != "Y" ~ disabling_condition_for_chronic,
      ProjectType %in% c(1, 4, 8) |
        LivingSituation %in% c(16, 1, 18) ~
        case_when(
          homeless_year_prior ~ "Y",
          four_or_more_times != "Y" ~ four_or_more_times,
          TRUE ~ twelve_or_more_months
        ),
      LivingSituation %in% c(15, 6, 7, 25, 4, 5) ~
        case_when(
          LOSUnderThreshold == 0 |
            DateToStreetESSH == 0 ~ "N",
          homeless_year_prior ~ "Y",
          four_or_more_times != "Y" ~ four_or_more_times,
          TRUE ~ twelve_or_more_months
        ),
      LivingSituation %in% c(29, 14, 2, 32, 36,
                             35, 28, 19, 3, 31,
                             33, 34, 10, 20, 21,
                             11, 8, 9, 99) ~
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
                chronic == "DK/R" ~ 3,
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
           labels = c("Y", "N", "DK/R", "M"))) %>%
  ungroup() %>%
  filter(EntryDate == first_entry_date) %>%
  # select(PersonalID, EnrollmentID, HouseholdID, chronic, numeric_chronic,
  #        min_chronicity, HoH_chronicity, HoH_or_adult_chronicity, new_chronic) %>%
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
          
project_list <- c(
  # "942",    # DataLab - Coordinated Entry 
  # "1546",    # DataLab - ES-EE ESG I    
  # "1547",    # DataLab - ES-EE ESG II (with CE elements) 
  # "1544",    # DataLab - ES-EE RHY   
  # "1548",    # DataLab - ES-NbN ESG
  # "1564",    # DataLab - HP ESG
  "1552"#,    # DataLab - PSH CoC I
  # "1550",    # DataLab - PSH HOWPA  
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
                         DOBDataQuality %in% c(8, 9) ~ "DK/R",
                       TRUE ~ "DNC"), 
           age_group = case_when(age >= 18 ~ "adult",
                                 age < 18 ~ "child",
                                 TRUE ~ detailed_age_group),
           detailed_age_group = factor(detailed_age_group, ordered = TRUE, 
                                       levels = c("Under 5", "5-12", "13-17", "18-24", "25-34",
                                                  "35-44", "45-54", "55-61", "62+", "DK/R", "DNC")),) %>%
    # get a second opinion on when to apply the household type calcs
    group_by(HouseholdID) %>%
    mutate(youngest_age = min(age, na.rm = TRUE),
           oldest_age = max(age, na.rm = TRUE),
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
           youth_household, youth, has_children)
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
  mutate(adults = max(if_else(age_group == "adult", 1, 0)),
         children = max(if_else(age_group == "child", 1, 0)),
         unknown = max(if_else(age_group %in% c("DK/R", "DNC"), 1, 0)),
         HoH_HMID = min(case_when(
           RelationshipToHoH == 1 &
             ProjectType %in% c(3, 13) ~ MoveInDate
         ), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(household_type = case_when(
    adults == 1 &
      children == 1 ~ "AdultsAndChildren",
    unknown == 1 ~ "Unknown",
    adults == 1 ~ "AdultsOnly",
    TRUE ~ "ChildrenOnly"
  )) %>%
  select(PersonalID, household_type, HoH_HMID)

recent_household_enrollment <- recent_program_enrollment %>%
  left_join(client_plus, by = "PersonalID") %>%
  left_join(household_info, by = "PersonalID")

# Q4a
{
  Q4a <- Project %>%
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
              adults_served = uniqueN(PersonalID[age_group == "adult"]),
              children_served = uniqueN(PersonalID[age_group == "child"]),
              unknown_served = uniqueN(PersonalID[age_group %in% c("DK/R", "DNC")]),
              leavers = uniqueN(PersonalID[!is.na(ExitDate)]),
              adult_leavers = uniqueN(PersonalID[age_group == "adult" &
                                                   !is.na(ExitDate)]),
              adult_and_hoh_leavers = uniqueN((PersonalID[(age_group == "adult" |
                                                             RelationshipToHoH == 1) &
                                                            !is.na(ExitDate)])),
              stayers = uniqueN(PersonalID[is.na(ExitDate)]),
              adult_stayers = uniqueN(PersonalID[age_group == "adult" &
                                                   is.na(ExitDate)]),
              veterans = uniqueN(PersonalID[VeteranStatus == 1 &
                                              age_group == "adult"]),
              chronic = uniqueN(PersonalID[chronic == "Y"]),
              youth = uniqueN(PersonalID[youth == 1]),
              # can't figure out why this one isn't working
              parenting_youth = uniqueN(PersonalID[has_children == 1 & youth == 1]),
              adult_hoh = uniqueN(PersonalID[age_group == "adult" &
                                               RelationshipToHoH == 1]),
              other_hoh = uniqueN(PersonalID[age_group != "adult" &
                                               RelationshipToHoH == 1]),
              long_stayers = uniqueN(PersonalID[is.na(ExitDate) &
                                                  trunc((EntryDate %--% report_end_date) / days(1)) >= 365 &
                                                  (age_group == "adult" |
                                                     RelationshipToHoH == 1)])
              ) %>% 
    mutate(rowname = "All_Clients") %>% 
    pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
    pivot_wider(names_from = "rowname", values_from = "values")
  
  Q5a_dq <- recent_program_enrollment_dq %>%
    left_join(client_plus, by = "PersonalID") %>%
    left_join(chronicity_data, by = "EnrollmentID") %>%
    summarise(persons_served = n_distinct(PersonalID),
              adults_served = uniqueN(PersonalID[age_group == "adult"]),
              children_served = uniqueN(PersonalID[age_group == "child"]),
              unknown_served = uniqueN(PersonalID[age_group %in% c("DK/R", "DNC")]),
              leavers = uniqueN(PersonalID[!is.na(ExitDate)]),
              adult_leavers = uniqueN(PersonalID[age_group == "adult" &
                                                   !is.na(ExitDate)]),
              adult_and_hoh_leavers = uniqueN((PersonalID[(age_group == "adult" |
                                                             RelationshipToHoH == 1) &
                                                            !is.na(ExitDate)])),
              stayers = uniqueN(PersonalID[is.na(ExitDate)]),
              adult_stayers = uniqueN(PersonalID[age_group == "adult" &
                                                   is.na(ExitDate)]),
              veterans = uniqueN(PersonalID[VeteranStatus == 1 &
                                              age_group == "adult"]),
              chronic = uniqueN(PersonalID[chronic == "Y"]),
              youth = uniqueN(PersonalID[youth == 1]),
              # can't figure out why this one isn't working
              parenting_youth = uniqueN(PersonalID[has_children == 1 & youth == 1]),
              adult_hoh = uniqueN(PersonalID[age_group == "adult" &
                                               RelationshipToHoH == 1]),
              other_hoh = uniqueN(PersonalID[age_group != "adult" &
                                               RelationshipToHoH == 1]),
              long_stayers = uniqueN(PersonalID[is.na(ExitDate) &
                                                  trunc((EntryDate %--% report_end_date) / days(1)) >= 365 &
                                                  (age_group == "adult" |
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
      NameDataQuality %in% c(8, 9) ~ "DK/R",
      NameDataQuality == 99 |
        is.na(FirstName) |
        is.na(LastName) ~ "M",
      NameDataQuality == 2 ~ "DI",
      TRUE ~ "OK")) 
    
  Q6a_ssn <- recent_program_enrollment_dq %>%
    inner_join(Client, by = "PersonalID") %>%
    mutate(sequential = lapply(SSN, sequential_ssn),
           dq_flag = case_when(
             SSNDataQuality %in% c(8, 9) ~ "DK/R",
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
        is.na(DOB) ~ "DK/R",
      is.na(DOBDataQuality) |
        (DOBDataQuality == 99 &
           is.na(DOB)) ~ "M",
      DOBDataQuality == 2 |
        (DOBDataQuality %in% c(8, 9, 99) &
           !is.na(DOB)) |
        DOB < mdy("1/1/1915") |
        DOB > DateCreated |
        (DOB >= EntryDate &
           (age_group == "adult" |
              RelationshipToHoH == 1))
        
        ~ "DI",
      TRUE ~ "OK")) 
  
  Q6a_race <- recent_program_enrollment_dq %>%
    inner_join(Client, by = "PersonalID") %>%
    mutate(dq_flag = case_when(
      RaceNone %in% c(8, 9) ~ "DK/R",
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
      Ethnicity %in% c(8, 9) ~ "DK/R",
      Ethnicity == 99 ~ "M",
      TRUE ~ "OK")) 
  
  Q6a_gender <- recent_program_enrollment_dq %>%
    inner_join(Client, by = "PersonalID") %>%
    mutate(dq_flag = case_when(
      GenderNone %in% c(8, 9) ~ "DK/R",
      GenderNone == 99 |
        (Female == 0 &
           Male == 0 &
           NoSingleGender == 0 &
           Transgender == 0 &
           Questioning == 0) ~ "M",
      TRUE ~ "OK")) 
  
  columns <- c("DataElement", "DK/R", "M", "DI", "OK")
  
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
    mutate(`DK/R` = if_else(is.na(`DK/R`), 0, as.double(`DK/R`)),
           M = if_else(is.na(M), 0, as.double(M)),
           DI = if_else(is.na(DI), 0, as.double(DI)),
           Total = `DK/R` + M + DI) %>%
    add_row(DataElement = "Overall Score", 
            `DK/R` = 0, M = 0, DI = 0, 
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
                           age_group == "adult") |
             (age_group == "child" &
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
    inner_join(income_sources %>%
                 filter(DataCollectionStage == 5) %>%
                 select(EnrollmentID, InformationDate, IncomeBenefitsID),
               by = c("EnrollmentID" = "EnrollmentID")) %>%
    get_annual_id(., "IncomeBenefitsID") %>%
    left_join(income_sources, by = "IncomeBenefitsID") %>%
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
                                age_group == "adult") &
             (enroll_IncomeFromAnySource %in% c(8, 9, 99) |
                is.na(enroll_IncomeFromAnySource) |
                (enroll_IncomeFromAnySource == 0 &
                   !is.na(enroll_number_of_sources) &
                   enroll_number_of_sources > 0) |
                (enroll_IncomeFromAnySource == 1 &
                   (is.na(enroll_number_of_sources) |
                      enroll_number_of_sources == 0))),
           income_annual_dq = (RelationshipToHoH == 1 |
                                age_group == "adult") &
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
                                 age_group == "adult") &
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
    filter(EntryDate >= mdy("10/1/2016") &
             (RelationshipToHoH == 1 |
                age_group == "adult")) %>%
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
    return_household_groups(., client_group) 
  
  Q7a_moved_in <- recent_household_enrollment %>%
    filter(HoH_HMID <= report_end_date) %>% 
    mutate(client_group = "MovedIn") %>%
    return_household_groups(., client_group) 
  
  Q7a <- recent_household_enrollment %>%
    return_household_groups(., age_group) %>%
    rename(client_group = age_group) %>%
    union(Q7a_all) 
  
  if(nrow(Q7a_moved_in) > 0) {
    Q7a <- Q7a %>%
      union(Q7a_moved_in)
  }
  
  # did this one with table(), want to talk through whether this is the best call
  # Q7a <- table(Q7a$age_group, Q7a$household_type)
}

# Q7b
{
  year_household_info <- all_program_enrollments %>%
    group_by(HouseholdID) %>%
    mutate(HoH_HMID = min(case_when(
             RelationshipToHoH == 1 ~ MoveInDate), na.rm = TRUE)) %>%
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
    mutate(client_group = "TotalHouseholds") %>%
    return_household_groups(., client_group) 
  
  Q8a_moved_in <- recent_household_enrollment %>%
    filter(RelationshipToHoH == 1 &
             HoH_HMID <= report_end_date) %>% 
    mutate(client_group = "MovedInHouseholds") %>%
    return_household_groups(., client_group) 
  
  if(nrow(Q8a_moved_in) > 0) {
    Q8a <- Q8a_all %>%
      union(Q8a_moved_in)
  } else {
    Q8a <- Q8a_all 
  }
  
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
        pivot_longer(!ContactGroup, names_to = "Group", values_to = "values") %>%
        pivot_wider(names_from = "Group", values_from = "values") %>%
        adorn_totals("row")
    }
    
    Q9a <- recent_household_enrollment %>%
      filter((ProjectType == 4 |
                (ProjectType == 1 &
                   TrackingMethod == 3)) &
               (PersonalID %in% recent_CLS_for_Q9$PersonalID |
                  (DateOfEngagement >= report_start_date &
                     DateOfEngagement <= report_end_date)) &
               (RelationshipToHoH == 1 |
                  age_group == "adult")) %>%
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
                  DateOfEngagement <= report_end_date) &
               (RelationshipToHoH == 1 |
                  age_group == "adult")) %>%
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
  Q10_genders <- Client %>%
    mutate(gender_combined = case_when(
      Questioning == 1 ~ "Questioning",
      NoSingleGender == 1 ~ "NoSingleGender",
      Transgender == 1 ~ "Transgender",
      Female == 1 ~ "Female",
      Male == 1 ~ "Male",
      GenderNone %in% c(8, 9) ~ "DK/R",
      TRUE ~ "DNC"),
      gender_combined = factor(gender_combined, ordered = TRUE, 
                               levels = c("Male", "Female", "NoSingleGender", "Questioning", 
                                          "Transgender", "DK/R", "DNC"))) %>%
    select(PersonalID, gender_combined)
  
  Q10a <- recent_household_enrollment %>%
    filter(age_group == "adult") %>%
    left_join(Q10_genders, by = "PersonalID") %>%
    return_household_groups(., gender_combined) %>%
    adorn_totals("row")
}

# Q10b
{
  Q10b_data <- recent_household_enrollment %>%
    filter(age_group == "child") 
      
  if(nrow(Q10b_data) > 0) {
    Q10b <- Q10b_data %>%
      left_join(Q10_genders, by = "PersonalID") %>%
      return_household_groups(., gender_combined) %>%
      mutate_at(colnames(Q10a[2:5]), as.numeric) %>%
      adorn_totals("row")
  }
}

# Q10c
{
  Q10c_data <- recent_household_enrollment %>%
    filter(age_group %in% c("DK/R", "DNC")) 
  
  if(nrow(Q10c_data) > 0) {
    Q10c <- Q10c_data %>%
      left_join(Q10_genders, by = "PersonalID") %>%
      return_household_groups(., gender_combined) %>%
      mutate_at(colnames(Q10a[2:5]), as.numeric) %>%
      adorn_totals("row")
  }
}

# Q10d
{
  Q10d <- recent_household_enrollment %>%
    left_join(Q10_genders, by = "PersonalID") %>%
    mutate(detailed_age_group = as.character(detailed_age_group),
      Q10d_age_group = case_when(
      detailed_age_group %in% c("Under 5", "5-12", "13-17") ~ "Under18",
      detailed_age_group %in% c("25-34", "35-44", "45-54", "55-61") ~ "25-61",
      TRUE ~ detailed_age_group)) %>%
    group_by(gender_combined) %>%
    summarise(total = n_distinct(PersonalID),
              under_18 = n_distinct(PersonalID[Q10d_age_group == "Under18"]),
              age_18_24 = n_distinct(PersonalID[Q10d_age_group == "18-24"]),
              age_25_61 = n_distinct(PersonalID[Q10d_age_group == "25-61"]),
              age_62_and_up = n_distinct(PersonalID[Q10d_age_group == "62+"]),
              age_unknown_refused = n_distinct(PersonalID[Q10d_age_group == "DK/R"]),
              age_not_collected = n_distinct(PersonalID[Q10d_age_group == "DNC"])) %>%
    adorn_totals("row")
}

# Q11
{
  Q11 <- recent_household_enrollment %>%
    return_household_groups(., detailed_age_group) %>%
    adorn_totals("row")
}

# Q12a
{
  Q12_races <- Client %>%
    mutate(race_combined = case_when(
      AmIndAKNative + Asian + BlackAfAmerican +
        NativeHIPacific + White > 1 ~ "MultipleRaces",
      White == 1 ~ "White",
      BlackAfAmerican == 1 ~ "BlackAfAmerican",
      Asian == 1 ~ "Asian",
      AmIndAKNative == 1 ~ "AmIndAKNative",
      NativeHIPacific == 1 ~ "NativeHIPacific",
      RaceNone %in% c(8, 9) ~ "DK/R",
      TRUE ~ "DNC"),
      race_combined = factor(race_combined, ordered = TRUE, 
                               levels = c("White", "BlackAfAmerican", "Asian", 
                                          "AmIndAKNative", "NativeHIPacific", "DK/R", "DNC"))) %>%
    select(PersonalID, race_combined)
  
  Q12a <- recent_household_enrollment %>%
    left_join(Q12_races, by = "PersonalID") %>%
    return_household_groups(., race_combined) %>%
    adorn_totals("row")
}

# Q12b
{
  Q12b <- recent_household_enrollment %>%
    left_join(Client %>%
                select(PersonalID, Ethnicity), by = "PersonalID") %>%
    mutate(display_ethnicity = case_when(Ethnicity == 1 ~ "Hispanic/Latinx",
                                         Ethnicity == 0 ~ "NotHispanic/Latinx",
                                         Ethnicity %in% c(8, 9) ~ "DK/R",
                                         TRUE ~ "DNC"),
      display_ethnicity = factor(display_ethnicity, ordered = TRUE,
                                 levels = c("NotHispanic/Latinx", "Hispanic/Latinx",
                                            "DK/R", "DNC"))) %>%
    return_household_groups(., display_ethnicity) %>%
    adorn_totals("row")
}

# Q13a1
{
  Q13a <- recent_household_enrollment %>%
    inner_join(disability_table %>%
    filter(DataCollectionStage == 1), by ="EnrollmentID")
  
  Q13a1 <- Q13a %>%
    return_household_groups(., disability_name)
}

# Q13b1
{
  Q13b <- recent_household_enrollment %>%
    filter(!is.na(ExitDate)) %>%
    inner_join(disability_table %>%
                 filter(DataCollectionStage == 3), by ="EnrollmentID")
  
  Q13b1 <- Q13b %>%
    return_household_groups(., disability_name)
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
    return_household_groups(., disability_name)
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
    return_household_groups(., disability_count_group) %>%
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
    return_household_groups(., disability_count_group) %>%
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
    return_household_groups(., disability_count_group) %>%
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
                                     DomesticViolenceVictim %in% c(8, 9) ~ "DK/R",
                                     TRUE ~ "DNC"),
           dv_experience = factor(dv_experience, ordered = TRUE,
                                  levels = c("Yes", "No", "DK/R", "DNC"))) %>%
    return_household_groups(., dv_experience) %>%
    adorn_totals("row")
}

# Q14b
{
  Q14b <- Q14 %>%
    filter(DomesticViolenceVictim == 1) %>%
    mutate(currently_fleeing = case_when(CurrentlyFleeing == 1 ~ "Yes",
                                     CurrentlyFleeing == 0 ~ "No",
                                     CurrentlyFleeing %in% c(8, 9) ~ "DK/R",
                                     TRUE ~ "DNC"),
           currently_fleeing = factor(currently_fleeing, ordered = TRUE,
                                  levels = c("Yes", "No", "DK/R", "DNC"))) %>%
    return_household_groups(., currently_fleeing) %>%
    adorn_totals("row")
}

# Q15
{
  for(residence_type in unique(na.omit(residences$APR_PriorLocationGroup))) {
    residences_to_include <- residences %>%
      filter(APR_PriorLocationGroup == residence_type &
               !is.na(LocationDescription)) 
    
    group_of_residences <- recent_household_enrollment %>%
      filter(age_group == "adult" |
               RelationshipToHoH == 1) %>%
      inner_join(residences_to_include, by = c("LivingSituation" = "Location")) %>%
      return_household_groups(., LocationDescription, residences_to_include$LocationDescription[1]) %>%
      full_join(residences_to_include, by = "LocationDescription") %>%
      arrange(APR_PriorOrder) %>%
      adorn_totals("row")
    
    if (exists(("Q15"))) {
      Q15 <- Q15 %>%
        union(group_of_residences)
    } else {
      Q15 <- group_of_residences
    }
  }
  
  Q15 <- Q15 %>%
    select(LocationDescription, total, without_children,
           children_and_adults, only_children, unknown) %>%
    mutate(LocationDescription = if_else(
      LocationDescription == "Total", "Subtotal", LocationDescription
    ))
  
  Q15[is.na(Q15)] <- 0
  
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
  entry_income_groups <- recent_household_enrollment %>%
    left_join(IncomeBenefits %>%
                select(-PersonalID) %>%
                filter(DataCollectionStage == 1),
              by = c("EnrollmentID", "EntryDate" = "InformationDate")) %>%
    determine_total_income() %>%
    create_income_groups()
    
  # currently has two too many folx in the 501-1000 group
  annual_income_groups <- recent_household_enrollment %>%
    left_join(IncomeBenefits %>%
                select(-PersonalID) %>%
                filter(DataCollectionStage == 1),
              by = c("EnrollmentID", "EntryDate" = "InformationDate")) %>%
    determine_total_income() %>%
    create_income_groups()
  
  income_annual <- recent_program_enrollment %>%
    inner_join(IncomeBenefits %>%
                 select(-PersonalID) %>%
                 filter(DataCollectionStage == 5) %>%
                 select(EnrollmentID, InformationDate, IncomeBenefitsID),
               by = c("EnrollmentID" = "EnrollmentID")) %>%
    get_annual_id(., "IncomeBenefitsID") %>%
    inner_join(IncomeBenefits %>%
                 select(-PersonalID), by = "IncomeBenefitsID") %>%
    determine_total_income() %>%
    create_income_groups()
}
