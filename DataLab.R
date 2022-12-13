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

source("datalab_functions.R")
source("DataLab_Lists.R")

if (combining_files) {
  file_source_dir <- choose.dir()
  
  datalab_zips <- list.files(file_source_dir, full.names = TRUE)
  
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
} else {
  file_source <- file.choose()
  
  for (file in names(hmis_csvs)){
    
    data <- read_csv(unzip(file_source, paste0(file, ".csv")),
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

source("DataLab_hc_variables.R")

# remove deleted records exportID colummns before proceeding with processing
for (file in names(hmis_csvs)){
  
  data <- get(file) %>%
    distinct()
  
  new_ExportID <- data$ExportID[1]
  
  data <- data %>%
    select(-ExportID) %>%
    distinct() %>%
    mutate(ExportID = new_ExportID)
  
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
               ExitDate <= report_end_date)
  }
  
  if (file == "Funder") {
    data$Funder[data$ProjectID == 1552] <- 2
    data$Funder[data$ProjectID == 1554] <- 3
    data$Funder[data$ProjectID == 1565] <- 4
  }
  
  if (file == "Export") {
    data$SoftwareName <- "DataLab"
    data$SoftwareVersion <- "1.0"
  }
  
  assign(file, data)
  
}

all_bed_nights <- Services %>%
  left_join(Enrollment %>%
              filter(ProjectID %in% Project$ProjectID[Project$ProjectType == 1 &
                                                        Project$TrackingMethod == 3]) %>%
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
  filter(ProjectID %nin% Project$ProjectID[Project$ProjectType == 1 &
                                             Project$TrackingMethod == 3] |
           EnrollmentID %in% Exit$EnrollmentID[Exit$ExitDate >= report_start_date &
                                                 Exit$ExitDate <= report_end_date] |
           EnrollmentID %in% bed_nights_in_report$EnrollmentID)

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
    DisablingCondition %in% c(8, 9) ~ "Client.Does.Not.Know.or.Refused",
    TRUE ~ "Information.Missing"),
    homeless_year_prior = trunc((DateToStreetESSH %--% EntryDate) / years(1)) >= 1 &
      !is.na(DateToStreetESSH),
    four_or_more_times = case_when(
      TimesHomelessPastThreeYears == 4 ~ "Y",
      TimesHomelessPastThreeYears %in% c(1, 2, 3) ~ "N",
      TimesHomelessPastThreeYears %in% c(8, 9) ~ "Client.Does.Not.Know.or.Refused",
      TRUE ~ "Information.Missing"),
    twelve_or_more_months = case_when(
      MonthsHomelessPastThreeYears >= 112 ~ "Y",
      MonthsHomelessPastThreeYears %in% c(101, 102, 103, 104, 
                                          105, 106, 107, 108, 
                                          109, 110, 111) ~ "N",
      MonthsHomelessPastThreeYears %in% c(8, 9) ~ "Client.Does.Not.Know.or.Refused",
      TRUE ~ "Information.Missing"
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
  left_join(Client %>%
              select(-ExportID), 
            by = "PersonalID") %>%
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
                chronic == "Client.Does.Not.Know.or.Refused" ~ 3,
                chronic == "Information.Missing" ~ 4
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
           labels = c("Y", "N", "Client.Does.Not.Know.or.Refused", "Information.Missing"))) %>%
  ungroup() %>%
  filter(EntryDate == first_entry_date) %>%
  select(EnrollmentID, new_chronic)

chronicity_data <- chronic_household %>%
  full_join(chronic_individual %>%
              select(EnrollmentID, chronic),
            by = "EnrollmentID") %>%
  mutate(chronic = if_else(is.na(new_chronic), chronic, as.character(new_chronic))) %>%
  select(-new_chronic)