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

for (filename in unique(CSV_columns$File)) {
  
  assign(paste0(tolower(filename), "_columns"), 
         CSV_columns$ColumnName[CSV_columns$File == filename])
  
  assign(paste0(tolower(filename), "_column_types"), 
         CSV_columns$RDataType[CSV_columns$File == filename])
  
} 

psh_residence <- 403

subsidized_residences <- c(419, 428, 431, 433, 434, 420, 
                           436, 437, 438, 439, 440,
                           psh_residence)

FY24_residence_types <- ResidenceUses %>%
  mutate(FY24_type_numeric = case_when(
    str_length(as.character(Location)) == 3 ~ floor(Location/100) * 100,
    !is.na(Location) ~ 0,
    TRUE ~ 400)) %>%
  select(Location, FY24_type_numeric, Location_FY22)

{
  # If FY22 2.02.07 = 0, then FY24 2.08.01= 0
  # If FY22 2.02.07 = 1, then FY24 2.08.01= 1
  HMISParticipation <- Project %>%
    mutate(HMISParticipationID = row_number(),
           HMISParticipationType = if_else(HMISParticipatingProject == 1, 1, 0)) %>%
    rename(HMISParticipationStatusStartDate = OperatingStartDate,
           HMISParticipationStatusEndDate = OperatingEndDate) %>%
    select(all_of(hmisparticipation_columns))
  }
{
  Project <- Project %>% 
    mutate(
      # If FY22 2.02.06C = 3, then set FY24 2.02.06 to 15
      ProjectType = case_when(
        TrackingMethod == 3 ~ 1, 
        ProjectType == 1 &
          TrackingMethod != 3 ~ 0,
        TRUE ~ ProjectType),
      # If FY22 2.02.06 = 13, then require FY24 2.02.06A
      RRHSubType = case_when(
        ProjectType == 13 ~ 2),
      # If 2.02.06 = 0,1,2,3,8,9,10,15 or 13 and Dependent A, = 2, then require 2.02.06D
      HousingType = ifelse(
        !(ProjectType == 13 & RRHSubType == 1), HousingType, NA)
    ) %>%
    select(all_of(project_columns))
  }

{
  Affiliation <- Affiliation %>%
    select(all_of(affiliation_columns))
}
{
  Client <- Client %>%
    mutate(HispanicLatinaeo = case_when(
      Ethnicity %in% c(1, 0) ~ Ethnicity,
      TRUE ~ 0
    ),
    MidEastNAfrican = 0, 
    AdditionalRaceEthnicity = NA, 
    CulturallySpecific = 0, 
    DifferentIdentity = 0, 
    DifferentIdentityText = NA) %>%
    rename(Woman = Female,
           Man = Male,
           NonBinary = NoSingleGender) %>%
    select(all_of(client_columns))
}
{
  # aligning project types to 4.20.2 (referred-to)
  referral_projects <- Project %>%
    select(ProjectID, ProjectType) %>%
    filter(ProjectType %in% c(0, 1, 2, 3, 9, 10, 13)) %>%
    mutate(ProjectType = case_when(
      ProjectID %in% Funder$ProjectID[Funder$Funder == 44] ~ 12,
      ProjectType %in% c(9, 10) ~ 15, 
      ProjectType %in% c(0, 1) ~ 10, 
      ProjectType == 2 ~ 11,
      ProjectType == 3 ~ 14,
      TRUE ~ ProjectType
    ))
  
  referral_locations <- Event %>%
    mutate(PersonalID = as.character(PersonalID)) %>%
    left_join(Enrollment %>%
                select(PersonalID, ProjectID, EntryDate) %>%
                inner_join(referral_projects, by = "ProjectID"), 
              by = "PersonalID") %>%
    filter(Event == ProjectType & 
             EntryDate >= EventDate) %>%
    group_by(EventID) %>%
    arrange(EntryDate) %>%
    slice(1L) %>%
    ungroup()
}
{
  ##  No mapping required (for vendors, at least)
  ##  All HMIS Project Types depending on design of Coordinated Entry System
  CEParticipation <- Project %>%
    mutate(CEParticipationID = row_number(),
           PreventionAssessment = 0,    
           CrisisAssessment = if_else(
             ProjectID %in% Enrollment$ProjectID[Enrollment$EnrollmentID 
                                                 %in% Assessment$EnrollmentID[Assessment$AssessmentLevel == 1]],
             1, 0),
           HousingAssessment = if_else(
             ProjectID %in% Enrollment$ProjectID[Enrollment$EnrollmentID 
                                                 %in% Assessment$EnrollmentID[Assessment$AssessmentLevel == 2]],
             1, 0),
           DirectServices = 0,
           AccessPoint = pmax(PreventionAssessment, CrisisAssessment,
                              HousingAssessment, DirectServices),
           ReceivesReferrals = if_else(
             ProjectID %in% referral_locations$ProjectID,
             1, 0)) %>%
    rename(CEParticipationStatusStartDate = OperatingStartDate,
           CEParticipationStatusEndDate = OperatingEndDate) %>%
    select(all_of(ceparticipation_columns))
}
{
  #If 4.12.01 = 435, then "Rental Subsidy Type" option list
  CurrentLivingSituation <- CurrentLivingSituation %>%
    left_join(FY24_residence_types,
              by = c("CurrentLivingSituation" = "Location_FY22")) %>%
    mutate(
      CurrentLivingSituation = CurrentLivingSituation + FY24_type_numeric,
      # need to confirm mapping of "Permanent housing for formerly homeless persons"
      CLSSubsidyType = case_when(
        CurrentLivingSituation == psh_residence ~ 440,
        CurrentLivingSituation %in% subsidized_residences ~ CurrentLivingSituation),
      CurrentLivingSituation = if_else(!is.na(CLSSubsidyType),
                                       435, CurrentLivingSituation)) %>%
    select(all_of(currentlivingsituation_columns))
}
{
  Disabilities <- Disabilities %>%
    rename(TcellCount= TCellCount,
           TcellSource = TCellSource) %>%
    select(all_of(disabilities_columns))
}
{
  EmploymentEducation <- EmploymentEducation %>%
    select(all_of(employmenteducation_columns))
}
{
  #If 3.917A.01 = 435, then "Rental Subsidy Type" option list
  Enrollment <- Enrollment %>%
    left_join(EnrollmentCoC %>%
                filter(DataCollectionStage == 1) %>%
                group_by(EnrollmentID) %>%
                arrange(InformationDate) %>%
                slice(1L) %>%
                ungroup() %>%
                select(EnrollmentID, CoCCode),
              by = "EnrollmentID") %>%
    left_join(FY24_residence_types,
              by = c("LivingSituation" = "Location_FY22")) %>%
    mutate(
      CoCCode = replace_na(CoCCode, "missing"),
      LivingSituation = LivingSituation + FY24_type_numeric,
      # need to confirm mapping of "Permanent housing for formerly homeless persons"
      RentalSubsidyType = case_when(
        LivingSituation == psh_residence ~ 440,
        LivingSituation %in% subsidized_residences ~ LivingSituation),
      LivingSituation = if_else(!is.na(RentalSubsidyType),
                                435, LivingSituation),
      TranslationNeeded = 0,
      PreferredLanguage = 171,
      PreferredLanguageDifferent = NA) %>%
    rename(EnrollmentCoC = CoCCode,
           HOHLeaseholder = HoHLeaseholder
           # only required if loading in from DataLab.R
           # , DateCreated = enroll_DateCreated
           ) %>%
    select(all_of(enrollment_columns))
}
{
  # Responses in Appendix A--Living Situation Option List reconfigured
  # If 3.12.01 = 435, then "Rental Subsidy Type" option list
  Exit <- Exit %>% 
    left_join(FY24_residence_types,
              by = c("Destination" = "Location_FY22")) %>%
    mutate(
      Destination = Destination + FY24_type_numeric,
      # need to confirm mapping of "Permanent housing for formerly homeless persons"
      DestinationSubsidyType = case_when(
        Destination == psh_residence ~ 440,
        Destination %in% subsidized_residences ~ Destination),
      Destination = if_else(!is.na(DestinationSubsidyType),
                            435, Destination)) %>%
    # only required if loading in from DataLab.R
    rename(
          # DateCreated = exit_DateCreated,
           #  only required when source database has different capitalization
           AskedOrForcedToExchangeForSexPastThreeMonths = AskedOrForcedToExchangeForSexPas,
           WorkplaceViolenceThreats = WorkPlaceViolenceThreats) %>%
    select(all_of(exit_columns))
}
{
  Export <- Export %>%
    mutate(ImplementationID = "0001") %>%
    select(all_of(export_columns))
}
{
  Funder <- Funder %>%
    select(all_of(funder_columns))
}
{
  HealthAndDV <- HealthAndDV %>%
    rename(DomesticViolenceSurvivor = DomesticViolenceVictim) %>%
    select(all_of(healthanddv_columns))
}
{
  IncomeBenefits <- IncomeBenefits %>%
    rename(VHAServicesHA = VAMedicalServices,
           NoVHAReasonHA = NoVAMedReason) %>%
    select(all_of(incomebenefits_columns))
}
{
  Inventory <- Inventory %>%
    select(all_of(inventory_columns))
}
{
  Organization <- Organization %>%
    select(all_of(organization_columns))
}
{
  ProjectCoC <- ProjectCoC %>%
    select(all_of(projectcoc_columns))
}
{
  Services <- Services %>%
    mutate(FAStartDate = NA,
           FAEndDate = NA) %>%
    select(all_of(services_columns))
}
{
  User <- User %>%
    select(all_of(user_columns))
}
{
  YouthEducationStatus <- YouthEducationStatus %>%
    select(all_of(youtheducationstatus_columns))
}
{
  Assessment <- Assessment %>%
    select(all_of(assessment_columns))
}
{
  AssessmentQuestions <- AssessmentQuestions %>%
    select(all_of(assessmentquestions_columns))
}
{
  AssessmentResults <- AssessmentResults %>%
    select(all_of(assessmentresults_columns))
}
{
  Event <- Event %>%
    select(all_of(event_columns))
}

# write it all out --------------------------------------------------------

df_names <- CSV_columns$File %>% unique()

for(df in df_names) {
  write.csv(
    get(df),
    file = paste0("output/", df, ".csv"),
    row.names = FALSE,
    quote = TRUE,
    eol = "\r\n",
    na = ""
  )
}


