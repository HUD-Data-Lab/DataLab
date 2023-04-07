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

source("DataLab.R")

for (filename in unique(CSV_columns$File)) {
  
  assign(paste0(tolower(filename), "_columns"), 
         CSV_columns$ColumnName[CSV_columns$File == filename])
  
  assign(paste0(tolower(filename), "_column_types"), 
         CSV_columns$RDataType[CSV_columns$File == filename])
  
} 

new_Client <- Client %>%
  mutate(HispanicLatinaeo = case_when(
    Ethnicity %in% c(1, 0) ~ Ethnicity
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
  

# If FY22 2.02.07 = 0, then FY24 2.08.01= 0
# If FY22 2.02.07 = 1, then FY24 2.08.01= 1
new_HMISParticipation <- Project %>%
  mutate(HMISParticipationID = row_number(),
         HMISParticipationType = if_else(HMISParticipatingProject == 1, 1, 0)) %>%
  rename(HMISParticipationStatusStartDate = OperatingStartDate,
         HMISParticipationStatusEndDate = OperatingEndDate) %>%
  select(all_of(hmisparticipation_columns))
  
referral_projects <- Project %>%
  select(ProjectID, ProjectType) %>%
  filter(ProjectType %in% c(1, 2, 3, 9, 10, 13)) %>%
  mutate(ProjectType = case_when(
    ProjectID %in% Funder$ProjectID[Funder$Funder == 44] ~ 12,
    ProjectType == 1 ~ 10,
    ProjectType == 2 ~ 11,
    ProjectType == 3 ~ 14,
    ProjectType %in% c(9, 10) ~ 15,
    TRUE ~ ProjectType
  )) 


referral_locations <- Event %>%
  left_join(Enrollment %>%
              select(PersonalID, ProjectID, EntryDate) %>%
              inner_join(referral_projects, by = "ProjectID"), 
            by = "PersonalID",
            multiple = "all") %>%
  filter(Event == ProjectType & 
           EntryDate >= EventDate) %>%
  group_by(EventID) %>%
  arrange(EntryDate) %>%
  slice(1L) %>%
  ungroup()

##  No mapping required (for vendors, at least)
##  All HMIS Project Types depending on design of Coordinated Entry System
new_CEParticipation <- Project %>%
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


new_Project <- Project %>% 
  mutate(
    # If FY22 2.02.06C = 3, then set FY24 2.02.06 to 15
    ProjectType = if_else(
      TrackingMethod == 3, 15, ProjectType),
    # If FY22 2.02.06 = 13, then require FY24 2.02.06A
    RRHSubType = case_when(
      ProjectType == 13 ~ 2),
    # If 2.02.06 = 1,2,3,8,9,10,15 or 13 and Dependent A, = 2, then require 2.02.06D
    HousingType = ifelse(
      !(ProjectType == 13 & RRHSubType == 1), HousingType, NA)
  ) %>%
  select(all_of(project_columns))


psh_residence <- 403

subsidized_residences <- c(419, 428, 431, 433, 434, 420, 
                           436, 437, 438, 439, 440,
                           psh_residence)

FY24_residence_types <- ResidenceUses %>%
  mutate(FY24_type_numeric = case_when(
    FY24_type == "Homeless" ~ 100,
    FY24_type == "Institutional" ~ 200,
    FY24_type == "Temporary" ~ 300,
    FY24_type %in% c("Permanent", "Subsidized") ~ 400,
    FY24_type == "Other" ~ 0)) %>%
  select(Location, FY24_type_numeric)

# Responses in Appendix A--Living Situation Option List reconfigured
# If 3.12.01 = 435, then "Rental Subsidy Type" option list
new_Exit <- Exit %>% 
  left_join(FY24_residence_types,
            by = c("Destination" = "Location")) %>%
  mutate(
    Destination = Destination + FY24_type_numeric,
    # need to confirm mapping of "Permanent housing for formerly homeless persons"
    DestinationSubsidyType = case_when(
      Destination == psh_residence ~ 440,
      Destination %in% subsidized_residences ~ Destination),
    Destination = if_else(!is.na(DestinationSubsidyType),
                            435, Destination)) %>%
  # only required if loading in from DataLab.R
  rename(DateCreated = exit_DateCreated,
         #  only required when source database has different capitalization
         WorkplaceViolenceThreats = WorkPlaceViolenceThreats,
         WorkplacePromiseDifference = WorkPlacePromiseDifference) %>%
  select(all_of(exit_columns))

#If 3.917A.01 = 435, then "Rental Subsidy Type" option list
new_Enrollment <- Enrollment %>%
  left_join(EnrollmentCoC %>%
              filter(DataCollectionStage == 1) %>%
              group_by(EnrollmentID) %>%
              arrange(InformationDate) %>%
              slice(1L) %>%
              ungroup() %>%
              select(EnrollmentID, CoCCode),
            by = "EnrollmentID") %>%
  left_join(FY24_residence_types,
            by = c("LivingSituation" = "Location")) %>%
  mutate(
    LivingSituation = LivingSituation + FY24_type_numeric,
    # need to confirm mapping of "Permanent housing for formerly homeless persons"
    LivingSituationSubsidyType = case_when(
      LivingSituation == psh_residence ~ 440,
      LivingSituation %in% subsidized_residences ~ LivingSituation),
    LivingSituation = if_else(!is.na(LivingSituationSubsidyType),
                          435, LivingSituation),
    TranslationNeeded = 0,
    PreferredLanguage = 171,
    PreferredLanguageDifferent = NA) %>%
  rename(EnrollmentCoC = CoCCode,
         # only required if loading in from DataLab.R
         DateCreated = enroll_DateCreated) %>%
  select(all_of(enrollment_columns))
  

#If 4.12.01 = 435, then "Rental Subsidy Type" option list
new_CurrentLivingSituation <- CurrentLivingSituation %>%
  left_join(FY24_residence_types,
            by = c("CurrentLivingSituation" = "Location")) %>%
  mutate(
    CurrentLivingSituation = CurrentLivingSituation + FY24_type_numeric,
    # need to confirm mapping of "Permanent housing for formerly homeless persons"
    CLSSubsidyType = case_when(
      CurrentLivingSituation == psh_residence ~ 440,
      CurrentLivingSituation %in% subsidized_residences ~ CurrentLivingSituation),
    CurrentLivingSituation = if_else(!is.na(CLSSubsidyType),
                              435, CurrentLivingSituation)) %>%
  select(all_of(currentlivingsituation_columns))

new_Export <- Export %>%
  mutate(ImplementationID = "0001") %>%
  select(all_of(export_columns))

new_Organization <- Organization %>%
  select(all_of(organization_columns))

new_Organization <- Organization %>%
  select(all_of(organization_columns))

new_Organization <- Organization %>%
  select(all_of(organization_columns))

##################
## CE Activity  ##
##################

new_CEActivity_AB <- Assessment %>%
  filter(year(AssessmentDate) == 2022) %>%
  mutate(PreventionScreening = 0,
         PreventionOutcome = NA,
         PreventionLocation = NA,
         ShelterScreening = if_else(
           # If FY22 4.19.04 = 1, then FY24 4.21.04 = 1
           AssessmentLevel == 1, 1, 0),
         ShelterOutcome = case_when(
           # If FY22 4.19.04 = 1 AND 4.19.07 = 1, then FY24 4.21.04C = 6
           AssessmentLevel == 1 &
             PrioritizationStatus == 1 ~ 6),
         ShelterLocation = if_else(
           ShelterOutcome %in% c(1, 3), 99, NA),
         HousingScreening = if_else(
           # If FY22 4.19.04 = 2, then FY24 4.21.05 = 1
           AssessmentLevel == 2, 1, 0),
         HousingOutcome = case_when(
           # If FY22 4.19.04 = 2 AND 4.19.07 = 1, then FY24 4.21.05E = 9
           AssessmentLevel == 2 &
             PrioritizationStatus == 1 ~ 9),
         HousingLocation = if_else(
           HousingOutcome %in% 1:7, 99, NA),
         ServiceProvided = 0,
         ServiceOutcome = NA,
         ReferralResult = NA,
         ReferralResultDate = NA
         ) %>%
  rename(ActivityID = AssessmentID,
         ActivityDate = AssessmentDate,
         # If FY22 4.19.03 = 1, then FY24 4.21.02 = 1
         # If FY22 4.19.03 = 2, then FY24 4.21.02 = 2
         # If FY22 4.19.03 = 3, then FY24 4.21.02 = 3
         ContactType = AssessmentType) %>%
  # filter(!is.na(ContactType)) %>%
  select(all_of(ceactivity_columns))


new_CEActivity_EB <- Event %>%
  filter(year(EventDate) == 2022) %>%
  left_join(referral_locations %>%
              select(EventID, ProjectID),
            by = "EventID") %>%
  mutate(
    LocationCrisisOrPHHousing = if_else(
      is.na(LocationCrisisOrPHHousing), ProjectID, LocationCrisisOrPHHousing),
    ContactType = NA,
    PreventionScreening = if_else(Event %in% c(1, 16), 
                                  1, 0),
    PreventionOutcome = case_when(
      Event == 1 ~ 1,
      Event == 16 ~ 4
    ),
    PreventionLocation = case_when(
      Event == 1 ~ LocationCrisisOrPHHousing),
    ShelterScreening = if_else(Event %in% c(2, 10), 
                               1, 0),
    ShelterOutcome = case_when(
      Event == 2 ~ 1,
      Event == 10 ~ 3
    ),
    ShelterLocation = case_when(
      Event %in% c(2, 10) ~ LocationCrisisOrPHHousing),
    HousingScreening = if_else(Event %in% c(8, 9, 11:15, 17, 18), 
                               1, 0),
    HousingOutcome = case_when(
      Event == 8 ~ 12,
      Event == 9 ~ 11,
      Event == 11 ~ 2,
      Event == 12 ~ 3,
      Event == 13 ~ 4,
      Event == 14 ~ 5,
      Event == 15 ~ 6,
      Event == 17 ~ 7,
      Event == 18 ~ 8
    ),
    HousingLocation = case_when(
      Event %in% c(11, 12, 13, 14, 15, 17) ~ LocationCrisisOrPHHousing),
    ServiceProvided = if_else(Event %in% c(3, 4), 
                              1, 0),
    ServiceOutcome = case_when(
      Event %in% c(3, 4) ~ Event
    ),
    ReferralResult = case_when(
      Event %in% c(1, 2, 10:15, 17) ~ ReferralResult
    ),
    ReferralResultDate = case_when(
      Event %in% c(1, 2, 10:15, 17) ~ ResultDate
    )
  ) %>%
  rename(ActivityID = EventID,
         ActivityDate = EventDate) %>%
  select(all_of(ceactivity_columns))

combined_CEActivity <- new_CEActivity_AB %>%
  select(-ActivityID) %>%
  full_join(new_CEActivity_EB %>%
              `colnames<-`(c(paste0("EB_", colnames(new_CEActivity_EB)))) %>%
              select(-EB_ActivityID),
            by = c("EnrollmentID" = "EB_EnrollmentID", 
                   "PersonalID" = "EB_PersonalID",
                   "ActivityDate" = "EB_ActivityDate"),
            multiple = "all") %>%
  mutate(c_PreventionScreening = if_else(PreventionScreening == 1 |
                                           EB_PreventionScreening == 1, 
                                         1, 0),
         c_PreventionOutcome = EB_PreventionOutcome,
         c_PreventionLocation = EB_PreventionLocation,
         c_ShelterScreening = if_else(ShelterScreening == 1 |
                                        EB_ShelterScreening == 1, 
                                      1, 0),
         c_ShelterOutcome = if_else(is.na(EB_ShelterOutcome), ShelterOutcome, EB_ShelterOutcome),
         c_ShelterLocation = if_else(is.na(EB_ShelterLocation), ShelterLocation, as.numeric(EB_ShelterLocation)),
         c_HousingScreening = if_else(HousingScreening == 1 |
                                        EB_HousingScreening == 1, 
                                      1, 0),
         c_HousingOutcome = if_else(is.na(EB_HousingOutcome), HousingOutcome, EB_HousingOutcome),
         c_HousingLocation = if_else(is.na(EB_HousingLocation), HousingLocation, as.numeric(EB_HousingLocation)),
         c_ServiceProvided = EB_ServiceProvided,
         c_ServiceOutcome = EB_ServiceOutcome,
         c_ReferralResult = EB_ReferralResult,
         c_ReferralResultDate = EB_ReferralResultDate
  )

ceactivity_columns

possible_outcomes <- combined_CEActivity %>%
  select(c(all_of(ceactivity_columns[6:18]),
           all_of(paste0("EB_", ceactivity_columns[6:18])),
           all_of(paste0("c_", ceactivity_columns[6:18])))) %>%
  distinct()

write_csv(possible_outcomes, "current_mapping.csv")
