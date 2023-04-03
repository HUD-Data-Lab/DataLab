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

# If FY22 2.02.07 = 0, then FY24 2.08.01= 0
# If FY22 2.02.07 = 1, then FY24 2.08.01= 1
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
      TrackingMethod == 3, 15, ProjectType
    ),
    #If FY22 2.02.06 = 13, then require FY24 2.02.06A
    RRHSubType = case_when(
      ProjectType == 13 ~ 2
    ),
    #If 2.02.06 = 1,2,3,8,9,10,15 or 13 and Dependent A, = 2, then require 2.02.06D
    HousingType = ifelse(
      !(ProjectType == 13 & RRHSubType == 1), NA, HousingType)
  ) %>%
  select(all_of(project_columns))

#If 2.02.06 = 1 or 15, then require FY24 2.07.06; Null unless Project.csv ProjectType = 1 or 15
####issue getting this to run without an error####
Inventory <- Inventory %>%
      mutate(
        ifelse(
        Project$ProjectType %in% c(1,15),Availability,NA)
      )


  

#If 3.12.01 = 435, then "Rental Subsidy Type" option list
Exit <- Exit %>% 
  mutate(
    SubsidyType= case_when(
      Destination %in% c(428,419,431,433,434,420,436,437,438,439,440) ~ Destination
               ),
      Destination = if_else(!is.na(SubsidyType),435,Destination)
             )

#If 3.917A.01 = 435, then "Rental Subsidy Type" option list
Enrollment <- mutate(Enrollment, SubsidyType = case_when(
  LivingSituation %in% c(428,419,431,433,434,420,436,437,438,439,440) ~ LivingSituation),
  LivingSituation = if_esle(!is.na(SubsidyType),435,LivingSituation)
  )

#If 4.12.01 = 435, then "Rental Subsidy Type" option list
CurrentLivingSituation <- CurrentLivingSituation %>%
  mutate(
    SubsidyType = case_when(
      CurrentLivingSituation %in% c(428,419,431,433,434,420,436,437,438,439,440) ~ CurrentLivingSituation
    ),
    CurrentLivingSituation = if_esle(!is.na(SubsidyType),435,CurrentLivingSituation)
  )


