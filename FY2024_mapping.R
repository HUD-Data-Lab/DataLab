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

new_client_columns <- c("PersonalID", "FirstName", "MiddleName", "LastName",
                        "NameSuffix", "NameDataQuality", "SSN", "SSNDataQuality",
                        "DOB", "DOBDataQuality", "AmIndAKNative", "Asian",
                        "BlackAfAmerican", "HispanicLatinaeo", "NativeHIPacific", 
                        "White", "RaceNone", "Woman", "Man", "NonBinary", 
                        "CulturallySpecific", "Transgender", "Questioning", 
                        "DifferentIdentity", "GenderNone", "VeteranStatus",
                        "YearEnteredService", "YearSeparated", "WorldWarII",
                        "KoreanWar", "VietnamWar", "DesertStorm", "AfghanistanOEF",
                        "IraqOIF", "IraqOND", "OtherTheater", "MilitaryBranch",
                        "DischargeStatus", "DateCreated", "DateUpdated",
                        "UserID", "DateDeleted", "ExportID")
Client <- Client %>%
  mutate(HispanicLatinaeo = case_when(
    Ethnicity %in% c(1, 0) ~ Ethnicity
  )) %>%
  rename(Woman = Female,
         Man = Male,
         NonBinary = NoSingleGender)
  select(all_of(new_client_columns))
  
# If FY22 2.02.06C = 0, then set FY24 2.02.06 to 15 
# If FY22 2.02.06C = 3, then set FY24 2.02.06 to 1
Project <- mutate(Project,ProjectType= case_when(
  ProjectType == 0 ~ 15, 
  ProjectType == 3 ~ 1,
  TRUE ~ ProjectType
  ),
  # If FY22 2.02.07 = 0, then FY24 2.08.01= 0
  # If FY22 2.02.07 = 1, then FY24 2.08.01= 1
  TargetPopulation= case_when(
    TargetPopulation == 0 ~ 0,
    TargetPopulation == 1 ~1,
    TRUE ~ TargetPopulation
  ),
  #If FY22 2.02.06 = 13, then require FY24 2.02.06A
    RRHSubtype = case_when(
      ProjectType == 13 ~ 2
  ),
  #If 2.02.06 = 1,2,3,8,9,10,15 or 13 and Dependent A, = 2, then require 2.02.06D
 ifelse(
    ProjectType ==13 & RRHSubtype == 1,NA,HousingType)
 )  


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


