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
  

