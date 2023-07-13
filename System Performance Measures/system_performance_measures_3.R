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

# 3.2. System Performance Measure 3.2: Persons Experiencing Homelessness ----
## Create DF with Active client counts
# enrollment_data
# bed_nights_in_report_ee_format #use this to get a count of active persons

# NOT CONFIDENT in the coding of Method 2 Active Clients in the creation of "df_spm.3.2_base"

# df_spm.3.2_base <- enrollment_data %>% 
#   mutate(M2.ES.Nbn_enrlmnt_qual = 
#            ExitDate >= report_start_date,
#          M2.ES.Nbn_srvc_qual =
#            EntryDate <= report_end_date &
#            (is.na(ExitDate) | ExitDate > report_end_date) &
#            EntryDate >= report_start_date &
#            EntryDate <= report_end_date &
#            EntryDate >= report_end_date, #What are you?
#          M2.Active.Clients = M2.ES.Nbn_enrlmnt_qual | M2.ES.Nbn_srvc_qual,
#          M1.Active.Clients =
#            EntryDate <= report_end_date &
#            (ExitDate >= report_start_date | is.na(ExitDate)),
#          ProjectType_category = case_when(
#            ProjectType == 1 & M1.Active.Clients ~ "1.2 Emergency Shelter - entry/exit",
#            ProjectType == 0 & M2.Active.Clients ~ "1.1 Emergency shelter - night-by-night",
#            ProjectType == 8 & M1.Active.Clients ~ "8.0 Safe haven",
#            ProjectType == 2 & M1.Active.Clients ~ "2.0 Transitional housing")) %>% 
#   filter(ProjectType %in% c(0,1,8,2) & (M1.Active.Clients | M2.Active.Clients))

# Try out method 5.active_enrollments


df_spm.3.2_u <- active_enrollments %>% 
  filter(
    Method2 &
    ProjectType %in% c(0,1,2,8) &
      EntryDate <= report_end_date &
      (is.na(ExitDate) | ExitDate > report_start_date) #|
      # ((ProjectType == 1 &
      #     EnrollmentID %in% universe$EnrollmentID))
  )



# Count ES (Combine to single count)
# 
# SPM.3.2_ES.EE.Count<- df_spm.3.2_base %>% 
#   filter(ProjectType == 1) %>%
#   {n_distinct(.$PersonalID)}
# 
# SPM.3.2_ES.NbN.Count <- df_spm.3.2_base %>% 
#   filter(ProjectType == 0) %>% 
#   {n_distinct(.$PersonalID)}

Count_3.2_ES <- df_spm.3.2_u %>% 
  filter(ProjectType == 0 | ProjectType == 1) %>% 
  {n_distinct(.$PersonalID)}

# df.ES.client.detail <- df_spm.3.2_base %>% 
#   filter(ProjectType == 0 | ProjectType == 1) %>% 
#   select(ProjectType_category,PersonalID,EntryDate,ExitDate)

# Count Safe Haven
SPM.3.2_SH.Count <- df_spm.3.2_u %>% 
  filter(ProjectType == 8) %>% 
  {n_distinct(.$PersonalID)}

# df.SH.client.detail <- df_spm.3.2_base %>% 
#   filter(ProjectType == 8)

# Count Th
# Client 92109 is not in the dataset. Any of the datasets (client, enrollment, enrollment.data)
SPM.3.2_TH.Count <- df_spm.3.2_u %>% 
  filter(ProjectType == 2) %>% 
  {n_distinct(.$PersonalID)}

# df.TH.client.detail <- df_spm.3.2_base %>% 
#   filter(ProjectType == 2) %>% 
#   select(ProjectType_category,PersonalID,EntryDate,ExitDate)

# Count universe

SPM.3.2_universe_count <- n_distinct(df_spm.3.2_u$PersonalID)

## Metric 3.2 table ----
Metric.3.2.table <- data.frame(
  "Project Type" =c("Unduplicated Total sheltered persons","Emergency Shelter Total", "Safe Haven Total", "Transitional Housing Total"),
  "Previous.FY" = NA,
  "Current.FY" = c(SPM.3.2_universe_count,Count_3.2_ES,SPM.3.2_SH.Count,SPM.3.2_TH.Count),
  "Difference" = NA
)

## SPM 3.2 Final Table ----

Metric.3.2.table %>% 
  kbl(caption = "Metric 3.2 - Change in annual counts of persons experiencing homelessness in HMIS") %>% 
  kable_styling(bootstrap_options = c("striped","hover"))

## Client Detail CSV File ----

# df_spm.3.2_base %>% 
#   select(ProjectType_category,ProjectType,PersonalID,EntryDate,ExitDate,M1.Active.Clients,M2.Active.Clients) %>% 
#   write_csv("3.2.Client.detail.csv")

# df.ES.client.detail %>% 
#   select(PersonalID,EntryDate,ExitDate,ProjectType,Destination,ProjectType_category) %>% 
#   write_csv("3.2.Client.detail.ES.check.csv")
# 
# df.SH.client.detail %>% 
#   select(PersonalID,EntryDate,ExitDate,ProjectType,Destination,ProjectType_category) %>% 
#   write_csv("3.2.Client.detail.SH.check.csv")
# 
# df.TH.client.detail %>% 
#   write_csv("3.2.Client.detail.TH.check.csv")