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

# items_to_keep <- c(items_to_keep,
#                    do.call(paste0, expand.grid("spm_1", c("a", "b"), c("1", "2"), c("_dq", ""))))

# 3.2. System Performance Measure 3.2: Persons Experiencing Homelessness ----

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


# df_spm.3.2_u <- active_enrollments %>% 
#   filter(
#     Method2 &
#     ProjectType %in% c(0,1,2,8) &
#       EntryDate <= report_end_date &
#       (is.na(ExitDate) | ExitDate > report_start_date) #|
#       # ((ProjectType == 1 &
#       #     EnrollmentID %in% universe$EnrollmentID))
#   )


df_spm.3.2_u <- active_enrollments %>% 
  filter(
    (Method2 & ProjectType %in% c(1)) | (Method1 & ProjectType %in% c(0,2,8))
    #   EntryDate <= report_end_date &
    #   (is.na(ExitDate) | ExitDate > report_start_date) #|
    # # ((ProjectType == 1 &
    # #     EnrollmentID %in% universe$EnrollmentID))
  )


# Count ES (Combine to single count)

Count_3.2_ES <- df_spm.3.2_u %>% 
  filter(ProjectType == 0 | ProjectType == 1) %>% 
  {n_distinct(.$PersonalID)}

# Count Safe Haven
SPM.3.2_SH.Count <- df_spm.3.2_u %>% 
  filter(ProjectType == 8) %>% 
  {n_distinct(.$PersonalID)}

# Count Th
# Client 92109 is not in the dataset. Any of the datasets (client, enrollment, enrollment.data)
SPM.3.2_TH.Count <- df_spm.3.2_u %>% 
  filter(ProjectType == 2) %>% 
  {n_distinct(.$PersonalID)}

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

df_spm.3.2_u %>%
  select(PersonalID,ProjectType,EntryDate,MoveInDate,ExitDate,Method1,Method2,Method5) %>%
  write_csv("3.2.Client.detail.csv")

rm(list = ls()[ls() %nin% items_to_keep]) 
