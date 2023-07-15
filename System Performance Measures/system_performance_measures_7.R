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

# 7. System Performance Performance Measure 7: Successful Placement from Street Outreach and Successful Placement in or Retention of Permanent Housing ----

## Metric 7a.1 ----  

{
  df_7a1_clientDetail <- enrollment_data %>% 
    mutate(
      "m1_active.clients" = EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_start_date),
      "exclude_enrollment" = (ExitDate > report_end_date | is.na(ExitDate)) | Destination %in% c(206,329,24), #Remove enrollments with these qualifiers
      "leaver.stayer" = ifelse(is.na(ExitDate), "Stayer","Leaver" ),
      "Destination_category" = case_when(
        Destination %in% c(300:399) ~ "Temporary",
        Destination %in% c(400:499) ~ "Permanent")) %>% 
    filter(ProjectType ==4)
  
  df_7a1_sliced.detail <- df_7a1_clientDetail %>%
    filter(PersonalID %nin% df_7a1_clientDetail$PersonalID[(df_7a1_clientDetail$ExitDate > report_end_date | is.na(df_7a1_clientDetail$ExitDate)) | 
                                                             df_7a1_clientDetail$Destination %in% c(206,329,24)]) %>% 
    group_by(PersonalID) %>% 
    arrange(desc(EnrollmentID), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>% 
    slice(which.max(ExitDate)) #Keeps the most recent exit information. If the exit dates are the same it will keep first in DF
  
  # Metric 7a1 variable calculations
  
  
  Count_7a.1_universe <- df_7a1_sliced.detail %>% n_distinct(.$PersonalID)
  
  Count_7a.1_temp.inst.exts <- df_7a1_sliced.detail %>% 
    filter(Destination %in% c(100:115,117:206,208:399)) %>% # Don't include 207 and 116
    {n_distinct(.$PersonalID)}
  
  Count_7a.1_PH.exts <- df_7a1_sliced.detail %>% 
    filter(Destination %in% c(400:499)) %>% 
    {n_distinct(.$PersonalID)}
  
  Percent_7a.1_successful <- round(((Count_7a.1_temp.inst.exts + Count_7a.1_PH.exts) / Count_7a.1_universe )*100,2)
  
  Metric.7a1.table <- data.frame(
    "A" =c(" "," Universe: Persons who exit Street Outreach", 
           "Of persons above, those who exited to temporary & some institutional destinations", 
           "Of the persons above, those who exited to permanent housing destinations",
           "% Successful exits"),
    "B" = c("Previous FY",NA,NA,NA,NA),
    "C" = c("Current FY",Count_7a.1_universe,Count_7a.1_temp.inst.exts,Count_7a.1_PH.exts,Percent_7a.1_successful),
    "D" = c("% Difference",NA,NA,NA,NA))
  
  
  ## Metric 7b ----
  
  
  df_7b_baseline <- enrollment_data %>% 
    filter(ProjectType %in% c(0,1,2,3,8,9,10,13), #leaving SO out of this. Could be duplicates between 7a and 7b
           EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate >= report_start_date) #includes stayers for metric 7b.2 | added >= report_start_date to allow for people exiting on start date to be included in report
    ) %>% 
    group_by(HouseholdID) %>%
    mutate(HoH_HMID = suppressWarnings(min(case_when(
      RelationshipToHoH == 1 ~ MoveInDate), na.rm = TRUE)),
      MoveInDate = case_when(ExitDate < HoH_HMID ~ NA, 
                             EntryDate > HoH_HMID ~ EntryDate, 
                             TRUE ~ HoH_HMID ))
  
  ### 7b.1 Metric ----
  
  df_7b.1_client.detail <- df_7b_baseline %>% 
    mutate("spm.7b.leaver_qual" = ProjectType %in% c(3,9,10) & MoveInDate <= report_end_date,
           "leaver.stayer" = ifelse(is.na(ExitDate), "Stayer","Leaver"),
           "Destination_category" = case_when(
             Destination %in% c(300:399) ~ "Temporary", #check 200 series
             Destination %in% c(400:499) ~ "Permanent")) %>%
    filter(PersonalID %nin% df_7b_baseline$PersonalID[df_7b_baseline$ProjectType %in% c(3,9,10) 
                                                      & df_7b_baseline$MoveInDate <= report_end_date],
           PersonalID %nin% df_7b_baseline$PersonalID[df_7b_baseline$Destination %in% c(206,225,215,24)])
  
  df_7b.1_detail.slice <- df_7b.1_client.detail %>% 
    filter(PersonalID %nin% df_7b.1_client.detail$PersonalID[df_7b.1_client.detail$ExitDate > report_end_date | 
                                                               is.na(df_7b.1_client.detail$ExitDate)]) %>%  # Exclude everyone with a ineligible exit or still active. This should only leave people with a single enrollment that ends in the report period
    group_by(PersonalID) %>% 
    arrange(desc(EnrollmentID), .by_group = TRUE) %>% # sorts by most recently created enrollment date. This could be the spot for a tie-breaker.
    slice(which.max(ExitDate))
  
  Count_7b.1_universe <- n_distinct(df_7b.1_detail.slice$PersonalID)
  
  Count_7b.1_exits.PH <- df_7b.1_detail.slice %>% 
    filter(Destination %in% c(400:499)) %>% 
    n_distinct(.$PersonalID)
  
  Percent_7b.1_successful <- round((Count_7b.1_exits.PH / Count_7b.1_universe)*100,2)
  
  Metric.7b1.table <- data.frame(
    "A" =c(" ",
           "Universe: Persons in ES-EE, ES-NbN,SH,TH, and PH - RRH who exited, plus persons in other PH projects who exited without moving into housing", 
           "Of the persons above, those who exited to permanent housing destinations", 
           "% Successful exits"),
    "B" = c("Previous FY",NA,NA,NA),
    "C" = c("Current FY",Count_7b.1_universe,Count_7b.1_exits.PH,Percent_7b.1_successful),
    "D" = c("% Difference",NA,NA,NA))
  
  
  ### 7b.2 Metric ----
  
  df_7b.2.stayers.leavers <- df_7b_baseline %>% 
    mutate("spm.7b.2_leaver_qual" =  EntryDate <= report_end_date & ExitDate <= report_end_date,
           "spm.7b.2_stayer_qual"= EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_end_date),
           "Destination_category" = case_when(
             Destination %in% c(300:399) ~ "Temporary",
             Destination %in% c(400:499) ~ "Permanent")) %>% 
    filter(ProjectType %in% c(3,9,10) & (spm.7b.2_stayer_qual | spm.7b.2_leaver_qual)) %>% 
    group_by(PersonalID) %>% 
    arrange(!is.na(ExitDate), desc(ExitDate), .by_group = TRUE) %>%  #Arrange with NA at top and then descending from most recent exit
    slice_head(n=1) #Keep first row of each group
  
  # There are cases of ties for is.na(ExitDate). Example client 511607 has two active PSH enrollments (Projecttype == 3) with two different entry dates
  # Arrange is keeping the enrollment with the most recent entry date.
  
  df_7b.2_client_detail <- df_7b.2.stayers.leavers %>% 
    filter(
      PersonalID %nin% df_7b.2.stayers.leavers$PersonalID[df_7b.2.stayers.leavers$spm.7b.2_stayer_qual & (df_7b.2.stayers.leavers$MoveInDate > report_end_date | is.na(df_7b.2.stayers.leavers$MoveInDate))],
      PersonalID %nin% df_7b.2.stayers.leavers$PersonalID[df_7b.2.stayers.leavers$spm.7b.2_leaver_qual & is.na(df_7b.2.stayers.leavers$MoveInDate)],
      PersonalID %nin% df_7b.2.stayers.leavers$PersonalID[df_7b.2.stayers.leavers$Destination %in% c(206,225,215,24)]
    )
  
  
  Count_7b.2_universe <- n_distinct(df_7b.2_client_detail$PersonalID)
  
  Count_7b.2.PH_dest <- df_7b.2_client_detail %>% #this only has those with an exit date in report range
    filter(Destination %in% c(400:499)) %>% 
    n_distinct(.$PersonalID)
  
  Count_7b.2_stayers <- df_7b.2_client_detail %>% 
    filter(spm.7b.2_stayer_qual) %>% 
    n_distinct(.$PersonalID)
  
  Count_7b.2_styrs.exits <- Count_7b.2.PH_dest + Count_7b.2_stayers
  
  Percent_7b.2_successful <- round((Count_7b.2_styrs.exits/Count_7b.2_universe)*100,2)
  
  Metric.7b2.table <- data.frame(
    "A" =c(" ",
           "Universe: Persons in all PH projects except PH-RRH who exited after moving into housing, or who moved into housing and remained in the PH project",
           "Of persons above, those who remained in applicable PH projects and those who exited to permanent housing destinations",
           "% Successful exits/retention"),
    "B" = c("Previous FY",NA,NA,NA),
    "C" = c("Current FY",Count_7b.2_universe,Count_7b.2_styrs.exits,Percent_7b.2_successful),
    "D" = c("% Difference",NA,NA,NA))
  
  
  ###
  df_7b.2_stayers.leavers <- df_7b_baseline %>% 
    mutate("spm.7b.2_leaver_qual" =  EntryDate <= report_end_date & ExitDate <= report_end_date,
           "spm.7b.2_stayer_qual"= EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_end_date)) %>% 
    filter(ProjectType %in% c(3,9,10) & (spm.7b.2_stayer_qual | spm.7b.2_leaver_qual)) %>% 
    group_by(PersonalID) %>% 
    arrange(!is.na(ExitDate), desc(ExitDate), .by_group = TRUE) %>%  #Arrange with NA at top and then descending from most recent exit
    slice_head(n=1) #Keep first row of each group
  
  # There are cases of ties for is.na(ExitDate). Example client 511607 has two active PSH enrollments (Projecttype == 3) with two different entry dates
  # Arrange is keeping the enrollment with the most recent entry date.
  
  df_7b.2_stayer.leavers_cleaned <- df_7b.2_stayers.leavers %>% 
    filter(
      PersonalID %nin% df_7b.2_stayers.leavers$PersonalID[df_7b.2_stayers.leavers$spm.7b.2_stayer_qual & (df_7b.2_stayers.leavers$MoveInDate > report_end_date | is.na(df_7b.2_stayers.leavers$MoveInDate))],
      PersonalID %nin% df_7b.2_stayers.leavers$PersonalID[df_7b.2_stayers.leavers$spm.7b.2_leaver_qual & is.na(df_7b.2_stayers.leavers$MoveInDate)],
      Destination %nin% c(206,225,215,329,24))
  
  
  Count_7b.2_universe <- n_distinct(df_7b.2_stayer.leavers_cleaned$PersonalID)
  
  Count_7b.2_PH_dest <- df_7b.2_stayer.leavers_cleaned %>% #this only has those with an exit date in report range
    filter(Destination %in% 400:499) %>% 
    n_distinct(.$PersonalID)
  
  Count_7b.2_stayers <- df_7b.2_stayer.leavers_cleaned %>% 
    filter(spm.7b.2_stayer_qual) %>% 
    n_distinct(.$PersonalID)
  
  Count_7b.2_styrs.exits <- Count_7b.2_PH_dest + Count_7b.2_stayers
  
  Percent_7b.2_successful <- round((Count_7b.2_styrs.exits/Count_7b.2_universe)*100,2)
  
  Metric.7b2.table <- data.frame(
    "A" =c(" ",
           "Universe: Persons in all PH projects except PH-RRH who exited after moving into housing, or who moved into housing and remained in the PH project",
           "Of persons above, those who remained in applicable PH projects and those who exited to permanent housing destinations",
           "% Successful exits/retention"),
    "B" = c("Previous FY",NA,NA,NA),
    "C" = c("Current FY",Count_7b.2_universe,Count_7b.2_styrs.exits,Percent_7b.2_successful),
    "D" = c("% Difference",NA,NA,NA))
  
}

## SPM 7 Final Tables ----
Metric.7a1.table %>% 
  kbl(caption = "Metric 7a.1 - Change in exits to permanent housing destination") %>% 
  kable_styling(bootstrap_options = c("striped","hover"))

Metric.7b1.table %>% 
  kbl(caption = "Metric 7b.1 - Change in exits to permanent housing destinations") %>% 
  kable_styling(bootstrap_options = c("striped","hover"))

Metric.7b2.table %>% 
  kbl(caption = "Metric 7b.2 - Change in exit to or retention of permanent housing ") %>% 
  kable_styling(bootstrap_options = c("striped","hover"))


## Client detail CSV files ----
df_7a1_sliced.detail %>% 
  select(PersonalID,EntryDate,ExitDate,ProjectType,leaver.stayer,Destination,Destination_category) %>% 
  write_csv("7a1.Client.detail.csv")

df_7b.1_detail.slice %>% 
  select(PersonalID,EntryDate,ExitDate,ProjectType,leaver.stayer,Destination,Destination_category) %>% 
  write_csv("7b1.Client.detail.csv")

# df_7b.2_detail %>% 
#   select(PersonalID,EntryDate,ExitDate,ProjectType,Destination,Destination_category) %>% 
#   write_csv("7b2.Client.detail.csv")

# Draft section Don't Review ----

# # CHECK Compare universe client lists to check for duplicates
# spm.7a1_persons <- df_SPM.7a.1.exits[,2]
# spm7.b1_persons <- df_SPM.7b.1.exits[,2]
# 
# Duplicates <- rbind(spm.7a1_persons,spm7.b1_persons)
# 
# df_dupCount <- Duplicates %>% 
#   group_by(PersonalID) %>% 
#   summarise(n=n()) %>%
#   arrange(desc(n)) %>%
#   filter(n > 1)
# #107 duplicates in both universe lists. There should be no overlap between each universe df
# 
# df_dupCount
# df_dupCount_list <- df_dupCount$PersonalID
# 
# df_sppm_duplicates <- df_SPM.7.baseline %>% 
#   filter(PersonalID %in% df_dupCount_list) %>% 
#   select(EnrollmentID,PersonalID, EntryDate, ExitDate,ProjectType)
# 
# df_sppm_duplicates <- df_sppm_duplicates %>% 
#   mutate(SPM.7_in_report_range = ExitDate<report_end_date & ExitDate>report_start_date) %>% 
#   arrange(PersonalID,by=ExitDate)
# 
# DF_duplicates_test <- df_SPM.7.baseline %>% 
#   filter(PersonalID == 416498)


# New idea: Arrange and slice the baseline to keep the most recent exit for all projects types ( still trying to think through NbN)
# This would keep the most recent exit for each project type. We don't need total active for this measure
# In theory this would allow us to have distinct persons between each metrics
# baseline data - enrollment_data
# Baseline NbN - bed_nights_in_report_ee_format

df_SPM.7.baseline <- enrollment_data

df_SPM.7.M1.active <- df_SPM.7.baseline %>% 
  mutate(Destination_Names = case_when(
    Destination %in% c(101,116,118) ~ "Homeless.Sit",
    Destination %in% c(206,215,207,204,205,225) ~ "Institutional.Set",
    Destination %in% c(329,314,312,313,302,327,332) ~ "Temporary.Sit",
    Destination %in% c(426,411,421,410,435,422,423) ~ "Permanent.Sit",
    Destination %in% c(24,8,9,99,30,17) ~ "Other"),
    MoveinDate_qual = MoveInDate <= report_end_date) %>% #Qualifier variable for Movein date. If TRUE exclude
  filter(ProjectType %in% c(0,4,1,8,2,13),
         EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_start_date),
         is.na(MoveInDate) | MoveInDate > report_end_date)

df_SPM.7.exits <- df_SPM.7.M1.active %>% 
  mutate(No_exit = is.na(ExitDate) | ExitDate > report_end_date,
         No_exit_list = ifelse(No_exit,PersonalID,NA)) # This is to get a list of all personalIDs with an ineligible enrollment.

df_SPM.7_list <- unique(df_SPM.7.exits$No_exit_list) #Use the T/F from No_exit_list to create the list
df_SPM.7_list <- df_SPM.7_list[!is.na(df_SPM.7_list)] # Remove the NA

df_SPM.7.exits <- df_SPM.7.exits %>% 
  filter(PersonalID %nin% df_SPM.7_list, # Exclude everyone with a ineligible exit. This should only leave people with a single enrollment or enrollments that end in the report period
         Destination %nin% c(206,329,24)) %>% 
  group_by(PersonalID) %>% 
  slice(which.max(ExitDate))  # Why not slice_max()? // this keeps the latest exit for all duplicates. There should be no duplicates after this

### New Idea Counts

SPM.7a.1_counts <- df_SPM.7.exits %>% #creates a summary table with the counts for SPM.7a.1
  filter(ProjectType == 4) %>% 
  group_by(Destination_Names) %>% 
  summarise(n=n())

SPM.7b.1_counts <- df_SPM.7.exits %>% #creates a summary table with the counts for SPM.7b.1
  filter(ProjectType %in% c(0,1,8,2,13)) %>% 
  group_by(Destination_Names) %>% 
  summarise(n=n())

### New Idea Duplicate check

spm.7a1_persons <- df_SPM.7.exits %>%
  filter(ProjectType == 4) %>%
  select(PersonalID)

spm.7.b1_persons <- df_SPM.7.exits %>%
  filter(ProjectType %in% c(0,1,8,2,13)) %>%
  select(PersonalID)

Duplicates <- rbind(spm.7a1_persons,spm.7.b1_persons)

df_dupCount <- Duplicates %>% 
  group_by(PersonalID) %>% 
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  filter(n > 1)
# looks like there are no duplicates.

## 7b.2 - exit or retention of permanent housing


