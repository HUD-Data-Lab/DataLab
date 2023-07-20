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
    filter(ProjectType == 4) %>% 
    mutate(
      "m1_active.clients" = EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_start_date),
      "exclude_enrollment" = (ExitDate > report_end_date | is.na(ExitDate)) | Destination %in% c(206,329,24), 
      "leaver.stayer" = ifelse(is.na(ExitDate) | ExitDate>report_end_date , "Stayer","Leaver" ),
      "Destination_category" = case_when(
        Destination %in% c(300:399) ~ "Temporary",
        Destination %in% c(400:499) ~ "Permanent"))
  
  df_7a1_sliced.LatestExit <- df_7a1_clientDetail %>%
    filter(PersonalID %nin% df_7a1_clientDetail$PersonalID[df_7a1_clientDetail$ExitDate > report_end_date | is.na(df_7a1_clientDetail$ExitDate)]) %>% # removes all clients who are a stayer.
    group_by(PersonalID) %>% 
    arrange(desc(EnrollmentID), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>% 
    slice(which.max(ExitDate)) %>%  #Keeps the most recent exit information. If the exit dates are the same it will keep first in DF
    ungroup()
  
 df_7a1_sliced_exitdestinations<-  df_7a1_sliced.LatestExit %>% 
    filter(PersonalID %nin% df_7a1_sliced.LatestExit$PersonalID[df_7a1_sliced.LatestExit$Destination %in% c(206,329,24)])
  
  # Metric 7a1 variable calculations
  
  
  Count_7a.1_universe <- df_7a1_sliced_exitdestinations %>% n_distinct(.$PersonalID)
  
  Count_7a.1_temp.inst.exts <- df_7a1_sliced_exitdestinations %>% 
    filter(Destination %in% c(100:115,117:206,208:399)) %>% # Don't include 207 and 116
    {n_distinct(.$PersonalID)}
  
  Count_7a.1_PH.exts <- df_7a1_sliced_exitdestinations %>% 
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
    mutate(
      HoH_HMID = suppressWarnings(min(case_when(RelationshipToHoH == 1 ~ MoveInDate), na.rm = TRUE)),
      MoveInDate = case_when(ExitDate < HoH_HMID ~ NA, 
                             EntryDate > HoH_HMID ~ EntryDate, 
                             TRUE ~ HoH_HMID )) %>% # What does this do?
    ungroup()
  
  ### 7b.1 Metric ----
  
  df_7b.1_client.detail <- df_7b_baseline %>% 
    filter(
      PersonalID %nin% df_7b_baseline$PersonalID[df_7b_baseline$ExitDate > report_end_date | # Exclude everyone with a ineligible exit or still active. This should only leave people with a single enrollment that ends in the report period
                                                          is.na(df_7b_baseline$ExitDate)]) %>% 
    mutate("spm.7b.leaver_qual" = ProjectType %in% c(3,9,10) & MoveInDate <= report_end_date,
           "leaver.stayer" = ifelse(is.na(ExitDate), "Stayer","Leaver"),
           "Destination_category" = case_when(
             Destination %in% c(200:399) ~ "Temporary or Institutional", #Include both Temporary and Institutional destinations
             Destination %in% c(400:499) ~ "Permanent")) %>%
    group_by(PersonalID) %>% 
    arrange(desc(EnrollmentID), .by_group = TRUE) %>% # sorts by most recently created enrollment date. This could be the spot for a tie-breaker.
    slice(which.max(ExitDate)) %>% 
    ungroup()
    
  df_7b.1_detail.slice <- df_7b.1_client.detail %>% 
    filter(
      PersonalID %nin% df_7b.1_client.detail$PersonalID[df_7b.1_client.detail$ProjectType %in% c(3,9,10) 
                                                 & df_7b.1_client.detail$MoveInDate <= report_end_date], #removes clients whose most recent exit was 3,9,10 and moveindate <= report end date
      PersonalID %nin% df_7b.1_client.detail$PersonalID[df_7b.1_client.detail$Destination %in% c(206,225,215,24)]) #removes clients whose most recent exit destination was marked as an X in appendix
   

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
  
  # df_7b.2.stayers.leavers <- df_7b_baseline %>% 
  #   mutate("spm.7b.2_leaver_qual" =  EntryDate <= report_end_date & ExitDate <= report_end_date,
  #          "spm.7b.2_stayer_qual"= EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_end_date),
  #          "Destination_category" = case_when(
  #            Destination %in% c(200:399) ~ "Temporary or Institutional",
  #            Destination %in% c(400:499) ~ "Permanent")) %>% 
  #   filter(ProjectType %in% c(3,9,10) & (spm.7b.2_stayer_qual | spm.7b.2_leaver_qual)) %>% 
  #   group_by(PersonalID) %>% 
  #   arrange(!is.na(ExitDate), desc(ExitDate), .by_group = TRUE) %>%  #Arrange with NA at top and then descending from most recent exit
  #   slice_head(n=1) #Keep first row of each group. This gives us the latest stay (no exit is considered the most recent stay)

#Edits based on Zach's review
  df_7b.2.stayers.leavers <- df_7b_baseline %>% 
    filter(ProjectType %in% c(3,9,10))
  
  df_7b.2_StayerLatestStay_u <- df_7b.2.stayers.leavers %>% 
    filter(PersonalID %in% df_7b.2.stayers.leavers$PersonalID[df_7b.2.stayers.leavers$ExitDate > report_end_date | is.na(df_7b.2.stayers.leavers$ExitDate)]) %>% # keeps all clients who are a stayer.)
    group_by(PersonalID) %>% 
    arrange(!is.na(ExitDate), desc(EnrollmentID), .by_group = TRUE) %>%  #Arrange with NA at top and then descending from most recently created enrollment ID
    slice(which.max(EntryDate))# %>% #keeps the most recent entry date
  
  df_7b.2_leaverLatestStay_u <- df_7b.2.stayers.leavers %>% 
    filter(PersonalID %nin% df_7b.2.stayers.leavers$PersonalID[df_7b.2.stayers.leavers$ExitDate > report_end_date | is.na(df_7b.2.stayers.leavers$ExitDate)]) %>% # removes all clients who are a stayer.
    group_by(PersonalID) %>% 
    arrange(desc(EnrollmentID), .by_group = TRUE) %>% # sorts by most recently created enrollment date. This could be the spot for a tie-breaker.
    slice(which.max(ExitDate)) %>% #keeps the most recent exit enrollment
    ungroup()
  
  df_7b.2_stayer_u <- df_7b.2_StayerLatestStay_u %>% 
    filter(PersonalID %nin% df_7b.2_StayerLatestStay_u$PersonalID[df_7b.2_StayerLatestStay_u$MoveInDate > report_end_date | is.na(df_7b.2_StayerLatestStay_u$MoveInDate)])
  
  df_7b.2_leaver_u <- df_7b.2_leaverLatestStay_u %>%
    filter(PersonalID %nin% df_7b.2_leaverLatestStay_u$PersonalID[is.na(df_7b.2_leaverLatestStay_u$MoveInDate)], #remove clients with no move in date
           PersonalID %nin% df_7b.2_leaverLatestStay_u$PersonalID[df_7b.2_leaverLatestStay_u$Destination %in% c(206,225,215,24)]) #remove clients with ineligible exit
  
  df_7b.2_client_detail <- df_7b.2_stayer_u %>% 
    rbind(df_7b.2_leaver_u) %>% 
    mutate("spm.7b.2_leaver_qual" =  EntryDate <= report_end_date & ExitDate <= report_end_date,
           "spm.7b.2_stayer_qual"= EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_end_date),
           "Destination_category" = case_when(
             Destination %in% c(200:399) ~ "Temporary or Institutional",
             Destination %in% c(400:499) ~ "Permanent"))
    
  # 
  # df_7b.2_client_detail <- df_7b.2.stayers.leavers %>% 
  #   filter(
  #     PersonalID %nin% df_7b.2.stayers.leavers$PersonalID[df_7b.2.stayers.leavers$spm.7b.2_stayer_qual & (df_7b.2.stayers.leavers$MoveInDate > report_end_date | is.na(df_7b.2.stayers.leavers$MoveInDate))], #Exclude client based on move-in date
  #     PersonalID %nin% df_7b.2.stayers.leavers$PersonalID[df_7b.2.stayers.leavers$spm.7b.2_leaver_qual & is.na(df_7b.2.stayers.leavers$MoveInDate)],
  #     PersonalID %nin% df_7b.2.stayers.leavers$PersonalID[df_7b.2.stayers.leavers$Destination %in% c(206,225,215,24)]
  #   )
  
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
  # df_7b.2_stayers.leavers <- df_7b_baseline %>% 
  #   mutate("spm.7b.2_leaver_qual" =  EntryDate <= report_end_date & ExitDate <= report_end_date,
  #          "spm.7b.2_stayer_qual"= EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_end_date)) %>% 
  #   filter(ProjectType %in% c(3,9,10) & (spm.7b.2_stayer_qual | spm.7b.2_leaver_qual)) %>% 
  #   group_by(PersonalID) %>% 
  #   arrange(!is.na(ExitDate), desc(ExitDate), .by_group = TRUE) %>%  #Arrange with NA at top and then descending from most recent exit
  #   slice_head(n=1) #Keep first row of each group
  # 
  # # There are cases of ties for is.na(ExitDate). Example client 511607 has two active PSH enrollments (Projecttype == 3) with two different entry dates
  # # Arrange is keeping the enrollment with the most recent entry date.
  # 
  # df_7b.2_stayer.leavers_cleaned <- df_7b.2_stayers.leavers %>% 
  #   filter(
  #     PersonalID %nin% df_7b.2_stayers.leavers$PersonalID[df_7b.2_stayers.leavers$spm.7b.2_stayer_qual & (df_7b.2_stayers.leavers$MoveInDate > report_end_date | is.na(df_7b.2_stayers.leavers$MoveInDate))],
  #     PersonalID %nin% df_7b.2_stayers.leavers$PersonalID[df_7b.2_stayers.leavers$spm.7b.2_leaver_qual & is.na(df_7b.2_stayers.leavers$MoveInDate)],
  #     Destination %nin% c(206,225,215,329,24))
  # 
  # 
  # Count_7b.2_universe <- n_distinct(df_7b.2_stayer.leavers_cleaned$PersonalID)
  # 
  # Count_7b.2_PH_dest <- df_7b.2_stayer.leavers_cleaned %>% #this only has those with an exit date in report range
  #   filter(Destination %in% 400:499) %>% 
  #   n_distinct(.$PersonalID)
  # 
  # Count_7b.2_stayers <- df_7b.2_stayer.leavers_cleaned %>% 
  #   filter(spm.7b.2_stayer_qual) %>% 
  #   n_distinct(.$PersonalID)
  # 
  # Count_7b.2_styrs.exits <- Count_7b.2_PH_dest + Count_7b.2_stayers
  # 
  # Percent_7b.2_successful <- round((Count_7b.2_styrs.exits/Count_7b.2_universe)*100,2)
  # 
  # Metric.7b2.table <- data.frame(
  #   "A" =c(" ",
  #          "Universe: Persons in all PH projects except PH-RRH who exited after moving into housing, or who moved into housing and remained in the PH project",
  #          "Of persons above, those who remained in applicable PH projects and those who exited to permanent housing destinations",
  #          "% Successful exits/retention"),
  #   "B" = c("Previous FY",NA,NA,NA),
  #   "C" = c("Current FY",Count_7b.2_universe,Count_7b.2_styrs.exits,Percent_7b.2_successful),
  #   "D" = c("% Difference",NA,NA,NA))
  
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

df_7b.2_client_detail %>% 
  select(PersonalID,EntryDate,ExitDate,ProjectType,Destination,Destination_category) %>%
  write_csv("7b2.Client.detail.csv")