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

items_to_keep <- c(items_to_keep,
                   do.call(paste0, expand.grid(c("spm_7a1", "spm_7b1", "spm_7b2"), c("_dq", ""))))

# 7. System Performance Performance Measure 7: Successful Placement from Street Outreach and Successful Placement in or Retention of Permanent Housing ----

## Metric 7a.1 ----  
{
  df_7a1_clientDetail <- active_enrollments %>%
    filter(
      Method1 == TRUE & 
        ProjectType == 4) %>% 
    mutate(
      "leaver.stayer" = ifelse(is.na(ExitDate) | ExitDate > report_end_date , 
                               "Stayer", "Leaver"),
      "Destination_category" = case_when(
        Destination %in% c(100:399) &
          Destination %nin% c(116, 207) ~ "Temporary or Institutional",
        Destination %in% c(400:499) ~ "Permanent"))
  
  spm_7a1_dq <- df_7a1_clientDetail %>%
    filter(PersonalID %nin% df_7a1_clientDetail$PersonalID[df_7a1_clientDetail$leaver.stayer == "Stayer"]) %>% # removes all clients who are a stayer.
    group_by(PersonalID) %>%
    arrange(desc(ExitDate)) %>%
    slice(1L) %>%  #Keeps the most recent exit information. If the exit dates are the same it will keep first in DF
    ungroup()
  
  spm_7a1_exits <- spm_7a1_dq %>% 
    filter(Destination %nin% c(206, 329, 24)) %>% 
    select(PersonalID, EntryDate, ExitDate, ProjectType, leaver.stayer,
          Destination, Destination_category)
  
  # Metric 7a1 variable calculations
  
  Count_7a.1_temp.inst.exts <- spm_7a1_exits %>% 
    filter(
      Destination %in% c(100:399) &
        Destination %nin% c(116, 207)) %>% # Don't include 207 and 116
    {n_distinct(.$PersonalID)}
  
  Count_7a.1_PH.exts <- spm_7a1_exits %>% 
    filter(Destination %in% c(400:499)) %>% 
    {n_distinct(.$PersonalID)}
  
  Percent_7a.1_successful <- round(
    ((Count_7a.1_temp.inst.exts + Count_7a.1_PH.exts) / 
       nrow(spm_7a1_exits)) * 100, 2)
  
  spm_7a1 <- data.frame(
    "row_names" =c(
      "Universe: Persons who exit Street Outreach", 
      "Of persons above, those who exited to temporary & some institutional destinations", 
      "Of the persons above, those who exited to permanent housing destinations",
      "% Successful exits"),
    "Previous.FY"= c(NA,NA,NA,NA),
    "Current.FY"= as.character(c(nrow(spm_7a1_exits),
                                 Count_7a.1_temp.inst.exts,
                                 Count_7a.1_PH.exts,
                                 Percent_7a.1_successful)),
    "Percent.Difference"= c(NA,NA,NA,NA))
  
  
  ## Metric 7b ----
  
  df_7b_baseline <- enrollment_data %>% 
    filter(
      ProjectType %in% c(0, 1, 2, 3, 8, 9, 10, 13), #leaving SO out of this. Could be duplicates between 7a and 7b
      EntryDate <= report_end_date & 
        (is.na(ExitDate) | ExitDate >= report_start_date)) %>%  #includes stayers for metric 7b.2 | added >= report_start_date to allow for people exiting on start date to be included in report
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
    arrange(desc(ExitDate), EnrollmentID) %>% # sorts by most recently created enrollment date. This could be the spot for a tie-breaker.
    slice(1L) %>% 
    ungroup()
  
  spm_7b1_dq <- df_7b.1_client.detail %>% 
    select(PersonalID,EntryDate,ExitDate,ProjectType,leaver.stayer,Destination,Destination_category) %>% 
    filter(
      PersonalID %nin% df_7b.1_client.detail$PersonalID[df_7b.1_client.detail$ProjectType %in% c(3,9,10) 
                                                        & df_7b.1_client.detail$MoveInDate <= report_end_date], #removes clients whose most recent exit was 3,9,10 and moveindate <= report end date
      PersonalID %nin% df_7b.1_client.detail$PersonalID[df_7b.1_client.detail$Destination %in% c(206,225,215,24)]) #removes clients whose most recent exit destination was marked as an X in appendix
  
  
  Count_7b.1_universe <- n_distinct(spm_7b1_dq$PersonalID)
  
  Count_7b.1_exits.PH <- spm_7b1_dq %>% 
    filter(Destination %in% c(400:499)) %>% 
    n_distinct(.$PersonalID)
  
  Percent_7b.1_successful <- round((Count_7b.1_exits.PH / Count_7b.1_universe)*100,2)
  
  spm_7b1 <- data.frame(
    "row_names" =c("Universe: Persons in ES-EE, ES-NbN,SH,TH, and PH - RRH who exited, plus persons in other PH projects who exited without moving into housing", 
                   "Of the persons above, those who exited to permanent housing destinations", 
                   "% Successful exits"),
    "Previous.FY"= c(NA,NA,NA),
    "Current.FY"= as.character(c(Count_7b.1_universe,Count_7b.1_exits.PH,Percent_7b.1_successful)),
    "Percent.Difference"= c(NA,NA,NA))
  
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
    arrange(desc(ExitDate), EnrollmentID) %>% 
    slice(1L) %>%
    ungroup()
  
  df_7b.2_stayer_u <- df_7b.2_StayerLatestStay_u %>% 
    filter(PersonalID %nin% df_7b.2_StayerLatestStay_u$PersonalID[df_7b.2_StayerLatestStay_u$MoveInDate > report_end_date | is.na(df_7b.2_StayerLatestStay_u$MoveInDate)])
  
  df_7b.2_leaver_u <- df_7b.2_leaverLatestStay_u %>%
    filter(PersonalID %nin% df_7b.2_leaverLatestStay_u$PersonalID[is.na(df_7b.2_leaverLatestStay_u$MoveInDate)], #remove clients with no move in date
           PersonalID %nin% df_7b.2_leaverLatestStay_u$PersonalID[df_7b.2_leaverLatestStay_u$Destination %in% c(206,225,215,24)]) #remove clients with ineligible exit
  
  spm_7b2_dq <- df_7b.2_stayer_u %>% 
    select(PersonalID,EntryDate,ExitDate,ProjectType,Destination) %>%
    rbind(df_7b.2_leaver_u) %>% 
    mutate("spm.7b.2_leaver_qual" =  EntryDate <= report_end_date & ExitDate <= report_end_date,
           "spm.7b.2_stayer_qual"= EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_end_date),
           "Destination_category" = case_when(
             Destination %in% c(200:399) ~ "Temporary or Institutional",
             Destination %in% c(400:499) ~ "Permanent"))
  
  Count_7b.2_universe <- n_distinct(spm_7b2_dq$PersonalID)
  
  Count_7b.2.PH_dest <- spm_7b2_dq %>% #this only has those with an exit date in report range
    filter(Destination %in% c(400:499)) %>% 
    n_distinct(.$PersonalID)
  
  Count_7b.2_stayers <- spm_7b2_dq %>% 
    filter(spm.7b.2_stayer_qual) %>% 
    n_distinct(.$PersonalID)
  
  Count_7b.2_styrs.exits <- Count_7b.2.PH_dest + Count_7b.2_stayers
  
  Percent_7b.2_successful <- round((Count_7b.2_styrs.exits/Count_7b.2_universe)*100,2)
  
  spm_7b2 <- data.frame(
    "row_names" =c(
      "Universe: Persons in all PH projects except PH-RRH who exited after moving into housing, or who moved into housing and remained in the PH project",
      "Of persons above, those who remained in applicable PH projects and those who exited to permanent housing destinations",
      "% Successful exits/retention"),
    "Previous.FY" = c(NA,NA,NA),
    "Current.FY" = as.character(c(Count_7b.2_universe,Count_7b.2_styrs.exits,Percent_7b.2_successful)),
    "Percent.Difference" = c(NA,NA,NA))
  
}

rm(list = ls()[ls() %nin% items_to_keep]) 