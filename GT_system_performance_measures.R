
library(tidyverse)
library(lubridate)
library(readxl)

#SPM Programming specs: https://icfonline.sharepoint.com/:w:/r/sites/NHDAP/_layouts/15/Doc.aspx?action=edit&sourcedoc=%7B4ec4f3da-89f4-4c2d-810c-1f93ecc0e60c%7D&wdOrigin=TEAMS-ELECTRON.teamsSdk.openFilePreview&wdExp=TEAMS-CONTROL&web=1
#HMIS Glossary: https://icfonline.sharepoint.com/:w:/r/sites/NHDAP/_layouts/15/Doc.aspx?action=edit&sourcedoc=%7Bda79cae5-b933-41f0-b408-40162ade797d%7D&wdOrigin=TEAMS-ELECTRON.teamsSdk.openFilePreview&wdExp=TEAMS-CONTROL&web=1

#Run lines 1 - 102 in source(system_performance_measures.R)

#Create table to fill values in as we complete them.
SPM.1a <- data.frame(
  "Population" =c("Persons in ES-EE,ES-NbN, and SH","Persons in ES-EE, ES-NbN,SH,TH, and PH"),
  "Previous.FY.Universe.(Persons)" = c(NA,NA),
  "Current.FY.Universe.(Persons)" = c(NA,NA),
  "Previous.FY.Average.LOT.Experiencing.Homelessness" = c(NA,NA),
  "Current.FY.Average.LOT.Experiencing.Homelessness" = c(NA,NA),
  "Difference" = c(NA,NA),
  "Previous.FY.Median.LOT.Experiencing.Homelessness" = c(NA,NA),
  "Current.FY.Median.LOT.Experiencing.Homelessness" = c(NA,NA),
  "Difference2" = c(NA,NA)
)

view(SPM.1a) # view empty table

# Measure 1a: Length of time Persons
# Need Active Clients M5: 1+ Nights Active
# Data needed: [Project Start date], [Report End Date], [Project Exit Date], [Report Start Date], [Project Type], [Date of bed night (TypeProvided == 200)]
# ([Project Start Date] )

###****

df_entryExit <- Client %>% 
  left_join(Enrollment, by = "PersonalID",keep = FALSE) %>% 
  left_join(Exit, by = "EnrollmentID","PersonalID",keep = FALSE) %>% 
  left_join(Project, by = "ProjectID",keep = FALSE)

df_entryExit <- df_entryExit %>% 
  rename(PersonalID = PersonalID.x)

# 1. System Performance Measure 1: Length of Time Persons Remain Homeless ----
## Step 1: Create DF  ----
#Select active clients across all projects of relevant types in the CoC who have a Bednight in the report range. Use Method 5: Active Clients
# NOTE: NbN only applies to Nbn. Use Service date to create Dummy enrollemnts to allow entrydate to be used consistently through

df_NbN_srvcs <- Services %>% 
  filter(RecordType == 200) %>% 
  select(EnrollmentID,PersonalID,DateProvided) %>% 
  mutate(Entrydate = DateProvided,
         Exitdate = DateProvided)

df_SPM.1.join <- Services %>% 
  left_join(df_entryExit,by="EnrollmentID","PersonalID")


df_SPM.1_base <- df_SPM.1.join %>% 
  # Create a filter variable for Method 5 of Active Clients. Refer to HMIS Glossary for description
  mutate(M5_EE_qual = 
           EntryDate <= report_end_date &
           (ExitDate >report_start_date | is.na(ExitDate)),
         M5_bdnt_qual = ProjectType == 1 & 
           TypeProvided == 200 & # I think this will leave out other project types in the dataset. May need to modify it to allow other project types
           DateProvided >= report_start_date & 
           DateProvided <= report_end_date & 
           DateProvided >= EntryDate, 
         M5_ptype_qual = ProjectType %in% c(0,2,3,8,9,10,13), #Use this to filter by clients that meet base qualifier under Method 5: Active Clients
         M5.Active.Clients = 
           M5_EE_qual & 
           (M5_bdnt_qual | M5_ptype_qual), # Active clients following Method 5 outlined in the Glossary 
         M1ab.L1.crrnt.prsns_qual = #Using [M5.Active.Clients] create the base active clients using the identified project types in the SPM reporting specs
           M5.Active.Clients & 
           (ProjectType %in% c(0,1,8)), #Active clients who are in ES-EE, ES-Nbn, and SH
         M1ab.L2.crrnt.prsns_qual = 
           M5.Active.Clients == 1 &
           (ProjectType %in% c(0,1,8,2)), # Active clients who are in ES-EE, ES-Nbn, SH, TH
         M1b.HmlsEntry_qual =  #Homeless at entry qualification for SPM 1b
           LivingSituation %in% c(16,1,18), #living situation codes are changed in FY24 (i.e., 16 ~ 116)
         M1b.strtdate_qual = 
           EntryDate >= report_start_date & 
           EntryDate <= report_end_date,
         M1b.hsngdate_qual = 
           MoveInDate >= report_start_date & 
           MoveInDate <= report_end_date,
         M1b.extdate_qual = 
           is.na(MoveInDate) & ExitDate >= report_start_date & ExitDate <= report_end_date,
         M1b.Actv.clnt.add = M1b.HmlsEntry_qual & (M1b.strtdate_qual | M1b.hsngdate_qual | M1b.extdate_qual))  #*** Use this variable for including the additional criteria for measure 1.b
  
# This is for including the additional Active client criteria for measure M1.b Which includes persons in PH project types 3,9,10 and 13
#These are the qualifier variables
  
  
### Keep all relevant bed nights ----
df_SPM.1.bednht <- df_SPM.1_base %>% 
  select(EnrollmentID, PersonalID.x,ProjectID,ProjectType,
         EntryDate,TypeProvided,DateProvided,
         MoveInDate,ExitDate,
         M5.Active.Clients,M1ab.L1.crrnt.prsns_qual,
         M1ab.L2.crrnt.prsns_qual,M1b.Actv.clnt.add) %>% 
  filter(DateProvided >= EntryDate & DateProvided <= report_end_date)


### Gwen Recommendation. Does the same thing, but based on Boolean logic (returns TURE or FALSE) ----

df_SPM.1_gwen_rec <- df_SPM.1.join %>% 
  # Create a filter variable for Method 5 of Active Clients. Refer to HMIS Glossary for description
  mutate(M5_EE_qual = EntryDate <= report_end_date & 
           (ExitDate > report_start_date | is.na(ExitDate)),
         M5_bdnt_qual = ProjectType == 1 & 
           TypeProvided == 200 & # I think this will leave out other project types in the dataset. May need to modify it to allow other project types
           DateProvided >= report_start_date & 
           DateProvided <= report_end_date & 
           DateProvided >= EntryDate, 
         M5_ptype_qual = ProjectType == 0 | ProjectType == 2 | 
           ProjectType == 3 | ProjectType == 8 | 
           ProjectType == 9 | ProjectType == 10 | 
           ProjectType == 13, 
         #*** Use this to filter by clients that meet base qualifier under Method 5: Active Clients
         M5.Active.Clients = M5_EE_qual & 
           (M5_bdnt_qual | 
              M5_ptype_qual))  # Active clients following Method 5 outlined in the Glossary

## Step 2: Negate bed nights of overlapping HMIS records ----


df_negateTest <- df_SPM.1.bednht %>% # Filter to keep relevant project data
  filter(ProjectType == 0 | 
           ProjectType == 1 | 
           ProjectType == 2 | 
           ProjectType == 3 |
           ProjectType == 8 |
           ProjectType == 9 |
           ProjectType == 10 |
           ProjectType == 13)

#2 and 13 are the available project types in Iowa BOS data

df_episodes.TH <- df_negateTest %>% 
  filter(ProjectType == 02) %>% 
  arrange(PersonalID.x, by=EntryDate) %>% 
  group_by(PersonalID.x) %>% 
  summarise(EntryDate = first(EntryDate), ExitDate = last(ExitDate)) %>% 
  arrange(PersonalID.x, by=EntryDate)

df_episodes.PH.RRH <- df_negateTest %>% 
  filter(ProjectType == 13) %>% 
  arrange(PersonalID.x, by=EntryDate) %>% 
  group_by(PersonalID.x) %>% 
  summarise(EntryDate = first(EntryDate), ExitDate = last(ExitDate)) %>% 
  arrange(PersonalID.x, by=EntryDate)
  
### Negate test ----  

Patient_episodes<- tribble(
  ~patient, ~admitted, ~discharge,
  810, "2020-12-15", "2020-12-16", # interval 1 - 15 to 16
  811, "2021-05-15", "2021-06-30",
  810, "2021-06-17", "2021-06-19", #Interval 2 - 17 to 03
  810, "2021-06-19", "2021-06-27", #Interval 2
  810, "2021-06-27", "2021-07-03", #interval 2
  810, "2021-07-11", "2021-07-17", #Interval 3 - 11 to 18
  810, "2021-07-12", "2021-07-14", #Interval 3
  810, "2021-07-16", "2021-07-18"  #interval 3
) 



#Solution found here: [https://stackoverflow.com/questions/72188780/grouping-dates-in-r-to-create-patient-episodes]

Patient_episodes %>% 
  arrange(patient, by=admitted) %>% # Sorts by patient and sorts from earliest start date to latest
  mutate(group = discharge == lead(admitted) | # No idea why group is here. But I would like to have an interval code.
           admitted == lag(discharge)) %>%  
  group_by(patient, group) %>%
  summarise(admitted = first(admitted), discharge = last(discharge)) %>% # key part of the code. This pulls the dates based on the sort
  arrange(patient, by=admitted)

# In [https://www.pharmasug.org/proceedings/2019/BP/PharmaSUG-2019-BP-219.pdf] page 5 there was a concern on grouping based on row reference
# I added the example in this code and it seems that R is correcting for that concern. 



# got this from [https://stackoverflow.com/questions/60459164/creating-episodes-for-groups-based-on-date-sequence]
# Not using this code chunk
df_overlap %>%
  group_by(Personalid) %>%
  mutate(difftime = Entry.date - lag(Entry.date, default = first(Entry.date)),
         expected2 = cumsum(difftime >= 30) + 1)

### Step 1: Create episodes ----


df4 <- df3

df4 <- df4 %>% 
  mutate(ProjectType_name = case_when(
    ProjectType %in% 0 ~ "ES EE",
    ProjectType %in% 1 ~ "ES NbN",
    ProjectType %in% 2 ~ "TH",
    ProjectType %in% 3 ~ "PH-PSH",
    ProjectType %in% 4 ~ "SO",
    ProjectType %in% 6 ~ "SSO",
    ProjectType %in% 7 ~ "Other",
    ProjectType %in% 8 ~ "Safe Haven",
    ProjectType %in% 9 ~ "PH-Housing only",
    ProjectType %in% 10 ~ "PH-Housing w Services",
    ProjectType %in% 11 ~ "Day Shelter",
    ProjectType %in% 12 ~ "Homeless Prevention",
    ProjectType %in% 13 ~ "PH-RRH",
    ProjectType %in% 14 ~ "CE"
  )) %>% group_by(PersonalID.x,EnrollmentID)



df %>% 
  arrange(patient, by=admitted) %>% # Sorts by patient and sorts from earliest start date to latest
  mutate(group = discharge == lead(admitted) | # No idea why group is here. I would like to have an interval code.
           admitted == lag(discharge)) %>%  
  group_by(patient, group) %>%
  summarise(admitted = first(admitted), discharge = last(discharge)) %>% # key part of the code. This pulls the dates based on the sort
  arrange(patient, by=admitted)



# 3.2. System Performance Measure 3.2: Persons Experiencing Homelessness ----

#NOTE need to check the active clients qualifiers and filters. Expected more than 1 eligible person to be counted

## DF reference ----
SPM.3.2 <- data.frame(
  "Project Type" =c("Unduplicated Total sheltered persons","Emergency Shelter Total", "Safe Haven Total", "Transitional Housing Total"),
  "Previous.FY" = NA,
  "Current.FY" = c((SPM.3.2_ES.Count+SPM.3.2_SH.Count+SPM.3.2_TH.Count),SPM.3.2_ES.Count,SPM.3.2_SH.Count,SPM.3.2_TH.Count),
  "Difference" = NA
)

SPM.3.2

## Create DF with Active client counts ----
df_spm.3.2_base <- df_SPM.1_base

df_spm.3.2_base <- df_spm.3.2_base %>% 
  mutate(M2.ES.Nbn_enrlmnt_qual = 
           ExitDate >= report_start_date,
         M2.ES.Nbn_srvc_qual =
           EntryDate <= report_end_date &
           (is.na(ExitDate) | ExitDate > report_end_date) &
           EntryDate >= report_start_date &
           EntryDate <= report_end_date &
           EntryDate >= report_end_date,
         M2.Active.Clients = M2.ES.Nbn_enrlmnt_qual | M2.ES.Nbn_srvc_qual,
         M1.Active.Clients =
           EntryDate <= report_end_date &
           (ExitDate >= report_start_date | is.na(ExitDate))
           )
# Count ES

SPM.3.2_ES.Count<- df_spm.3.2_base %>% 
  filter(ProjectType == 0 | ProjectType == 1) %>% 
  filter(M1.Active.Clients | M2.Active.Clients) %>% 
  n_distinct("PersonalID.x")

# Count Safe Haven
SPM.3.2_SH.Count <- df_spm.3.2_base %>% 
  filter(ProjectType == 8 & M1.Active.Clients) %>% 
  n_distinct("PersonalID.x")

# Count Th

SPM.3.2_TH.Count <- df_spm.3.2_base %>% 
  filter(ProjectType == 2 & M1.Active.Clients) %>% 
  n_distinct("PersonalID.x")

# ES project type 0 and 1
# Safe haven project type 8
# TH Project type = 2

# 7. System Performance Performance Measure 7: Successful Placement from Street Outreach and Successful Placement in or Retention of Permanent Housing ----

# **DONT FORGET** to run lines 1 - 100 in source(system_performance_measures.R)

## Metric 7a.1 ----  

`%nin%` = Negate(`%in%`)

{
df_SPM.7a1_counts <- enrollment_data %>% 
  filter(ProjectType == 4,
         EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_start_date),
         PersonalID %nin% enrollment_data$PersonalID[enrollment_data$ExitDate > report_end_date | is.na(enrollment_data$ExitDate)], # Exclude everyone with a ineligible exit. This should only leave people with a single enrollment or enrollments that end in the report period
         Destination %nin% c(206,329,24)) %>% 
  group_by(PersonalID) %>% 
  arrange(desc(EnrollmentID), .by_group = TRUE) %>% # sorts by most recently created enrollment date. This will be the tie-breaker.
  slice(which.max(ExitDate)) #Keeps the most recent exit information. If the exit dates are the same it will keep first in DF

Count_7a.1_universe <- df_SPM.7a1_counts %>% n_distinct(.$PersonalID)
Count_7a.1_temp.inst.exts <- df_SPM.7a1_counts %>% 
  filter(Destination %in% 200:399) %>% 
  n_distinct(.$PersonalID)
Count_7a.1_PH.exts <- df_SPM.7a1_counts %>% 
  filter(Destination %in% 400:499) %>% 
  n_distinct(.$PersonalID)
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
         EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_start_date), #includes stayers for metric 7b.2
         Destination %nin% c(206,329,24))
 
### 7b.1 Metric ----

df_7b.1_counts <- df_7b_baseline %>% 
  mutate("spm.7b.leaver_qual" = ProjectType %in% c(3,9,10) & MoveInDate <= report_end_date) %>% 
  filter(PersonalID %nin% df_7b_baseline$PersonalID[df_7b_baseline$ExitDate > report_end_date | is.na(df_7b_baseline$ExitDate)], # Exclude everyone with a ineligible exit or still active. This should only leave people with a single enrollment that ends in the report period
    spm.7b.leaver_qual == FALSE) %>% 
  group_by(PersonalID) %>% 
  arrange(desc(EnrollmentID), .by_group = TRUE) %>% # sorts by most recently created enrollment date. This could be the spot for a tie-breaker.
  slice(which.max(ExitDate))

Count_7b.1_universe <- n_distinct(df_7b.1_counts$PersonalID)
  
Count_7b.1_exits.PH <- df_7b.1_counts %>% 
  filter(Destination %in% 400:499) %>% 
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

df_7b.2_stayers.leavers <- df_7b_baseline %>% 
  mutate("spm.7b.2_leaver_qual" =  EntryDate <= report_end_date & ExitDate <= report_end_date,
         "spm.7b.2_stayer_qual"= EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate > report_end_date)) %>% 
  filter(ProjectType %in% c(3,9,10) & (spm.7b.2_stayer_qual | spm.7b.2_leaver_qual)) %>% 
  group_by(PersonalID) %>% 
  arrange(!is.na(ExitDate), desc(ExitDate), .by_group = TRUE) %>%  #Arrange with NA at top and then descending from most recent exit
  slice_head(n=1) #Keep first row of each group

# There are cases of ties for is.na(ExitDate). Example client 511607 has two active PSH enrollments (Projecttype == 3) with two different move in and entry dates
# Arrange is keeping the enrollment with the most recent entry date

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
library(kableExtra)

Metric.7a1.table %>% 
  kbl(caption = "Metric 7a.1 - Change in exits to permanent housing destination") %>% 
  kable_styling(bootstrap_options = c("striped","hover"))

Metric.7b1.table %>% 
  kbl(caption = "Metric 7b.1 - Change in exits to permanent housing destinations") %>% 
  kable_styling(bootstrap_options = c("striped","hover"))

Metric.7b2.table %>% 
  kbl(caption = "Metric 7b.2 - Change in exit to or retention of permanent housing ") %>% 
  kable_styling(bootstrap_options = c("striped","hover"))


# Draft section Don't Review ----

# CHECK Compare universe client lists to check for duplicates
spm.7a1_persons <- df_SPM.7a.1.exits[,2]
spm7.b1_persons <- df_SPM.7b.1.exits[,2]

Duplicates <- rbind(spm.7a1_persons,spm7.b1_persons)

df_dupCount <- Duplicates %>% 
  group_by(PersonalID) %>% 
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  filter(n > 1)
#107 duplicates in both universe lists. There should be no overlap between each universe df

df_dupCount
df_dupCount_list <- df_dupCount$PersonalID

df_sppm_duplicates <- df_SPM.7.baseline %>% 
  filter(PersonalID %in% df_dupCount_list) %>% 
  select(EnrollmentID,PersonalID, EntryDate, ExitDate,ProjectType)

df_sppm_duplicates <- df_sppm_duplicates %>% 
  mutate(SPM.7_in_report_range = ExitDate<report_end_date & ExitDate>report_start_date) %>% 
  arrange(PersonalID,by=ExitDate)

DF_duplicates_test <- df_SPM.7.baseline %>% 
  filter(PersonalID == 416498)


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

`%nin%` = Negate(`%in%`)

df_SPM.7.exits <- df_SPM.7.exits %>% 
  filter(PersonalID %nin% df_SPM.7_list, # Exclude everyone with a ineligible exit. This should only leave people with a single enrollment or enrollments that end in the report period
         Destination %nin% c(206,329,24)) %>% 
  group_by(PersonalID) %>% 
  slice(which.max(ExitDate))  # Why not slice_max()? // this keeps the latest exit for all duplicates. There should be no duplicates after this

### New Idea Counts ----

SPM.7a.1_counts <- df_SPM.7.exits %>% #creates a summary table with the counts for SPM.7a.1
  filter(ProjectType == 4) %>% 
  group_by(Destination_Names) %>% 
  summarise(n=n())

SPM.7b.1_counts <- df_SPM.7.exits %>% #creates a summary table with the counts for SPM.7b.1
  filter(ProjectType %in% c(0,1,8,2,13)) %>% 
  group_by(Destination_Names) %>% 
  summarise(n=n())

### New Idea Duplicate check ----

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


