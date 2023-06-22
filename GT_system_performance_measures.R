
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

# 4. System Performance Measure 4: Employment and Income ----

## 4.1 Adult system stayers

# 7. System Performance Performance Measure 7: Exit from Street Outreach ----

#Street Outreach Projectype = 4
# Homeless situations -> 101,116,118
# Institutional settings -> 206 (exclude from measure),215,207,204,205,225
# Temporary Housing Situations -> 329 (exclude from measure), 314, 312, 313, 302, 327, 332
# Permanent -> 426,411,421,410,435,422,423
# Other -> 24 (Exclude from universe),8,9,99,30,17

#Baseline data
df_SPM.7.baseline <- enrollment_data # May not be an important step, but i always liked using "df"

## 7a.1 Change in exits to permanent housing destinations ----  
### Add active clients filter Method1 and add destination names ----
df_SPM.7a1.M1.active <- df_SPM.7.baseline %>% 
  filter(ProjectType == 4,
         EntryDate <= report_end_date &
           (is.na(ExitDate) | ExitDate > report_start_date)) %>% 
  mutate(Destination_Names = case_when(
    Destination %in% c(101,116,118) ~ "Homeless.Sit",
    Destination %in% c(206,215,207,204,205,225) ~ "Institutional.Set",
    Destination %in% c(329,314,312,313,302,327,332) ~ "Temporary.Sit",
    Destination %in% c(426,411,421,410,435,422,423) ~ "Permanent.Sit",
    Destination %in% c(24,8,9,99,30,17) ~ "Other"
  ))

# This is unimportant, but one idea I had was identifying duplicates to identify who exited and then returned to another SO project
df_spm.7_dupCnt<- df_SPM.7a1.M1.active %>% 
  group_by(PersonalID) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


df_SPM.7a.1.exits <- df_SPM.7a1.M1.active %>% 
  arrange(PersonalID,by=EntryDate) %>% # May be able to remove this line
  left_join(df_spm.7_dupCnt, by = "PersonalID") %>%  # May be able to remove this line
   mutate(No_exit = is.na(ExitDate) | ExitDate > report_end_date,
         No_exit_list = ifelse(No_exit,PersonalID,NA)) # This is to get a list of all personalIDs with an ineligible enrollment.

df_SPM.7_list <- unique(df_SPM.7a.1.exits$No_exit_list,na.rm=TRUE) #Use the T/F from No_exit_list to create the list
df_SPM.7_list <- df_SPM.7_list[!is.na(df_SPM.7_list)] # Remove the NA

`%nin%` = Negate(`%in%`)

df_SPM.7a.1.exits <- df_SPM.7a.1.exits %>% 
  filter(PersonalID %nin% df_SPM.7_list, # Exclude everyone with a ineligible exit. This should only leave people with a single enrollment or enrollments that end in the report period
         Destination %nin% c(206,329,24)) %>% 
  group_by(PersonalID) %>% 
  slice(which.max(ExitDate)) # Why not slice_max()? // this keeps the latest exit for all duplicates. There shoudl be no duplicates after this

SPM.7a.1_counts <- df_SPM.7a.1.exits %>% #creates a summary table with the counts for SPM.7a.1
  group_by(Destination_Names) %>% 
  summarise(n=n())


### Test ----
df_SPM.7.test <- df_SPM.7a1.M1.active %>% 
  filter(PersonalID %in% c(640839, 652692, 512662,104183, 112123)) %>% 
  select(EnrollmentID,PersonalID,ProjectID,ProjectType, EntryDate,ExitDate,Destination,Destination_Names) %>% 
  arrange(PersonalID,by=EntryDate)

df_SPM.7.test <- df_SPM.7.test %>% 
  left_join(df_spm.7_dupCnt, by = "PersonalID")

df_SPM.7.test <-df_SPM.7.test %>% 
  mutate(No_exit = is.na(ExitDate) | ExitDate > report_end_date,
         No_exit_list = ifelse(No_exit,PersonalID,NA))


df_SPM.7_list <- unique(df_SPM.7.test$No_exit_list,na.rm=TRUE)
df_SPM.7_list <- df_SPM.7_list[!is.na(df_SPM.7_list)]

`%nin%` = Negate(`%in%`)

df_SPM.7.test <- df_SPM.7.test %>% 
  filter(PersonalID %nin% df_SPM.7_list,
         Destination %nin% c(206,329,24)) %>% 
  group_by(PersonalID) %>% 
  slice(which.max(ExitDate))

df_SPM.7.test %>% group_by(Destination_Names) %>% summarise(n=n())







