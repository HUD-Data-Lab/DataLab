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

# install and/or load packages
if(!require(tidyverse)) {
  install.packages("tidyverse"); 
  library(tidyverse)
}

# parameters
lookback_stop_date <- ymd("2013-10-1") #old vaklue 2014-10-01

system_performance_measures <- TRUE

# subscripts
source("00_read_2022_csv.R")
source("FY2024_mapping.R")

# Is there a reason this needs to be here and not above with other params?
report_start_date <- lookback_stop_date %m+% years(7)

# SPM universe
single_CoC_projects <- ProjectCoC %>%
  mutate(relevant_to_CoC = 
           str_sub(relevant_CoC, 4, 6) == str_sub(CoCCode, 4, 6)) %>%
  group_by(ProjectID) %>%
  summarise(num_of_CoCs = uniqueN(CoCCode),
            relevant_to_CoC = max(relevant_to_CoC)) %>%
  ungroup() %>%
  filter(num_of_CoCs == 1 &
           relevant_to_CoC == 1)

relevant_household_ids <- Enrollment %>%
  filter(RelationshipToHoH == 1 &
           (str_sub(relevant_CoC, 4, 6) == str_sub(EnrollmentCoC, 4, 6) |
              (is.na(EnrollmentCoC) & ProjectID %in% single_CoC_projects$ProjectID))) %>%
  select(HouseholdID) %>%
  distinct()

Enrollment <- Enrollment %>%
  filter(HouseholdID %in% relevant_household_ids$HouseholdID)

for (csv in c("Exit", "IncomeBenefits")) {
  assign(csv,
         get(csv) %>%
           filter(EnrollmentID %in% Enrollment$EnrollmentID))
}

enrollment_data <- Enrollment %>%
  select(all_of(colnames(Enrollment)[1:18])) %>%
  left_join(Exit %>%
              select(EnrollmentID, ExitDate, Destination),
            by = "EnrollmentID") %>%
  left_join(Project %>%
              select(ProjectID, ProjectType),
            by = "ProjectID") %>%
  mutate(Method5_ExitDate = if_else(
    is.na(ExitDate) | ExitDate >= report_end_date,
    report_end_date, ExitDate %m-% days(1)))

NbN_projects <- c(1212, 1210)
source("create_NbN_stays.R")
Project$ProjectType[Project$ProjectID %in% NbN_projects] <- 1

all_bed_nights <- Services %>%
  inner_join(enrollment_data %>%
              filter(ProjectType == 1) %>%
              select(EnrollmentID, EntryDate, ExitDate),
            by = "EnrollmentID") %>%
  filter(RecordType == 200 & 
           TypeProvided == 200 &
           DateProvided >= EntryDate &
           DateProvided <= report_end_date &
           # removed because specs call this an error in data entry
           # which--correcting for data quality is a whole other thing
           (is.na(ExitDate) |
              DateProvided < ExitDate))

bed_nights_in_report <- all_bed_nights %>%
  filter(DateProvided >= report_start_date)

# need to review this logic...
bed_nights_in_report_ee_format <- bed_nights_in_report %>%
  left_join(enrollment_data %>%
              select(EnrollmentID, ProjectID, ProjectType),
            by = "EnrollmentID") %>%
  mutate(EnrollmentID = paste("S_", ServicesID),
         EntryDate = DateProvided,
         Method5_ExitDate = DateProvided) %>%
  select(EnrollmentID, PersonalID, EntryDate, Method5_ExitDate,
         ProjectID, ProjectType)

method_5_active_enrollments <- enrollment_data %>%
  filter(EntryDate <= report_end_date &
           (is.na(ExitDate) |
              ExitDate > report_start_date) &
           (
             (ProjectType == 1 &
                EnrollmentID %in% bed_nights_in_report$EnrollmentID)
             |
             (ProjectType %in% c(0, 2, 3, 8, 9, 10, 13))
           )
         )

#  Measure 1a ----
{
  ##  uses this algorithm 
  ##  https://www.pharmasug.org/proceedings/2019/BP/PharmaSUG-2019-BP-219.pdf
  Q1a_line1_detail <- method_5_active_enrollments %>%
    filter(ProjectType %in% c(0, 8) &
    # is this right?
      EntryDate < Method5_ExitDate) %>%
    # ...and the way I add it here
    full_join(bed_nights_in_report_ee_format,
              by = colnames(bed_nights_in_report_ee_format)) %>%
    # unnecessary, added for algorithm QA
    select(colnames(bed_nights_in_report_ee_format)) %>%
    create_lot_blocks()
  
  df_for_negation <- enrollment_data %>%
    select(PersonalID, ProjectType, EntryDate, Method5_ExitDate)
  
  negated <- method_5_active_enrollments %>%
    filter(ProjectType %in% c(0, 8)) %>%
    select(PersonalID, ProjectType, EntryDate, Method5_ExitDate)
    left
  
  create_lot_blocks <- function(enrollments_with_Method5_ExitDate) {
    enrollments_with_Method5_ExitDate %>%
      arrange(PersonalID, EntryDate, Method5_ExitDate) %>%
      group_by(PersonalID) %>%
      mutate(lag_EntryDate = lag(EntryDate),
             lag_Method5_ExitDate = lag(Method5_ExitDate),
             cummax_Method5_ExitDate = as.Date(cummax(
               if_else(is.na(lag_Method5_ExitDate), -Inf, as.integer(lag_Method5_ExitDate))
             ), "1970-01-01"),
             cummax_Method5_ExitDate = case_when(
               cummax_Method5_ExitDate != -Inf ~ cummax_Method5_ExitDate)) %>%
      ungroup() %>%
      mutate(
        after_earlier = (EntryDate > cummax_Method5_ExitDate &
                           !is.na(lag_EntryDate)),
        different_person = PersonalID != lag(PersonalID) &
          !is.na(lag(PersonalID)),
        lot_block = cumsum(after_earlier | different_person) + 1
      ) %>%
      group_by(PersonalID, lot_block) %>%
      summarise(EntryDate = min(EntryDate),
                Method5_ExitDate = max(Method5_ExitDate))
  }
  
  View(Q1a_line1_detail %>%filter(PersonalID == 670613))
  }


# Measure 5 - Number of Persons who Become Homeless for the First Time ----

#### QUESTIONS
  ### Appropriate to use Method 5 exit date for last active date?
  ### Do we want to consolidate the code with functions (loops) or fine as-is?

  ## define report range universes

    ### 5.1 (ES, SH, TH, PH)
    Q5M1_report <- method_5_active_enrollments %>%
      filter(ProjectType %in% c(0,1,2,8) & 
               EntryDate >= report_start_date ) %>%
      arrange(PersonalID, EntryDate, EnrollmentID) %>%
      group_by(PersonalID) %>%
      filter(row_number()==1) %>% # are you just keeping the first enrollment? Is this your de-duplication?
      ungroup() %>% # I have seen this done quite often. What is the point of grouping and the ungrouping?
      mutate(
        client_lookbackdate = max( EntryDate %m-% days(730), lookback_stop_date )
        ) %>% 
      rename( "client_startdate" = "EntryDate") %>%
      select(PersonalID,ProjectType,client_startdate, client_lookbackdate)
    
    ### 5.2 (ES, SH, TH, PH)
    Q5M2_report <- method_5_active_enrollments %>%
      filter(ProjectType %in% c(0,1,2,8,3,9,10,13) &
               EntryDate >= report_start_date) %>% 
      arrange(PersonalID, EntryDate, EnrollmentID) %>%
      group_by(PersonalID) %>%
      filter(row_number()==1) %>%
      ungroup() %>%
      mutate(
        client_lookbackdate = max( EntryDate %m-% days(730), lookback_stop_date )
      ) %>% 
      rename( "client_startdate" = "EntryDate") %>%
      select(PersonalID, client_startdate, client_lookbackdate)
  
  ## define lookback universes
  Q5_lookback_u <- method_5_active_enrollments %>% # Wouldn't this take out the enrollments of the past? Method 5 is still an active client method.
    filter(ProjectType %in% c(0,1,2,8,3,9,10,13) &
             EntryDate < report_start_date) 
  
  
    ### 5.1 lookback
    Q5M1_lookback <- Q5_lookback_u %>%
      filter(PersonalID %in% Q5M1_report$PersonalID ) %>%
      arrange(PersonalID, desc(Method5_ExitDate), desc(EnrollmentID)) %>%
      group_by(PersonalID) %>% 
      filter(row_number()==1) %>%
      ungroup() %>%
      rename( "lookback_lastactivedate" = "Method5_ExitDate") %>%
      select(PersonalID, lookback_lastactivedate)
    
    ### 5.2 lookback
    Q5M2_lookback <- Q5_lookback_u %>%
      filter( PersonalID %in% Q5M2_report$PersonalID ) %>%
      arrange(PersonalID, desc(Method5_ExitDate), desc(EnrollmentID)) %>%
      group_by(PersonalID) %>% 
      filter(row_number()==1) %>%
      ungroup() %>%
      rename( "lookback_lastactivedate" = "Method5_ExitDate") %>%
      select(PersonalID, lookback_lastactivedate)
    
    
  
  ## calculations
    
    ### counts of report persons
    Q5M1_C2 <- Q5M1_report %>%  
      summarise(count = n()) %>%
      .$count
    
    Q5M2_C2 <- Q5M2_report %>% 
      summarise(count = n()) %>%
      .$count
    
    ### counts of report persons in lookback
    Q5M1_C3 <- 
      inner_join(
        x = Q5M1_report,
        y = Q5M1_lookback,
        by = "PersonalID"
      ) %>%
      filter(lookback_lastactivedate >= client_lookbackdate) %>%
      summarise(count = n()) %>%
      .$count
    
    Q5M2_C3 <- 
      inner_join(
        x = Q5M2_report,
        y = Q5M2_lookback,
        by = "PersonalID"
      ) %>%
      filter(lookback_lastactivedate >= client_lookbackdate) %>%
      summarise(count = n()) %>%
      .$count
    
    ### counts of new report persons (not in lookback)
    Q5M1_C4 <- Q5M1_C2 - Q5M1_C3
    
    Q5M2_C4 <- Q5M2_C2 - Q5M2_C3
  
  ## build output tables
  
  ### row headers
  Q5M1_Col_A <- list(
    "",
    "Universe: Person with entries into ES-EE, ES-NbN, SH, or TH during the reporting period.",
    "Of persons above, count those who were in ES-EE, ES-NbN, SH, TH, or any PH within 24 months prior to their start during the reporting year.",
    "Of persons above, count those who did not have entries in ES-EE, ES-NbN, SH, TH or PH in the previous 24 months. (i.e. number of persons experiencing homelessness for the first time)"
    )
  
  Q5M2_Col_A <- list(
    "",
    "Universe: Person with entries into ES-EE, ES-NbN, SH, TH, or PH during the reporting period.",
    "Of persons above, count those who were in ES-EE, ES-NbN, SH, TH, or any PH within 24 months prior to their start during the reporting year.",
    "Of persons above, count those who did not have entries in ES-EE, ES-NbN, SH, TH or PH in the previous 24 months. (i.e. number of persons experiencing homelessness for the first time)"
  )
  
  ### empty columns
  Q5_Col_B <- list("Previous FY", "", "", "")
  Q5_Col_D <- list("Difference", "", "", "")
  
  ### measure value columns
  Q5M1_Col_C <- list("Current FY", Q5M1_C2, Q5M1_C3, Q5M1_C4)
  
  Q5M2_Col_C <- list("Current FY", Q5M2_C2, Q5M2_C3, Q5M2_C4 )
  
  ### table assembly
  Q5M1_table <- data.frame(cbind(Q5M1_Col_A, Q5_Col_B, Q5M1_Col_C, Q5_Col_D)) %>% 
    setnames(.,c("A", "B", "C", "D"))
  
  Q5M2_table <- data.frame(cbind(Q5M2_Col_A, Q5_Col_B, Q5M2_Col_C, Q5_Col_D)) %>% 
    setnames(.,c("A", "B", "C", "D"))

# GT Edits ----
  ### Table outputs
  
library(kableExtra)
  
  Q5M1_table %>% 
    kbl(caption = "Metric 5.1") %>% 
    kable_styling(bootstrap_options = c("striped","hover"))
  
  Q5M2_table %>% 
    kbl(caption = "Metric 5.2") %>% 
    kable_styling(bootstrap_options = c("striped","hover"))
  
  #CSV exports:
  
  Q5M1_report %>% 
    write_csv("5.1.Client.detail.csv")
  
  Q5M2_report %>% 
    write_csv("5.2.Client.detail.csv")
 
   Q5M2_lookback %>% 
     write_csv("5.2.Lookback.detail.csv")  
    