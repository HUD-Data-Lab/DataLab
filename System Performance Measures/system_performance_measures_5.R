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

# Measure 5 - Number of Persons who Become Homeless for the First Time ----

#### QUESTIONS
### Appropriate to use Method 5 exit date for last active date?
### Do we want to consolidate the code with functions (loops) or fine as-is?

#### OBSERVATIONS
### With report_start_date coded to lookback_stop_date + 7 years, never will hit lookback_stop_date

#### SCRIPT START

## define report range universes ----

### 5.1 (ES, SH, TH, PH) ----
Q5M1_report <- method_5_active_enrollments %>%
  filter(ProjectType %in% c(0,1,2,8) & 
           EntryDate >= report_start_date ) %>%
  arrange(PersonalID, EntryDate, EnrollmentID) %>% 
  group_by(PersonalID) %>%
  filter(row_number()==1) %>%
  mutate(
    client_lookbackdate = max( EntryDate %m-% days(730), lookback_stop_date)
  ) %>% 
  ungroup() %>%
  rename( "client_startdate" = "EntryDate") %>%
  select(PersonalID, client_startdate, client_lookbackdate)


### 5.2 (ES, SH, TH, PH) ----
Q5M2_report <- method_5_active_enrollments %>%
  filter(ProjectType %in% c(0,1,2,8,3,9,10,13) &
           EntryDate >= report_start_date) %>%
  arrange(PersonalID, EntryDate, EnrollmentID) %>%
  group_by(PersonalID) %>%
  filter(row_number()==1) %>%
  mutate(
    client_lookbackdate = max( EntryDate %m-% days(730), lookback_stop_date )
  ) %>% 
  ungroup() %>%
  rename( "client_startdate" = "EntryDate") %>%
  select(PersonalID, client_startdate, client_lookbackdate)

## define lookback universes ----

### base lookback_u ----
Q5_lookback_u <- enrollment_data %>%
  filter(EntryDate < report_start_date &
           (
             (ProjectType == 1 &
                EnrollmentID %in% bed_nights_in_report$EnrollmentID)
             |
               (ProjectType %in% c(0, 2, 3, 8, 9, 10, 13))
           )
  )

### 5.1 lookback ----
Q5M1_lookback <- Q5_lookback_u %>%
  filter(PersonalID %in% Q5M1_report$PersonalID ) %>%
  arrange(PersonalID, desc(Method5_ExitDate), desc(EnrollmentID)) %>%
  group_by(PersonalID) %>% 
  filter(row_number()==1) %>%
  ungroup() %>%
  rename( "lookback_lastactivedate" = "Method5_ExitDate") %>%
  select(PersonalID, lookback_lastactivedate)

### 5.2 lookback ----
Q5M2_lookback <- Q5_lookback_u %>%
  filter( PersonalID %in% Q5M2_report$PersonalID ) %>%
  arrange(PersonalID, desc(Method5_ExitDate), desc(EnrollmentID)) %>%
  group_by(PersonalID) %>% 
  filter(row_number()==1) %>%
  ungroup() %>%
  rename( "lookback_lastactivedate" = "Method5_ExitDate") %>%
  select(PersonalID, lookback_lastactivedate)

## calculations ----

### counts of report persons ----
Q5M1_C2 <- Q5M1_report %>% 
  summarise(count = n()) %>%
  .$count

Q5M2_C2 <- Q5M2_report %>% 
  summarise(count = n()) %>%
  .$count

### counts of report persons in lookback ----
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

### counts of new report persons (not in lookback) ----
Q5M1_C4 <- Q5M1_C2 - Q5M1_C3

Q5M2_C4 <- Q5M2_C2 - Q5M2_C3

## build output tables ----

### row headers ----
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

### empty columns ----
Q5_Col_B <- list("Previous FY", "", "", "")
Q5_Col_D <- list("Difference", "", "", "")

### measure value columns ----
Q5M1_Col_C <- list("Current FY", Q5M1_C2, Q5M1_C3, Q5M1_C4)

Q5M2_Col_C <- list("Current FY", Q5M2_C2, Q5M2_C3, Q5M2_C4 )

### table assembly ----
Q5M1_table <- data.frame(cbind(Q5M1_Col_A, Q5_Col_B, Q5M1_Col_C, Q5_Col_D)) %>% 
  setnames(.,c("A", "B", "C", "D"))

Q5M2_table <- data.frame(cbind(Q5M2_Col_A, Q5_Col_B, Q5M2_Col_C, Q5_Col_D)) %>% 
  setnames(.,c("A", "B", "C", "D"))
