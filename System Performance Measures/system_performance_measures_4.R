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

### IF RUNNING STANDALONE: run system_performance_measures_0.R through line 118 first ###

# Measure 4: Employment and Income Growth for Homeless Persons in CoC Program-funded Projects

# Define universe ----

## Limit Enrollments to Active Client Method 1 and CoC-funded projects----

M4_enrollment_u <- active_enrollments %>%
  filter(
    Method1 == TRUE,
    ProjectType %in% c(2,3,8,9,10,13),
    ProjectID %in% 
      filter(Funder, 
             Funder %in% c(2,3,4,5,43,44,54,55) &
               StartDate <= report_end_date & 
               (is.na(EndDate) | EndDate >= report_start_date)
             )$ProjectID
    ) %>%
  inner_join(
    Client %>% select(PersonalID, DOB),
    by = "PersonalID"
  )

## Split into stayer & leaver universes & limit to adults----

# stayer clients (unique list of clients active on report_end_date)

M4_stayer_clients <- M4_enrollment_u %>% 
  filter(is.na(ExitDate) | ExitDate > report_end_date) %>%
  .$PersonalID %>%
  unique()

# stayers

M4_stayers <- M4_enrollment_u %>%
  filter(
    .$PersonalID %in% M4_stayer_clients, 
    difftime( report_end_date , EntryDate, units = "days") >= 365
    ) %>%
  arrange(PersonalID, EntryDate, EnrollmentID) %>%
  group_by(PersonalID) %>%
  filter(row_number()==1) %>%
  mutate(
    Age = floor(age_calc( DOB, max( report_start_date, EntryDate), units = "years"))
  ) %>%
  ungroup() %>% 
  filter(Age >= 18)
  
# leavers

M4_leavers <- M4_enrollment_u %>%
  filter(
    .$PersonalID %nin% M4_stayer_clients,
    ExitDate >= report_start_date & ExitDate <= report_end_date
  ) %>%
  arrange(PersonalID, EntryDate, EnrollmentID) %>%
  group_by(PersonalID) %>%
  filter(row_number()==1) %>%
  mutate(
    Age = floor(age_calc( DOB, max( report_start_date, EntryDate), units = "years"))
  ) %>%
  ungroup() %>% 
  filter(Age >= 18)

## Lookup income start + end records ----

# 



