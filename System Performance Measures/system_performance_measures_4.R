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

### IF RUNNING STANDALONE: run system_performance_measures_0.R through line 118 FIRST ###

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
    PersonalID %in% M4_stayer_clients, 
    difftime( report_end_date , EntryDate, units = "days") >= 365
    ) %>%
  arrange(PersonalID, EntryDate, EnrollmentID) %>%
  group_by(PersonalID) %>%
  filter(row_number()==1) %>%
  mutate(
    age = floor(age_calc( DOB, max( report_start_date, EntryDate), units = "years"))
  ) %>%
  ungroup() %>% 
  filter(age >= 18) %>% #end universe filtering
  # start prep calculations for income lookup
  mutate(
    years_enrolled = floor(age_calc( EntryDate, report_end_date, units = "years")),
    report_anniversary = EntryDate %m+% years(years_enrolled),
    report_anniversary_start = report_anniversary %m-% days(30),
    report_anniversary_end = report_anniversary %m+% days(30),
    annual_deadline_in_report = (report_anniversary_end %m+% days(1)) <= report_end_date 
  )
  
# leavers

M4_leavers <- M4_enrollment_u %>%
  filter(
    PersonalID %nin% M4_stayer_clients,
    ExitDate >= report_start_date & ExitDate <= report_end_date
  ) %>%
  arrange(PersonalID, EntryDate, EnrollmentID) %>%
  group_by(PersonalID) %>%
  filter(row_number()==1) %>%
  mutate(
    age = floor(age_calc( DOB, max( report_start_date, EntryDate), units = "years"))
  ) %>%
  ungroup() %>% 
  filter(age >= 18)

## Lookup income start + end records ----

#### QUESTIONS
  ## How to pull in info before lookback_stop_date for old records?
  ## Specs are garbled for step "g" under stayers. But I'm assuming intention
    ## is to identify the AA *previous* to the most recent AA. *LEFT COMMENT*
  ## How to handle >0 amount but "No" for income source?

#### datalab functions referenced
  ## APR/CAPER
    ##get_annual_id

# global income prep
### ADD THIS BACK AFTER reduce the size of the table

# base_income <- IncomeBenefits %>%
#   # mutate(across(c(Earned, Unemployment, SSI, SSDI,
#   VADisabilityService, VADisabilityNonService,
# PrivateDisability, WorkersComp, TANF, GA,
# SocSecRetirement, Pension, ChildSupport,
# Alimony, OtherIncomeSource), ~ ifnull(., 0))

# stayer income

stayer_income <- IncomeBenefits %>%
  filter(
    EnrollmentID %in% M4_stayers$EnrollmentID
  )

stayer_income_entry <- stayer_income %>% filter(DataCollectionStage == 1)

stayer_income_annuals <- stayer_income %>% 
  filter(DataCollectionStage == 5) %>%
  left_join(
    M4_stayers %>% select(EnrollmentID, EntryDate),
    by = "EnrollmentID"
  ) %>% 
  # these identify the relevant anniversary date and associated range 
    # for each existing Annual Assessment and checks to see if within
    # the +-30 day range.
  mutate(
    years_entry_to_aa = floor(age_calc( EntryDate %m-% days(30), InformationDate, units = "years")),
    related_anniversary_date = EntryDate %m+% years(years_entry_to_aa),
    in_range = 
      trunc((related_anniversary_date %--% InformationDate) / days(1)) >= -30 &
      trunc((related_anniversary_date %--% InformationDate) / days(1)) <= 30
    ) %>%
  # used following select step for QA
  #select( EnrollmentID, IncomeBenefitsID, EntryDate, InformationDate, years_entry_to_aa, related_anniversary_date, in_range) %>%
  
  filter(in_range == TRUE) %>%
  
  # remove duplicate in-range AAs for the same anniversary date
    ## there weren't any cases in Iowa BoS test data, but still think should account
      ## for this possibility
  arrange(EnrollmentID, desc(related_anniversary_date), desc(InformationDate), desc(IncomeBenefitsID)) %>%
  select( EnrollmentID,related_anniversary_date, InformationDate, IncomeBenefitsID) %>%
  group_by(EnrollmentID, related_anniversary_date) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>%
  group_by(EnrollmentID) %>%
  mutate(
    count_annuals_by_EE = n()
  ) %>%
  ungroup()


### ISSUE -- Need to determine if NULL Income/Benefit records are to be removed 
  ### and if this should be done prior to other filter steps.

    ## OLD VERSION -- going back 30 days in all cases should be OK
    # years_entry_to_aa = function(x){
    #   x = floor(age_calc( EntryDate, InformationDate, units = "years"))
    #   if(x < 1) {
    #     floor(age_calc( EntryDate %m-% days(30), InformationDate, units = "years"))
    #   } else {x}
    # }
  


