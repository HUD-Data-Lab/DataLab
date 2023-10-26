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
                   c("spm_4.1_4.2_4.3_dq", "spm_4.4_4.5_4.6_dq"),
                   paste0("spm_4.", 1:6))

### IF RUNNING STANDALONE: run system_performance_measures_0.R through line 118 FIRST ###

# Measure 4: Employment and Income Growth for Homeless Persons in CoC Program-funded Projects

# list of source Y/N columns
source_yn_cols <- c(
  "Earned",
  "Unemployment",
  "SSI",
  "SSDI",
  "VADisabilityService",
  "VADisabilityNonService",
  "PrivateDisability",
  "WorkersComp",
  "TANF",
  "GA",
  "SocSecRetirement",
  "Pension",
  "ChildSupport",
  "Alimony",
  "OtherIncomeSource"
)

# list of source amount columns
source_amount_cols <- paste0(c(source_yn_cols[1:14], "OtherIncome"), "Amount")

# Define universe ----

spm_4_projects <- Funder %>%
  filter(Funder %in% c(2, 3, 4, 5, 6, 43, 44, 54, 55) & # Updated to reflect change in programming SPM Programming Specs v1.1 line 1 of programming instructions
           StartDate <= report_end_date &
           (is.na(EndDate) |
              EndDate >= report_start_date) &
           ProjectID %in% Project$ProjectID[Project$ProjectType %in% 
                                              c(2, 3, 8, 9, 10, 13)]) %>%
  .$ProjectID %>%
  unique()

## Limit Enrollments to Active Client Method 1 and CoC-funded projects----

M4_enrollment_u <- active_enrollments %>%
  filter(
    Method1 == TRUE,
    ProjectID %in% spm_4_projects)

## Split into stayer & leaver universes & limit to adults----

# stayer clients (list of clients active on report_end_date)

M4_stayer_clients <- M4_enrollment_u %>% 
  filter(is.na(ExitDate) | ExitDate > report_end_date)

# stayers

spm_4.1_4.2_4.3_dq <- M4_enrollment_u %>%
  filter(
    EnrollmentID %in% M4_stayer_clients$EnrollmentID, 
    difftime( report_end_date , EntryDate, units = "days") >= 365
    ) %>%
  arrange(desc(EntryDate), EnrollmentID) %>%
  group_by(PersonalID) %>%
  slice(1L) %>%
  ungroup() %>% 
  filter(age >= 18 |
           is.na(age)) %>% #end universe filtering
  # start prep calculations for income lookup
  mutate(
    years_enrolled = trunc((EntryDate %--% report_end_date) / years(1)),
    report_anniversary = EntryDate %m+% years(years_enrolled),
    report_anniversary_start = report_anniversary %m-% days(30),
    report_anniversary_end = report_anniversary %m+% days(30) 
  )
  
stayer_income <- IncomeBenefits %>%
  filter(
    EnrollmentID %in% spm_4.1_4.2_4.3_dq$EnrollmentID &
      InformationDate <= report_end_date
  ) 

stayer_income_annuals <- stayer_income %>% 
  filter(DataCollectionStage == 5) %>%
  left_join(
    spm_4.1_4.2_4.3_dq %>% 
      select(EnrollmentID, EntryDate, report_anniversary_start,
             report_anniversary_end),
    by = "EnrollmentID"
  ) %>% 
  # these identify the relevant anniversary date and associated range 
  # for each existing Annual Assessment and checks to see if within
  # the +-30 day range.
  mutate(
    years_entry_to_aa = trunc((EntryDate %m-% days(30) %--% InformationDate) / years(1)),
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
  arrange(desc(InformationDate), desc(IncomeBenefitsID)) %>%
  # Used below select step for QA
  #select( EnrollmentID,related_anniversary_date, InformationDate, IncomeBenefitsID) %>%
  group_by(EnrollmentID, related_anniversary_date) %>% 
  slice(1L) %>% 
  ungroup() %>%
  group_by(EnrollmentID) %>%
  ungroup()

### stayer start and endpoint incomes ----
stayer_end_incomes <- stayer_income_annuals %>%
  # filter(InformationDate >= report_anniversary_start &
  #          InformationDate <= report_anniversary_end) %>%
  arrange(desc(InformationDate)) %>%
  group_by(EnrollmentID) %>%
  slice(1L) %>%
  ungroup() %>%
  determine_total_income() %>%
  mutate(other_income = calculated_total_income - earned_income) %>%
  select(c("EnrollmentID", "IncomeBenefitsID", "InformationDate", 
           "IncomeFromAnySource", all_of(source_amount_cols), "earned_income", 
           "other_income", "calculated_total_income"))

stayer_start_incomes <- stayer_income %>% 
  filter(DataCollectionStage == 1) %>%
  full_join(stayer_income_annuals,
            by = colnames(stayer_income)) %>%
  left_join(stayer_end_incomes %>%
              select(EnrollmentID, InformationDate) %>%
              rename(LaterInformationDate = InformationDate),
            by = "EnrollmentID") %>%
  filter(
    IncomeBenefitsID %nin% stayer_end_incomes$IncomeBenefitsID &
           (InformationDate < LaterInformationDate |
              is.na(LaterInformationDate))) %>%
  arrange(desc(InformationDate)) %>%
  group_by(EnrollmentID) %>%
  slice(1L) %>%
  ungroup() %>%
  determine_total_income() %>%
  mutate(other_income = calculated_total_income - earned_income) %>%
  select(c("EnrollmentID", "InformationDate", "IncomeFromAnySource",
           all_of(source_amount_cols), "earned_income", "other_income",
           "calculated_total_income"))

spm_4.1_4.2_4.3_dq <- spm_4.1_4.2_4.3_dq %>%
  select(HouseholdID, PersonalID, EnrollmentID, RelationshipToHoH, EntryDate,
         ExitDate, age) %>%
  inner_join(stayer_start_incomes %>%
               setNames(c("EnrollmentID", 
                          paste0("S_", 
                                 colnames(stayer_start_incomes)[2:ncol(stayer_start_incomes)]))),
             by = "EnrollmentID") %>%
  left_join(stayer_end_incomes %>%
              setNames(c("EnrollmentID", 
                         paste0("E_", colnames(stayer_end_incomes)[2:ncol(stayer_end_incomes)]))),
            by = "EnrollmentID")

########################################################
# leavers
########################################################

spm_4.4_4.5_4.6_dq <- M4_enrollment_u %>%
  filter(
    PersonalID %nin% M4_stayer_clients$PersonalID &
    ExitDate >= report_start_date & ExitDate <= report_end_date) %>%
  arrange(desc(EntryDate), EnrollmentID) %>%
  group_by(PersonalID) %>%
  slice(1L) %>%
  ungroup() %>% 
  filter(age >= 18)

leaver_exit_incomes <- IncomeBenefits %>%
  inner_join(spm_4.4_4.5_4.6_dq %>%
               select(EnrollmentID, ExitDate),
             by = "EnrollmentID") %>%
  filter(InformationDate == ExitDate &
           DataCollectionStage == 3) %>%
  group_by(EnrollmentID) %>%
  arrange(desc(InformationDate), desc(IncomeBenefitsID)) %>%
  slice(1L) %>%
  ungroup() %>%
  determine_total_income() %>%
  mutate(other_income = calculated_total_income - earned_income) %>%
  select(c("EnrollmentID", "InformationDate", "IncomeFromAnySource",
           all_of(source_amount_cols), "earned_income", "other_income",
           "calculated_total_income"))

leaver_start_incomes <- IncomeBenefits %>%
  inner_join(spm_4.4_4.5_4.6_dq %>%
               select(EnrollmentID, EntryDate),
             by = "EnrollmentID") %>%
  filter(InformationDate == EntryDate &
           DataCollectionStage == 1) %>%
  group_by(EnrollmentID) %>%
  arrange(desc(InformationDate), desc(IncomeBenefitsID)) %>%
  slice(1L) %>%
  ungroup() %>%
  determine_total_income() %>%
  mutate(other_income = calculated_total_income - earned_income) %>%
  select(c("EnrollmentID", "InformationDate", "IncomeFromAnySource",
           all_of(source_amount_cols), "earned_income", "other_income",
           "calculated_total_income"))

spm_4.4_4.5_4.6_dq <- spm_4.4_4.5_4.6_dq %>%
  select(HouseholdID, PersonalID, EnrollmentID, RelationshipToHoH, EntryDate,
         ExitDate, age) %>%
  inner_join(leaver_start_incomes %>%
               setNames(c("EnrollmentID", 
                          paste0("S_", colnames(leaver_start_incomes)[2:ncol(leaver_start_incomes)]))),
             by = "EnrollmentID") %>%
  left_join(leaver_exit_incomes %>%
              setNames(c("EnrollmentID", 
                         paste0("E_", colnames(leaver_exit_incomes)[2:ncol(leaver_start_incomes)]))),
            by = "EnrollmentID")

make_spm_4_table <- function(income_data,
                             client_type,
                             income_type) {
  
  row_header <- c(
    if_else(client_type == "leavers",
            "Universe:.Number.of.adults.who.exited.(system.leavers)",
            "Universe:.Number.of.adults.(system.stayers)"),
    paste0("Number.of.adults.",
           if_else(client_type == "leavers",
                   "who.exited.", ""),
           "with.increased.",
           income_type,
           ".income"),
    paste0("Percentage.of.adults.who.increased.",
           income_type,
           ".income")
  )
  
  income_column <- paste0(
    case_when(
      income_type == "non-employment cash" ~ "other",
      income_type == "total" ~ "calculated_total",
      TRUE ~ income_type
    ),
    "_income")
  
  start_income_column <- paste0("S_", income_column)
  later_income_column <- paste0("E_", income_column)
  
  calculations <- income_data %>%
    select(PersonalID, {{start_income_column}}, {{later_income_column}}) %>%
    summarise(total_people = n_distinct(PersonalID, na.rm = TRUE),
              people_increased = n_distinct(PersonalID[get(start_income_column)
                                                       < get(later_income_column)],
                                            na.rm = TRUE),
              percent_increased = round(people_increased / total_people * 100, 2))
  
  Current.FY <- as.numeric(calculations[1, ])
  
  cbind.data.frame(row_header, Previous.FY = NA, Current.FY, Difference = NA)
}

spm_4.1 <- make_spm_4_table(
  spm_4.1_4.2_4.3_dq,
  "stayers",
  "earned"
)

spm_4.2 <- make_spm_4_table(
  spm_4.1_4.2_4.3_dq,
  "stayers",
  "non-employment cash"
)

spm_4.3 <- make_spm_4_table(
  spm_4.1_4.2_4.3_dq,
  "stayers",
  "total"
)

spm_4.4 <- make_spm_4_table(
  spm_4.4_4.5_4.6_dq,
  "leavers",
  "earned"
)

spm_4.5 <- make_spm_4_table(
  spm_4.4_4.5_4.6_dq,
  "leavers",
  "non-employment cash"
)

spm_4.6 <- make_spm_4_table(
  spm_4.4_4.5_4.6_dq,
  "leavers",
  "total"
)

rm(list = ls()[ls() %nin% items_to_keep]) 


  


