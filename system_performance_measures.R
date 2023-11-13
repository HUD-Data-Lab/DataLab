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
lookback_stop_date <- ymd("2014-10-1") #switch from 2013-10-1 (old Kit) to 2014-10-1 (New Kit)

system_performance_measures <- TRUE

# subscripts
source("local_functions.R")
source("datalab_functions.R")
source("DataLab_Lists.R")

# save_to <- choose.dir()
file_source <- file.choose()

for (file in names(hmis_csvs_fy24)){
  
  data <- read_csv(unzip(file_source, paste0(file, ".csv")),
                   col_types = get(file, hmis_csvs_fy24))
  
  if (exists(file)) {
    data <- get(file) %>%
      full_join(data, by = intersect(colnames(get(file)),
                                     colnames(data)))
  } 
  
  assign(file, data)
  
  file.remove(paste0(file, ".csv"))
}

relevant_CoC <- "XX-501"

# Is there a reason this needs to be here and not above with other params?
report_start_date <- lookback_stop_date %m+% years(7)
report_end_date <- report_start_date %m+% years(1) %m-% days(1)

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

hmids <- Enrollment %>%
  filter(RelationshipToHoH == 1 &
           ProjectID %in% Project$ProjectID[Project$ProjectType %in% c(3, 9, 10, 13)]) %>%
  group_by(HouseholdID) %>%
  arrange(EntryDate) %>%
  slice(1L) %>%
  ungroup() %>%
  select(HouseholdID, MoveInDate) %>%
  rename(HoH_HMID = MoveInDate)

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
  left_join(Client %>%
              select(PersonalID, DOB),
            by = "PersonalID") %>%
  left_join(hmids,
            by = "HouseholdID") %>%
  mutate(MoveInDateAdj = case_when(
    !is.na(HoH_HMID) &
      HoH_HMID >= DOB &
      HoH_HMID >= EntryDate &
      (HoH_HMID <= ExitDate |
         is.na(ExitDate)) ~ HoH_HMID,
    !is.na(HoH_HMID) &
      (HoH_HMID <= ExitDate |
         is.na(ExitDate)) ~ EntryDate
  ))

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
         ExitDate = DateProvided) %>%
  select(EnrollmentID, PersonalID, EntryDate, ExitDate,
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

#  Measure 1a
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
    create_lot_blocks() %>%
    left_join(df_for_negation %>%
                filter(ProjectType == 2) %>%
                create_lot_blocks() %>%
                rename(n_EntryDate = EntryDate,
                       n_Method5_ExitDate = Method5_ExitDate),
              by = "PersonalID",
              relationship = "many-to-many") %>%
    filter(n_EntryDate <= Method5_ExitDate &
             n_Method5_ExitDate >= EntryDate)
  
  hold <- negated %>%
    select(lot_block.x, EntryDate, Method5_ExitDate,
           n_EntryDate, n_Method5_ExitDate) #%>%
    # ungroup() %>%
    
  
  hold_3 <- as.data.frame(melt(as.data.table(hold), 
                 id = c("lot_block.x", "EntryDate", "Method5_ExitDate"))) %>%
    filter(value >= EntryDate &
             (value < Method5_ExitDate |
                (value == Method5_ExitDate &
                   variable == "n_EntryDate")))
  
  hold_4 <- hold_3 %>%
    left_join(hold_3 %>%
                group_by(lot_block.x) %>%
                summarise(rows = n()),
              by = "lot_block.x")
  
  single_row_adjustments <- hold_4 %>%
    filter(rows == 1) %>%
    mutate(EntryDate = if_else(
      variable == "n_Method5_ExitDate", value,
      EntryDate),
      Method5_ExitDate = if_else(
        variable == "n_EntryDate", value,
        Method5_ExitDate))
  
  multi_row_adjustments <- as.data.frame(melt(as.data.table(hold %>%
                                                              filter(lot_block.x %nin% single_row_adjustments$lot_block.x)), 
                                              id = c("lot_block.x", "EntryDate", "Method5_ExitDate")))
  
  test <- hold %>%
    select(lot_block.x, EntryDate, Method5_ExitDate) %>%
    left_join(as.data.frame(melt(as.data.table(hold), 
                                 id = c("lot_block.x"))),
              by = "lot_block.x") %>%
    filter(variable %in% c("EntryDate", "Method5_ExitDate") |
             (value >= EntryDate &
                (value < Method5_ExitDate |
                   (value == Method5_ExitDate &
                      variable == "n_EntryDate"))))
  
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
                Method5_ExitDate = max(Method5_ExitDate)) %>%
      ungroup()
  }
  
  View(Q1a_line1_detail %>%filter(PersonalID == 670613))
  
  
  
  test_full <- enrollment_data %>%
    filter(ProjectType %in% c(0, 8,
                              2) &
             # is this right?
             EntryDate < Method5_ExitDate) %>%
    # ...and the way I add it here
    full_join(bed_nights_in_report_ee_format,
              by = colnames(bed_nights_in_report_ee_format)) %>%
    # unnecessary, added for algorithm QA
    select(colnames(bed_nights_in_report_ee_format))
  
  
  test_blocks <- test_full %>%
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
    )
  
  }


