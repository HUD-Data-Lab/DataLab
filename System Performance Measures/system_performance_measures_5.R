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

library(dplyr)
library(lubridate)

items_to_keep <- c(items_to_keep,
                   do.call(paste0, expand.grid("spm_5.", c("1", "2"), c("_dq", ""))))

# Measure 5 - Number of Persons who Become Homeless for the First Time ----

#### OBSERVATIONS
### With report_start_date coded to lookback_stop_date + 7 years, never will hit lookback_stop_date

#### SCRIPT START

# Helper function for debugging missing clients
debug_missing_clients <- function(missing_ids, universe_data, verbose = FALSE) {
  if (verbose && length(missing_ids) > 0) {
    message("=== Debugging Missing Clients ===")
    for (id in missing_ids) {
      client_enrollments <- active_enrollments %>%
        filter(PersonalID == id) %>%
        select(PersonalID, EnrollmentID, ProjectType, EntryDate, ExitDate) %>%
        arrange(EntryDate)
      
      message(paste("PersonalID:", id))
      message("Enrollments:")
      print(client_enrollments)
      
      # Check if client appears in universe
      in_universe <- id %in% universe_data$PersonalID
      message(paste("In universe:", in_universe))
      message("---")
    }
    message("=== End Debugging ===")
  }
}

## define report range universes ----

### 5.1 (ES, SH, TH) ----
Q5M1_report <- active_enrollments %>%
  filter(ProjectType %in% c(0, 1, 2, 8) & 
           EntryDate >= report_start_date & 
           EntryDate <= report_end_date) %>%
  group_by(PersonalID) %>%
  arrange(EntryDate, EnrollmentID) %>% 
  slice(1L) %>%
  mutate(
    client_lookbackdate = pmax(EntryDate - days(730), lookback_stop_date)) %>% 
  ungroup() %>%
  rename("client_startdate" = "EntryDate") %>%
  select(PersonalID, client_startdate, client_lookbackdate)

### 5.2 (ES, SH, TH, PH) ----
Q5M2_report <- active_enrollments %>%
  filter(ProjectType %in% c(0, 1, 2, 8, 3, 9, 10, 13) & 
           EntryDate >= report_start_date & 
           EntryDate <= report_end_date) %>%
  # Don't deduplicate at PersonalID level yet - we need ALL entries
  # that fall within the report period for proper lookback calculation
  mutate(
    client_lookbackdate = pmax(EntryDate - days(730), lookback_stop_date)
  ) %>% 
  rename("client_startdate" = "EntryDate") %>%
  select(PersonalID, EnrollmentID, ProjectType, client_startdate, client_lookbackdate)

# Debug the specific missing clients
missing_client_ids <- c(398555, 610622, 610623, 635887, 635888, 683471, 697596, 697597, 699471, 702806)

# Optional debugging - set to TRUE to enable
debug_mode <- FALSE
if (debug_mode) {
  debug_missing_clients(missing_client_ids, Q5M2_report, verbose = TRUE)
}

## calculations ----

# Lookback join logic with error handling for overlapping enrollment
create_lookback_join <- function(report_data, enrollment_source) {
  report_data %>%
    left_join(
      enrollment_source %>%
        mutate(
          capped_exit_date = if_else(is.na(ExitDate), 
                                     report_end_date,
                                     ExitDate)
        ) %>%
        filter(ProjectType %in% c(0, 1, 2, 8, 3, 9, 10, 13)),
      join_by(PersonalID,
              client_startdate > EntryDate,
              client_lookbackdate <= capped_exit_date),
      relationship = "many-to-many"  # Allow multiple matches per client
    )
}

spm_5.1_dq <- create_lookback_join(Q5M1_report, enrollment_data)

spm_5.2_dq <- create_lookback_join(Q5M2_report, enrollment_data)

### counts of report persons ----
Q5M1_C2 <- Q5M1_report %>%
  summarise(people = n_distinct(PersonalID)) %>%
  .$people

Q5M2_C2 <- Q5M2_report %>%
  summarise(people = n_distinct(PersonalID)) %>%
  .$people

### counts of report persons in lookback ----
Q5M1_C3 <- spm_5.1_dq %>%
  filter(!is.na(EntryDate) &
           (is.na(ExitDate) | 
              ExitDate >= client_lookbackdate)) %>% 
  summarise(people = n_distinct(PersonalID)) %>%
  .$people

Q5M2_C3 <- spm_5.2_dq %>%
  filter(!is.na(EntryDate) &
           (is.na(ExitDate) | 
              ExitDate >= client_lookbackdate)) %>% 
  summarise(people = n_distinct(PersonalID)) %>%
  .$people

### counts of new report persons (not in lookback) ----
Q5M1_C4 <- Q5M1_C2 - Q5M1_C3
Q5M2_C4 <- Q5M2_C2 - Q5M2_C3

# Additional validation for SPM 5.2
validate_spm5_results <- function() {
  message("=== SPM 5 Validation ===")
  message(paste("SPM 5.1 Universe:", Q5M1_C2))
  message(paste("SPM 5.1 In Lookback:", Q5M1_C3))
  message(paste("SPM 5.1 First Time:", Q5M1_C4))
  message("---")
  message(paste("SPM 5.2 Universe:", Q5M2_C2))
  message(paste("SPM 5.2 In Lookback:", Q5M2_C3))
  message(paste("SPM 5.2 First Time:", Q5M2_C4))
  
  # Check if previously missing clients are now included
  missing_now_included <- sum(missing_client_ids %in% Q5M2_report$PersonalID)
  message(paste("Previously missing clients now included:", missing_now_included, "out of", length(missing_client_ids)))
  
  if (missing_now_included < length(missing_client_ids)) {
    still_missing <- missing_client_ids[!missing_client_ids %in% Q5M2_report$PersonalID]
    message(paste("Still missing PersonalIDs:", paste(still_missing, collapse = ", ")))
  }
  message("=== End Validation ===")
}

# Uncomment to run validation
# validate_spm5_results()

## build output tables ----

### row headers ----
Q5M1_Col_A <- c(
  "Universe: Person with entries into ES-EE, ES-NbN, SH, or TH during the reporting period.",
  "Of persons above, count those who were in ES-EE, ES-NbN, SH, TH, or any PH within 24 months prior to their start during the reporting year.",
  "Of persons above, count those who did not have entries in ES-EE, ES-NbN, SH, TH or PH in the previous 24 months. (i.e. number of persons experiencing homelessness for the first time)"
)

Q5M2_Col_A <- c(
  "Universe: Person with entries into ES-EE, ES-NbN, SH, TH, or PH during the reporting period.",
  "Of persons above, count those who were in ES-EE, ES-NbN, SH, TH, or any PH within 24 months prior to their start during the reporting year.",
  "Of persons above, count those who did not have entries in ES-EE, ES-NbN, SH, TH or PH in the previous 24 months. (i.e. number of persons experiencing homelessness for the first time)"
)

### empty columns ----
Previous.FY <- as.character(c("", "", ""))
Difference <- as.character(c("", "", ""))

### measure value columns ----
Current.FY.5.1 <- as.character(c(Q5M1_C2, Q5M1_C3, Q5M1_C4))
Current.FY.5.2 <- as.character(c(Q5M2_C2, Q5M2_C3, Q5M2_C4))

### table assembly ----
spm_5.1 <- data.frame(cbind(Q5M1_Col_A, Previous.FY, Current.FY.5.1, Difference))
spm_5.2 <- data.frame(cbind(Q5M2_Col_A, Previous.FY, Current.FY.5.2, Difference))

# Debugging function for specific client analysis
analyze_specific_client <- function(personal_id, verbose = TRUE) {
  if (!verbose) return(NULL)
  
  message(paste("=== Analyzing PersonalID:", personal_id, "==="))
  
  # Get all enrollments for this client
  client_enrollments <- active_enrollments %>%
    filter(PersonalID == personal_id) %>%
    arrange(EntryDate) %>%
    select(PersonalID, EnrollmentID, ProjectType, EntryDate, ExitDate, MoveInDate)
  
  message("All enrollments:")
  print(client_enrollments)
  
  # Check if in report period
  report_period_enrollments <- client_enrollments %>%
    filter(EntryDate >= report_start_date & EntryDate <= report_end_date)
  
  message("Enrollments in report period:")
  print(report_period_enrollments)
  
  # Check if in SPM 5.2 universe
  in_universe <- personal_id %in% Q5M2_report$PersonalID
  message(paste("In SPM 5.2 universe:", in_universe))
  
  if (in_universe) {
    universe_record <- Q5M2_report %>% filter(PersonalID == personal_id)
    message("Universe record:")
    print(universe_record)
  }
  
  message("=== End Analysis ===")
}

# Function to analyze all missing clients
analyze_all_missing_clients <- function() {
  for (id in missing_client_ids) {
    analyze_specific_client(id, verbose = TRUE)
  }
}

# Uncomment to analyze specific missing clients
# analyze_all_missing_clients()

rm(list = ls()[ls() %nin% items_to_keep])
