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

install.packages("BiocManager")
BiocManager::install("IRanges")
library(IRanges)
library(dplyr)
library(lubridate)

items_to_keep <- c(items_to_keep,
                   "spm_1a", "spm_1b",
                   do.call(paste0, expand.grid("spm_1", c("a", "b"), c("1", "2"), "_dq")))

# Helper function to validate date ranges
validate_date_ranges <- function(df, verbose = FALSE) {
  invalid_ranges <- df %>%
    filter(EntryDate >= ExitDateAdj | is.na(EntryDate) | is.na(ExitDateAdj))
  
  if (nrow(invalid_ranges) > 0 && verbose) {
    warning(paste("Found", nrow(invalid_ranges), "invalid date ranges"))
  }
  
  df %>%
    filter(EntryDate < ExitDateAdj & !is.na(EntryDate) & !is.na(ExitDateAdj))
}

# Helper function to create IRanges with date handling
create_date_ranges <- function(df, start_col, end_col) {
  df %>%
    mutate(
      start_date = as.Date(!!sym(start_col)),
      end_date = as.Date(!!sym(end_col)),
      range = IRanges(
        start = as.numeric(start_date - as.Date("1970-01-01")),
        end = as.numeric(end_date - as.Date("1970-01-01"))
      )
    ) %>%
    select(PersonalID, range, start_date, end_date)
}

# Modularized negation function with edge handling
negate_lot_blocks <- function(enrollment_table,
                              projects_to_keep,
                              projects_to_remove,
                              include_3.917 = FALSE,
                              verbose = FALSE) {
  
  if (verbose) {
    message("Starting negation process...")
    message(paste("Projects to keep:", paste(projects_to_keep, collapse = ", ")))
    message(paste("Projects to remove:", paste(projects_to_remove, collapse = ", ")))
  }
  
  # Step 1: Filter and validate enrollments to keep
  enrollments_to_keep <- enrollment_table %>%
    filter(ProjectType %in% projects_to_keep &
             (ProjectType %nin% c(3, 9, 10, 13) |
                (lh_at_entry &
                   ((EntryDate >= report_start_date &
                       EntryDate <= report_end_date) |
                      (MoveInDateAdj > report_start_date &
                         MoveInDateAdj <= report_end_date &
                         !is.na(MoveInDateAdj)) |
                      (is.na(MoveInDateAdj) &
                         !is.na(ExitDate) &
                         ExitDate >= report_start_date &
                         ExitDate <= report_end_date))))) %>%
    validate_date_ranges(verbose = verbose)
  
  # Step 2: Create keep ranges with proper move-in date handling
  keep_ranges <- enrollments_to_keep %>%
    filter(EntryDate < MoveInDateAdj | is.na(MoveInDateAdj)) %>%
    mutate(
      effective_end_date = case_when(
        is.na(MoveInDateAdj) ~ ExitDateAdj,
        TRUE ~ MoveInDateAdj
      )
    ) %>%
    create_date_ranges("EntryDate", "effective_end_date")
  
  # Step 3: Handle 3.917 data if requested
  if (include_3.917) {
    # Add street outreach and prior homelessness periods
    stays_3917 <- enrollments_to_keep %>%
      filter(ProjectType == 1) %>%
      group_by(original_enrollment_id) %>%
      summarise(end_prior = min(EntryDate), .groups = "drop") %>%
      inner_join(active_enrollments %>%
                   select(PersonalID, EnrollmentID, DateToStreetESSH),
                 by = c("original_enrollment_id" = "EnrollmentID")) %>%
      filter(!is.na(DateToStreetESSH) &
               DateToStreetESSH < end_prior &
               end_prior >= lookback_stop_date) %>%
      create_date_ranges("DateToStreetESSH", "end_prior")
    
    enrollments_3917 <- enrollments_to_keep %>%
      filter(ProjectType != 1 &
               lh_at_entry &
               EntryDate >= lookback_stop_date &
               !is.na(DateToStreetESSH) &
               DateToStreetESSH < EntryDate) %>%
      create_date_ranges("DateToStreetESSH", "EntryDate")
    
    # Combine all keep ranges
    keep_ranges <- bind_rows(keep_ranges, stays_3917, enrollments_3917)
  }
  
  # Step 4: Create removal ranges with PH case handling
  delete_ranges <- enrollment_table %>%
    filter(ProjectType %in% projects_to_remove) %>%
    validate_date_ranges(verbose = verbose) %>%
    filter(ProjectType %nin% c(3, 9, 10, 13) |
             (!is.na(MoveInDateAdj) & 
              EntryDate <= MoveInDateAdj &
              MoveInDateAdj < ExitDateAdj)) %>%
    mutate(
      effective_start_date = case_when(
        ProjectType %in% c(3, 9, 10, 13) & !is.na(MoveInDateAdj) ~ MoveInDateAdj,
        TRUE ~ EntryDate
      )
    ) %>%
    create_date_ranges("effective_start_date", "ExitDateAdj")
  
  # Step 5: Apply set difference per client using IRanges
  all_clients <- unique(c(keep_ranges$PersonalID, delete_ranges$PersonalID))
  
  result_list <- map_dfr(all_clients, function(client) {
    client_keep <- keep_ranges %>% filter(PersonalID == client)
    client_delete <- delete_ranges %>% filter(PersonalID == client)
    
    if (nrow(client_keep) == 0) return(NULL)
    
    # Combine all keep ranges for this client
    keep_iranges <- client_keep$range
    if (length(keep_iranges) > 1) {
      keep_iranges <- reduce(keep_iranges)
    }
    
    # Apply deletions if any exist
    if (nrow(client_delete) > 0) {
      delete_iranges <- client_delete$range
      if (length(delete_iranges) > 1) {
        delete_iranges <- reduce(delete_iranges)
      }
      
      # Perform set difference
      final_ranges <- setdiff(keep_iranges, delete_iranges)
    } else {
      final_ranges <- keep_iranges
    }
    
    # Convert back to date ranges
    if (length(final_ranges) > 0) {
      tibble(
        PersonalID = client,
        start_date = as.Date(start(final_ranges), origin = "1970-01-01"),
        end_date = as.Date(end(final_ranges), origin = "1970-01-01")
      )
    } else {
      NULL
    }
  })
  
  if (verbose) {
    message(paste("Negation complete. Processed", length(all_clients), "clients"))
    message(paste("Final result has", nrow(result_list), "date ranges"))
  }
  
  return(result_list)
}

#SPM1 DQ table creation
make_spm1_dq_table <- function(date_table, verbose = FALSE) {
  
  if (nrow(date_table) == 0) {
    if (verbose) warning("Empty date table provided")
    return(data.frame(PersonalID = character(0), days = numeric(0)))
  }
  
  client_start_dates <- date_table %>%
    group_by(PersonalID) %>%
    left_join(Client %>% select(PersonalID, DOB), by = "PersonalID") %>%
    mutate(
      client_end_date = max(end_date),
      client_start_date = case_when(
        client_end_date - days(365) > DOB | is.na(DOB) ~ client_end_date - days(365),
        TRUE ~ DOB
      )
    ) %>%
    filter(report_start_date < client_end_date) %>%
    ungroup() %>%
    select(PersonalID, client_start_date, client_end_date) %>%
    distinct()
  
  # Create valid date ranges using IRanges for intersection
  valid_ranges_for_clients <- date_table %>%
    inner_join(client_start_dates, by = "PersonalID") %>%
    mutate(
      # Convert to IRanges for intersection calculation
      homeless_range = IRanges(
        start = as.numeric(start_date - as.Date("1970-01-01")),
        end = as.numeric(end_date - as.Date("1970-01-01"))
      ),
      valid_range = IRanges(
        start = as.numeric(client_start_date - as.Date("1970-01-01")),
        end = as.numeric(client_end_date - as.Date("1970-01-01"))
      ),
      # Calculate intersection
      intersection = intersect(homeless_range, valid_range),
      # Convert back to days
      days = width(intersection)
    ) %>%
    filter(days > 0) %>%
    group_by(PersonalID) %>%
    summarise(days = sum(days), .groups = "drop") %>%
    left_join(client_start_dates %>%
                left_join(Client %>% select(PersonalID, DOB), by = "PersonalID") %>%
                mutate(
                  client_end_date = client_end_date - days(1),
                  client_start_date = case_when(
                    client_start_date == DOB & !is.na(DOB) ~ client_start_date,
                    TRUE ~ client_start_date - days(1)
                  )
                ) %>%
                select(-DOB),
              by = "PersonalID") %>%
    distinct()
  
  if (verbose) {
    message(paste("Created DQ table with", nrow(valid_ranges_for_clients), "clients"))
  }
  
  return(valid_ranges_for_clients)
}

# Test function for edge cases
test_overlapping_enrollments <- function() {
  # Create test data for Engrave Interweaving case
  test_data <- tibble(
    PersonalID = c(665435, 665435),
    EnrollmentID = c(828864, 830636),
    ProjectType = c(0, 3), # ES and PH-RRH
    EntryDate = as.Date(c("2021-10-25", "2021-11-10")),
    ExitDate = as.Date(c("2021-11-24", "2022-04-22")),
    ExitDateAdj = ExitDate + days(1),
    MoveInDateAdj = as.Date(c(NA, "2021-11-10")),
    lh_at_entry = c(TRUE, TRUE)
  )
  
  # Test the negation
  result <- negate_lot_blocks(
    test_data,
    projects_to_keep = c(0, 1, 8),
    projects_to_remove = c(3, 9, 10, 13),
    verbose = TRUE
  )
  
  # Calculate days
  if (nrow(result) > 0) {
    total_days <- result %>%
      mutate(days = as.numeric(end_date - start_date)) %>%
      summarise(total = sum(days)) %>%
      pull(total)
    
    message(paste("Test result: Engrave Interweaving should have", total_days, "days homeless"))
    message("Expected: 17 days (Oct 25 - Nov 10)")
  }
  
  return(result)
}

# Main SPM 1 calculations
spm_1_enrollments <- active_enrollments %>%
  filter(PersonalID %in% active_enrollments$PersonalID[Method5] &
           ProjectType != 1) %>%
  full_join(bed_nights_ee_format %>%
              filter(original_enrollment_id %in% active_enrollments$EnrollmentID),
            by = colnames(bed_nights_ee_format)[1:6]) %>%
  mutate(ExitDateAdj = case_when(
    is.na(ExitDate) | ExitDate > report_end_date ~ report_end_date + days(1), 
    ProjectType == 1 ~ ExitDate + days(1),
    TRUE ~ ExitDate
  ))

# SPM 1a1
{
  spm_1a1_dq <- spm_1_enrollments %>%
    negate_lot_blocks(.,
                      projects_to_keep = c(0, 1, 8),
                      projects_to_remove = c(2, 3, 9, 10, 13)) %>%
    make_spm1_dq_table(.)
  
  spm_1a1 <- spm_1a1_dq %>%
    summarise(clients = n(),
              average_lot = round(mean(days), 2),
              median_lot = round(median(days), 2))
}

# SPM 1a2
{
  spm_1a2_dq <- spm_1_enrollments %>%
    negate_lot_blocks(.,
                      projects_to_keep = c(0, 1, 2, 8),
                      projects_to_remove = c(3, 9, 10, 13)) %>%
    make_spm1_dq_table(.)
  
  spm_1a2 <- spm_1a2_dq %>%
    summarise(clients = n(),
              average_lot = round(mean(days), 2),
              median_lot = round(median(days), 2))
}

spm_1a_row_headers <- c(
  "Persons.in.ES-EE,.ES-NbN,.and.SH",
  "Persons.in.ES-EE,.ES-NbN,.SH,.and.TH"
)

spm_1a_data <- union(spm_1a1, spm_1a2)

spm_1a <- 
  data.frame(spm_1a_row_headers,
             "Previous.FY.Universe.Persons" = NA,
             "Current.FY.Universe.Persons" = spm_1a_data$clients,
             "Previous FY Average LOT Experiencing Homelessness" = NA,
             "Current FY Average LOT Experiencing Homelessness" = spm_1a_data$average_lot,
             "Difference.In.Average" = NA,
             "Previous FY Median LOT Experiencing Homelessness" = NA,
             "Current FY Median LOT Experiencing Homelessness" = spm_1a_data$median_lot,
             "Difference.In.Median" = NA)

# SPM 1b1
{
  include_after_negation_1b1 <- spm_1_enrollments %>%
    negate_lot_blocks(.,
                      projects_to_keep = c(0, 1, 3, 8, 9, 10, 13),
                      projects_to_remove = c(2, 3, 9, 10, 13)) %>%
    make_spm1_dq_table(.) %>%
    select(PersonalID) %>%
    distinct()
  
  spm_1b1_dq <- spm_1_enrollments %>%
    filter(PersonalID %in% include_after_negation_1b1$PersonalID) %>%
    negate_lot_blocks(.,
                      projects_to_keep = c(0, 1, 3, 8, 9, 10, 13),
                      projects_to_remove = c(2, 3, 9, 10, 13),
                      include_3.917 = TRUE) %>%
    make_spm1_dq_table(.)
  
  spm_1b1 <- spm_1b1_dq %>%
    summarise(clients = n(),
              average_lot = round(mean(days), 2),
              median_lot = round(median(days), 2))
}

# SPM 1b2
{
  include_after_negation_1b2 <- spm_1_enrollments %>%
    negate_lot_blocks(.,
                      projects_to_keep = c(0, 1, 2, 3, 8, 9, 10, 13),
                      projects_to_remove = c(3, 9, 10, 13)) %>%
    make_spm1_dq_table(.) %>%
    select(PersonalID) %>%
    distinct()
  
  spm_1b2_dq <- spm_1_enrollments %>%
    filter(PersonalID %in% include_after_negation_1b2$PersonalID) %>%
    negate_lot_blocks(.,
                      projects_to_keep = c(0, 1, 2, 3, 8, 9, 10, 13),
                      projects_to_remove = c(3, 9, 10, 13),
                      include_3.917 = TRUE) %>%
    make_spm1_dq_table(.)
  
  spm_1b2 <- spm_1b2_dq %>%
    summarise(clients = n(),
              average_lot = round(mean(days), 2),
              median_lot = round(median(days), 2))
}

spm_1b_row_headers <- c(
  "Persons.in.ES-EE,.ES-NbN,.SH,.and.PH",
  "Persons.in.ES-EE,.ES-NbN,.SH,.TH,.and.PH"
)

spm_1b_data <- union(spm_1b1, spm_1b2)

spm_1b <- 
  data.frame(spm_1b_row_headers,
             "Previous.FY.Universe.Persons" = NA,
             "Current.FY.Universe.Persons" = spm_1b_data$clients,
             "Previous FY Average LOT Experiencing Homelessness" = NA,
             "Current FY Average LOT Experiencing Homelessness" = spm_1b_data$average_lot,
             "Difference.In.Average" = NA,
             "Previous FY Median LOT Experiencing Homelessness" = NA,
             "Current FY Median LOT Experiencing Homelessness" = spm_1b_data$median_lot,
             "Difference.In.Median" = NA)

# Optional debug: Run edge case test
# test_result <- test_overlapping_enrollments()

rm(list = ls()[ls() %nin% items_to_keep])
