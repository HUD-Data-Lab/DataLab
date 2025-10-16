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
                   "spm_1a", "spm_1b",
                   do.call(paste0, expand.grid("spm_1", c("a", "b"), c("1", "2"), "_dq")))

negate_lot_blocks <- function(enrollment_table,
                              projects_to_keep,
                              projects_to_remove,
                              include_3.917 = FALSE) {
  
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
                         ExitDate <= report_end_date)))))
  
  keep_ranges <- enrollments_to_keep %>%
    filter(
      EntryDate < ExitDateAdj &
        ##  QA this pair of conditions--if someone exits on the report start date,
        ##  then by definition wouldn't they have no bed night in the report range?
        ##  and following that up, what if they immediately re-enrolled? in that 
        ##  case I guess we'd definitely want to keep this, because it would be 
        ##  contiguous with another stay
        # (ExitDateAdj > report_start_date |
        #    !is.na(original_enrollment_id)) &
        (EntryDate < MoveInDateAdj |
           is.na(MoveInDateAdj))) %>%
    mutate(range = iv(EntryDate, 
                      case_when(
                        is.na(MoveInDateAdj) ~ ExitDateAdj,
                        TRUE ~ MoveInDateAdj))) %>%
    select(PersonalID, range)
  
  if(include_3.917) {
    
    stays <- enrollments_to_keep %>%
      filter(ProjectType == 1) %>%
      group_by(original_enrollment_id) %>%
      summarise(end_prior = min(EntryDate)) %>%
      ungroup() %>%
      inner_join(active_enrollments %>%
                   select(PersonalID, EnrollmentID, DateToStreetESSH),
                 by = c("original_enrollment_id" = "EnrollmentID")) %>%
      filter(!is.na(DateToStreetESSH) &
               DateToStreetESSH < end_prior &
               end_prior >= lookback_stop_date) %>%
      mutate(range = iv(DateToStreetESSH, 
                        end_prior)) %>%
      select(PersonalID, range)
    
    enrollments <- enrollments_to_keep %>%
      filter(ProjectType != 1 &
               lh_at_entry &
               EntryDate >= lookback_stop_date &
               !is.na(DateToStreetESSH) &
               DateToStreetESSH < EntryDate) %>%
      mutate(range = iv(DateToStreetESSH, 
                        EntryDate)) %>%
      select(PersonalID, range)
    
    keep_ranges <- keep_ranges %>%
      full_join(stays,
                by = colnames(keep_ranges)) %>%
      full_join(enrollments,
                by = colnames(keep_ranges))
    
  }
  
  delete_ranges <- enrollment_table %>%
    filter(ProjectType %in% projects_to_remove &
             EntryDate < ExitDateAdj &
             (ProjectType %nin% c(3, 9, 10, 13) |
                (EntryDate <= MoveInDateAdj &
                   MoveInDateAdj < ExitDateAdj))) %>%
    mutate(range = iv(case_when(ProjectType %in% c(3, 9, 10, 13) ~ MoveInDateAdj,
                                TRUE ~ EntryDate), 
                      ExitDateAdj))
  
  for (client in unique(keep_ranges$PersonalID)) {
    
    hold <- iv_set_difference(keep_ranges$range[keep_ranges$PersonalID == client],
                              delete_ranges$range[delete_ranges$PersonalID == client])
    
    date_ranges <- data.frame(
      PersonalID = rep(client, length(as.vector(hold))),
      keep_range = as.vector(hold)) %>%
      mutate(start_date = iv_start(keep_range),
             end_date = iv_end(keep_range)) %>%
      select(-keep_range)
    
    if (client == unique(keep_ranges$PersonalID)[1]) {
      kept_ranges_for_clients <- date_ranges
    } else {
      kept_ranges_for_clients <- kept_ranges_for_clients %>%
        union(date_ranges)
    }
  }
  kept_ranges_for_clients
}

make_spm1_dq_table <- function(date_table) {
  
  client_start_dates <- date_table %>%
    group_by(PersonalID) %>%
    left_join(Client %>%
                select(PersonalID, DOB),
              by = "PersonalID") %>%
    mutate(client_end_date = max(end_date),
           client_start_date = case_when(
             client_end_date - days(365) > DOB |
               is.na(DOB) ~ client_end_date - days(365),
             TRUE ~ DOB)) %>%
    filter(report_start_date < client_end_date &
             # added 10/10/25 to catch a person who was enrolled prior to the 
             # end of the report period even though they weren't born until
             # the next month
             client_start_date < client_end_date) %>%
    ungroup() %>%
    select(PersonalID, client_start_date, client_end_date) %>%
    distinct() 
  
  client_start_dates_range <- client_start_dates %>%
    mutate(range = iv(client_start_date, client_end_date))
  
  range_date_table <- date_table %>%
    mutate(range = iv(start_date, end_date))
  
  for (client in unique(client_start_dates_range$PersonalID)) {
    
    valid_date_range <- client_start_dates_range$range[client_start_dates_range$PersonalID == client]
    time_experiencing_homelessness <- range_date_table$range[range_date_table$PersonalID == client]
    
    overlap <- iv_locate_overlaps(time_experiencing_homelessness,
                                  valid_date_range,
                                  no_match = "drop")
    
    hold <- iv_align(time_experiencing_homelessness, 
                     valid_date_range, 
                     locations = overlap) 
    
    date_ranges <- data.frame(
      PersonalID = rep(client, length(as.vector(hold$needles))),
      keep_range = as.vector(hold$needles)) %>%
      mutate(start_date = iv_start(keep_range),
             end_date = iv_end(keep_range)) %>%
      select(-keep_range)
    
    if (client == unique(client_start_dates$PersonalID)[1]) {
      valid_ranges_for_clients <- date_ranges
    } else {
      valid_ranges_for_clients <- valid_ranges_for_clients %>%
        union(date_ranges)
    }
  }
  
  valid_ranges_for_clients %>%
    mutate(days = interval(ymd(start_date),
                           ymd(end_date)) %/% days(1)) %>%
    group_by(PersonalID) %>%
    summarise(days = sum(days)) %>%
    ungroup() %>%
    left_join(client_start_dates %>%
                left_join(Client %>%
                            select(PersonalID, DOB),
                          by = "PersonalID") %>%
                mutate(client_end_date = client_end_date %m-% days(1),
                       client_start_date = case_when(
                         client_start_date == DOB &
                           !is.na(DOB) ~ client_start_date,
                         TRUE ~ client_start_date %m-% days(1))) %>%
                select(-DOB),
              by = "PersonalID") %>%
    distinct()
}



spm_1_enrollments <- active_enrollments %>%
  filter(PersonalID %in% active_enrollments$PersonalID[Method5] &
           ProjectType != 1) %>%
  full_join(bed_nights_ee_format %>%
              filter(original_enrollment_id %in% active_enrollments$EnrollmentID),
            by = colnames(bed_nights_ee_format)[1:6]) %>%
  mutate(ExitDateAdj = case_when(
    is.na(ExitDate) | ExitDate > report_end_date ~ report_end_date %m+% days(1), 
    ProjectType == 1 ~ ExitDate %m+% days(1),
    TRUE ~ ExitDate)) 

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

rm(list = ls()[ls() %nin% items_to_keep]) 
