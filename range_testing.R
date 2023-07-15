library(ivs)

enrollments_to_check <- method_5_active_enrollments %>%
  full_join(bed_nights_in_report_ee_format,
            by = colnames(bed_nights_in_report_ee_format)) %>%
  mutate(ExitDateAdj = case_when(
    ProjectType == 1 ~ ExitDate %m+% days(1),
    is.na(ExitDate) | ExitDate > report_end_date ~ report_end_date %m+% days(1), 
    TRUE ~ ExitDate))

keep_ranges <- enrollments_to_check %>%
  filter(ProjectType %in% c(0, 1, 8) &
           EntryDate < ExitDateAdj) %>%
  mutate(range = iv(EntryDate, ExitDateAdj))

delete_ranges <- enrollments_to_check %>%
  filter(ProjectType == 2 &
           EntryDate < ExitDateAdj) %>%
  mutate(range = iv(EntryDate, ExitDateAdj))

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


test <- method_5_active_enrollments %>%
  full_join(bed_nights_in_report_ee_format,
            by = colnames(bed_nights_in_report_ee_format)) %>%
  negate_lot_blocks(.,
                    projects_to_keep = c(0, 1, 8),
                    projects_to_remove = c(3, 9, 10, 13),
                    require_hmid_to_remove = TRUE)

negate_lot_blocks <- function(enrollment_table,
                              projects_to_keep,
                              # need to write this in
                              include_3.917 = FALSE,
                              projects_to_remove,
                              require_hmid_to_remove = FALSE) {
  
  enrollments_to_check <- enrollment_table %>%
    mutate(ExitDateAdj = case_when(
      ProjectType == 1 ~ ExitDate %m+% days(1),
      is.na(ExitDate) | ExitDate > report_end_date ~ report_end_date %m+% days(1), 
      TRUE ~ ExitDate))
  
  keep_ranges <- enrollments_to_check %>%
    filter(ProjectType %in% projects_to_keep &
             EntryDate < ExitDateAdj) %>%
    mutate(range = iv(EntryDate, ExitDateAdj))
  
  delete_ranges <- enrollments_to_check %>%
    filter(ProjectType %in% projects_to_remove &
             EntryDate < ExitDateAdj &
             (EntryDate < MoveInDateAdj |
                require_hmid_to_remove == FALSE)) %>%
    mutate(range = iv(EntryDate, 
                      case_when(require_hmid_to_remove ~ MoveInDateAdj,
                              TRUE ~ ExitDateAdj)))
  
  
}
