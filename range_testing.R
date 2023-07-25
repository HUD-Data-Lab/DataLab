library(ivs)

enrollments_to_check <- active_enrollments %>%
  filter(Method5 &
           ProjectType != 1) %>%
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

{
  View(enrollments_to_check %>% 
         select(PersonalID, EnrollmentID, ProjectID, EntryDate, ExitDate, ExitDateAdj, ProjectType))
  }

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


test <- active_enrollments %>%
  filter(Method5 &
           ProjectType != 1) %>%
  full_join(bed_nights_in_report_ee_format,
            by = colnames(bed_nights_in_report_ee_format)) %>%
  mutate(ExitDateAdj = case_when(
    ProjectType == 1 ~ ExitDate %m+% days(1),
    is.na(ExitDate) | ExitDate > report_end_date ~ report_end_date %m+% days(1), 
    TRUE ~ ExitDate)) %>%
  negate_lot_blocks(.,
                    projects_to_keep = c(0, 1, 8),
                    projects_to_remove = c(3, 9, 10, 13))

test_counts <- test %>%
  mutate(days = interval(ymd(start_date),ymd(end_date))  %/% days(1)) %>%
  group_by(PersonalID) %>%
  summarise(days = sum(days)) %>%
  ungroup() %>%
  summarise(clients = n(),
            average_lot = mean(days))

negate_lot_blocks <- function(enrollment_table,
                              projects_to_keep,
                              # need to write this in
                              include_3.917 = FALSE,
                              projects_to_remove) {
  
  enrollments_to_check <- enrollment_table #%>%
    # filter(Method5 &
    #          ProjectType != 1) %>%
    # full_join(bed_nights_in_report_ee_format,
    #           by = colnames(bed_nights_in_report_ee_format)) %>%
    # mutate(ExitDateAdj = case_when(
    #   ProjectType == 1 ~ ExitDate %m+% days(1),
    #   is.na(ExitDate) | ExitDate > report_end_date ~ report_end_date %m+% days(1), 
    #   TRUE ~ ExitDate))
  
  keep_ranges <- enrollments_to_check %>%
    filter(ProjectType %in% projects_to_keep &
             EntryDate < ExitDateAdj) %>%
    mutate(range = iv(EntryDate, ExitDateAdj))
  
  delete_ranges <- enrollments_to_check %>%
    filter(ProjectType %in% projects_to_remove &
             EntryDate < ExitDateAdj &
             (EntryDate < MoveInDateAdj |
                ProjectType %nin% c(3, 9, 10, 13))) %>%
    mutate(range = iv(EntryDate, 
                      case_when(ProjectType %in% c(3, 9, 10, 13) ~ MoveInDateAdj,
                              TRUE ~ ExitDateAdj)))
  
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

checking <- keep_ranges %>%
  group_by(PersonalID) %>%
  summarise(enrollments = n()) %>%
  full_join(kept_ranges_for_clients %>%
              group_by(PersonalID) %>%
              summarise(blocks = n()),
            by = "PersonalID") %>%
  filter(enrollments != blocks)
