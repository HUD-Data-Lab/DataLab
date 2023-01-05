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

generate_new_kits <- TRUE

source("DataLab.R")

items_to_keep <- c("items_to_keep", ls())
hold_ProjectCoC <- ProjectCoC

# this is for testing variations in CoC code in test kit data,
# commented out for QA
# ProjectCoC <- ProjectCoC %>%
#   mutate(CoCCode = case_when(
#     ProjectID %in% c(340, 780, 1428) ~ "XX-500",
#     ProjectID == 1615 ~ "XX-502",
#     # ProjectID %in% c(340, 780, 1647) ~ "XX-501",
#     TRUE ~ CoCCode)) %>%
#   union(ProjectCoC %>%
#           filter(ProjectID == 1428))


multi_CoC_projects <- ProjectCoC %>%
  mutate(relevant_to_CoC = relevant_CoC == CoCCode) %>%
  group_by(ProjectID) %>%
  summarise(num_of_CoCs = uniqueN(CoCCode),
            relevant_to_CoC = max(relevant_to_CoC)) %>%
  ungroup()


enrollment_coc_plus <- EnrollmentCoC %>%
  left_join(multi_CoC_projects, by = "ProjectID") %>%
  # confirmed by Fran 12.21.22, issue #2
  mutate(filter_CoC_code = case_when(
    relevant_to_CoC & num_of_CoCs == 1 ~ relevant_CoC,
    num_of_CoCs > 1 ~ CoCCode))


relevant_assessments <- Assessment %>%
  inner_join(enrollment_coc_plus %>%
              select(EnrollmentID, InformationDate, filter_CoC_code,
                     HouseholdID), 
            by = "EnrollmentID") %>%
  filter(AssessmentDate >= report_start_date &
           AssessmentDate <= report_end_date & 
           (InformationDate <= AssessmentDate |
              is.na(InformationDate))) %>%
  group_by(AssessmentID) %>%
  arrange(desc(InformationDate)) %>%
  slice(1L) %>%
  ungroup() %>%
  # okay to filter events and assessments prior to
  # identifying which one is more recent
  # verified by Fran 12.21.22, issue #3
  filter(filter_CoC_code == relevant_CoC)


relevant_events <- Event %>%
  inner_join(enrollment_coc_plus %>%
              select(EnrollmentID, InformationDate, filter_CoC_code,
                     HouseholdID), 
            by = "EnrollmentID") %>%
  filter(EventDate >= report_start_date &
           EventDate <= report_end_date + days(90) &
           (InformationDate <= EventDate |
              is.na(InformationDate))) %>%
  group_by(EventID) %>%
  arrange(desc(InformationDate)) %>%
  slice(1L) %>%
  ungroup() %>%
  filter(filter_CoC_code == relevant_CoC)


CE_element_projects <- Project %>%
  inner_join(Enrollment %>%
               filter(EnrollmentID %in%
                        # okay to filter projects based on whether
                        # they contain CE data in the report period
                        # verified by Fran 12.21.22, issue #1 
                        union(relevant_assessments$EnrollmentID, 
                              relevant_events$EnrollmentID)) %>%
               select(ProjectID) %>%
               distinct(),
             by = "ProjectID") %>%
  filter(ContinuumProject == 1)


# enrollment_CoC_over_time <- Enrollment %>%
#   select(EnrollmentID, HouseholdID, EntryDate) %>%
#   left_join(Exit %>%
#               select(EnrollmentID, ExitDate),
#             by = "EnrollmentID") %>%
#   inner_join(EnrollmentCoC %>%
#                select(HouseholdID, CoCCode, InformationDate), 
#              by = "HouseholdID") %>%
#   filter(CoCCode == relevant_CoC &
#            InformationDate >= EntryDate &
#            (InformationDate <= ExitDate |
#               is.na(ExitDate))) %>%
#   select(-c(EntryDate, ExitDate))

most_recent_assessment <- relevant_assessments %>%
  group_by(PersonalID) %>%
  arrange(desc(AssessmentDate)) %>%
  slice(1L) %>%
  ungroup()
  

assessment_and_event_dates <- relevant_events %>%
  select(HouseholdID, EventDate) %>%
  rename(activity_date = EventDate) %>%
  union(relevant_assessments %>%
          select(HouseholdID, AssessmentDate) %>%
          rename(activity_date = AssessmentDate)) %>%
  distinct()


enrollment_data <- Enrollment %>%
  left_join(Exit %>%
              select(-PersonalID),
            by = "EnrollmentID") %>%
  inner_join(CE_element_projects %>%
               select(ProjectID, ProjectType, TrackingMethod, ProjectName),
             by = "ProjectID") 


enrollment_recent_assessment <- enrollment_data %>%
  inner_join(most_recent_assessment %>%
               select(HouseholdID, AssessmentDate, AssessmentType,
                      PrioritizationStatus) %>%
               group_by(HouseholdID) %>%
               arrange(desc(AssessmentDate)) %>%
               slice(1L) %>%
               ungroup(),
             #  okay to use most recent assessment for any household 
             #  member to determine the date of the most recent assessment,
             #  not restricted to HoH
             #  verified by Fran 12.21.22, issue #4
             by = "HouseholdID") %>%
  filter(AssessmentDate >= EntryDate &
           (AssessmentDate <= ExitDate |
              is.na(ExitDate))) 


# get additional client information (age for reporting)
client_plus <- add_client_info(enrollment_recent_assessment)
  
# okay to use date of assessment to determine  
# household type for questions 7-9d
# verified by Fran 12.21.22, issue #5
household_info <- get_household_info(enrollment_recent_assessment,
                                     return_type = "household")

enrollment_recent_assessment <- enrollment_recent_assessment %>%
  left_join(client_plus, by = "PersonalID") %>%
  left_join(household_info, by = "HouseholdID") %>%
  mutate(chronic = NA)

# Q4a
{
  Q4a_detail <- enrollment_data %>%
    select(c(all_of(ce_detail_columns[ce_detail_columns %nin% c("household_type",
                                                                "AssessmentDate")]),
             ProjectID)) %>%
    left_join(assessment_and_event_dates %>%
                filter(activity_date <= report_end_date), 
              by = "HouseholdID") %>%
    group_by(EnrollmentID) %>%
    mutate(active_at_event = !is.na(activity_date) &
             activity_date >= EntryDate &
             (activity_date <= ExitDate |
                is.na(ExitDate)),
           active_at_event = max(active_at_event)) %>%
    ungroup() %>%
    filter(active_at_event == 1)
    
  Q4a <- program_information_table(Q4a_detail$ProjectID,
                                   Q4a_detail)
  }


# Q5
{
  dq_recent_assessment <- enrollment_recent_assessment %>%
    filter(ProjectType != 4 |
             (!is.na(DateOfEngagement) &
                DateOfEngagement <= report_end_date))
  
  Q5a_detail <- enrollment_recent_assessment %>%
    select(c(all_of(ce_detail_columns),
             all_of(demographic_detail_columns))) %>%
    mutate(IncludedInDQ = EnrollmentID %in% dq_recent_assessment$EnrollmentID)
  
  Q5a <- create_summary_table(dq_recent_assessment, "Count.of.Clients.for.DQ") %>%
    left_join(create_summary_table(enrollment_recent_assessment, "Count.of.Clients"), 
              by = "Group") %>%
    rename(Category = Group)
  
  Q5a[c(5:9, 11, 13, 16), 2:3] <- NA
}


# Q6
{
  Q6a_data <- create_dq_Q1(dq_recent_assessment)
  Q6a <- Q6a_data[[1]]
  Q6a_detail <- Q6a_data[[2]]
}


# Q7a
{
  Q7a_detail <- enrollment_recent_assessment %>%
    select(all_of(ce_detail_columns),
           all_of(demographic_detail_columns))
  
  Q7a_all <- Q7a_detail %>%
    mutate(client_group = "Total") %>%
    return_household_groups(., client_group, c("Total")) 
  
  Q7a_moved_in <- Q7a_all[0, ]
  Q7a_moved_in[1, 1] <- "For PSH & RRH - the total persons served who moved into housing" 
  
  Q7a <- Q7a_detail %>%
    return_household_groups(., age_group, age_groups) %>%
    rename(client_group = age_group) %>%
    union(Q7a_all) %>%
    union(Q7a_moved_in)
}


# Q8
{
  Q8a_data <- households_served_table(enrollment_recent_assessment)
  Q8a <- Q8a_data[[1]]
  Q8a_detail <- Q8a_data[[2]]
  Q8a[2, 2:6] <- NA
}


# Q9
# Q9a
{
  Q9a_detail <- enrollment_recent_assessment %>%
    select(all_of(ce_detail_columns), AssessmentType) %>%
    mutate(assessment_type = case_when(AssessmentType == 1 ~ "Phone",
                                       AssessmentType == 2 ~ "Virtual",
                                       AssessmentType == 3 ~ "In-person")) 
  
  Q9a <- Q9a_detail %>%
    filter(RelationshipToHoH == 1) %>%
    return_household_groups(., assessment_type, 
                            c("Phone", "Virtual", "In-person")) %>%
    adorn_totals("row") %>%
    mutate(assessment_type = case_when(
      assessment_type == "Total" ~ "Total Households Assessed",
      TRUE ~ assessment_type))
}

# Q9b
{
  Q9b_detail <- enrollment_recent_assessment %>%
    select(all_of(ce_detail_columns), PrioritizationStatus) %>%
    mutate(prioritization_status = case_when(PrioritizationStatus == 1 ~ "Placed on Prioritization List (Prioritized)",
                                       PrioritizationStatus == 2 ~ "Not Placed on Prioritization List")) 
  
  Q9b <- Q9b_detail %>%
    filter(RelationshipToHoH == 1) %>%
    return_household_groups(., prioritization_status, 
                            c("Placed on Prioritization List (Prioritized)", 
                              "Not Placed on Prioritization List")) %>%
    adorn_totals("row") %>%
    untabyl() %>%
    mutate(prioritization_status = case_when(
      prioritization_status == "Total" ~ "Percent of Assessed Prioritized Of the total HH Assessed report the percent of those placed on the prioritization list",
      TRUE ~ prioritization_status))
  
  Q9b[3, 2:6] <- Q9b[1, 2:6] / Q9b[3, 2:6]
  
}

# Q9c
{
  Q9c_detail <- get_relevant_events(enrollment_recent_assessment) 
  
  Q9c_and_d_data <- Q9c_detail %>%
    filter(RelationshipToHoH == 1) %>%
    group_by(PersonalID) %>%
    slice(1L) %>%
    ungroup()
  
  Q9c_data <- Q9c_and_d_data %>%
    filter(Event_Event %in% 1:4)
  
  Q9c <- Q9c_data %>%
    return_household_groups(., Label, EventTypes$Label[1:4]) %>%
    adorn_totals("row") %>%
    union(Q9c_data %>%
            filter(Event_ProbSolDivRRResult == 1) %>%
            mutate(Label = "Result: Client housed/Re-Housed in a safe alternative") %>%
            return_household_groups(., Label, 
                                    "Result: Client housed/Re-Housed in a safe alternative"))
  
  Q9c[7, 1:6] <- c("Percent of successful referrals to Problem Solving/Diversion/Rapid Resolution",
                   Q9c[6, 2:6] / Q9c[2, 2:6])
  Q9c <- ifnull(Q9c, 0)
}

# Q9d
{
  Q9d_detail <-  "See Q9c_detail.csv"
  
  Q9d_data <- Q9c_and_d_data %>%
    filter(Event_Event %in% 5:18)
  
  prioritized_referred_data <- Q9b_detail %>%
    filter(RelationshipToHoH == 1 &
             prioritization_status == "Placed on Prioritization List (Prioritized)") %>%
    mutate(Label = PersonalID %in% Q9d_data$PersonalID) %>%
    return_household_groups(., Label, 
                            c(TRUE, FALSE)) %>%
    adorn_totals("row")
  
  prioritized_referred <- prioritized_referred_data[0, ]
  prioritized_referred[1, 1:6] <- c("Of the total HH prioritized (Q9b row 1) what percentage received a referral",
                            prioritized_referred_data[1, 2:6] / prioritized_referred_data[3, 2:6])
  
  referral_result_data <- Q9d_data %>%
    filter(Event_Event %in% c(10:15, 17:18)) %>%
    mutate(Label = referral_results[Event_ReferralResult],
           Label = if_else(is.na(Label), "No result recorded", Label))
  
  referral_result_table <- referral_result_data %>%
    return_household_groups(., Label, referral_results)
  
  housing_program_referrals <- referral_result_data %>%
    return_household_groups(., RelationshipToHoH, c(1))
  
  successful_housing_program_referrals <- referral_result_table[0, ]
  successful_housing_program_referrals[1, 1:6] <- c(
    "Percent of successful referrals to residential projects",
    referral_result_table[1, 2:6] / housing_program_referrals[1, 2:6])
  
  aftercare_results <- Q9d_data %>%
    filter(Event_Event == 5 &
             Event_ReferralCaseManageAfter == 1) %>%
    mutate(Label = "Result: Enrolled in Aftercare project") %>%
    return_household_groups(., Label, "Result: Enrolled in Aftercare project")
  
  Q9d <- Q9d_data %>%
    return_household_groups(., Label, EventTypes$Label[5:18]) %>%
    adorn_totals("row") %>%
    union(prioritized_referred) %>%
    union(referral_result_table) %>%
    union(aftercare_results) %>%
    union(successful_housing_program_referrals)
}


# Q10
{
  all_columns <- c("Crisis Needs Assessment",
                   "Housing Needs Assessment",
                   EventTypes$Label)
  
  Q10_assessments <- relevant_assessments %>%
    # filter(PersonalID %in% enrollment_recent_assessment$PersonalID) %>%
    mutate(event_type = "Assessment") %>%
    rename(unique_id = AssessmentID,
           group = AssessmentLevel,
           event_date = AssessmentDate) %>%
    select(PersonalID, EnrollmentID, event_type, event_date, unique_id, group)
  
  Q10_assessment_table <- Q10_assessments %>%
    group_by(group) %>%
    summarise(Total.Occurrences = uniqueN(unique_id)) %>%
    ungroup() %>%
    mutate(group = all_columns[group])
  
  Q10_events <- relevant_events %>%
    # filter(PersonalID %in% enrollment_recent_assessment$PersonalID) %>%
    filter(EventDate <= report_end_date) %>%
    mutate(event_type = "Event") %>%
    rename(unique_id = EventID,
           group = Event,
           event_date = EventDate) %>%
    select(PersonalID, EnrollmentID, event_type, event_date, unique_id,
           group, ReferralResult, ProbSolDivRRResult, ReferralCaseManageAfter)
  
  Q10_event_table <- Q10_events %>%
    group_by(group) %>%
    summarise(Total.Occurrences = uniqueN(unique_id),
              Successful.Referral = uniqueN(unique_id[group %in% c(10:15, 17:18) & 
                                                      ReferralResult == 1]),
              Unsuccessful.Referral.client.rejected = uniqueN(unique_id[group %in% c(10:15, 17:18) & 
                                                                        ReferralResult == 2]),
              Unsuccessful.Referral.provider.rejected = uniqueN(unique_id[group %in% c(10:15, 17:18) & 
                                                                          ReferralResult == 3]),
              Rehoused.in.safe.alternative = uniqueN(unique_id[group == 2 & 
                                                               ProbSolDivRRResult == 1]),
              Enrolled.in.aftercare = uniqueN(unique_id[group == 5 & 
                                                        ReferralCaseManageAfter == 1])
    ) %>%
    ungroup() %>%
    mutate(group = group + 2,
           group = all_columns[group])
  
  Q10_detail <- Q10_assessments %>%
    full_join(Q10_events, 
              by = intersect(colnames(Q10_assessments),
                             colnames(Q10_events)))
  
  Q10 <- as.data.frame(all_columns) %>%
    rename(group = all_columns) %>%
    left_join(Q10_assessment_table %>%
                full_join(Q10_event_table, by = c("group", "Total.Occurrences")), 
              by = "group")
  
}

# --------------------------------------
# --------------------------------------
# --------------------------------------

if (generate_new_kits) {
  
  folder_name <- paste("Test Kit", format(Sys.Date(), "%m.%d.%y"))
  
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
  
  if (!dir.exists(paste0(folder_name, "/Reports"))) {
    dir.create(paste0(folder_name, "/Reports"))
  }
  
    for (question in CE_APR_files) {
      if (exists(question)) {
        
        to_write <- get(question)
        
        if(question != "Q4a") {
          to_write <- to_write %>%
            set_hud_format()
          
          write.csv(get(paste0(question, "_detail")),
                    file.path(paste0("created_files_2/", question, ".csv")),
                    row.names = FALSE)
        }
        
        to_write <- to_write %>% 
          ifnull(., 0) 
        
        to_write[is.na(to_write)] <- ""
        
        write_csv(to_write, file.path(paste0("created_files/", question, ".csv")))
      } else {
        missing_files <- c(missing_files, paste("CE APR -", projects_included, "-", question))
      }
    }
    general_wd <- getwd()
    setwd(paste0(general_wd, "/", folder_name, "/Reports"))
    
    archive_write_dir("CE APR - DataLab - Systemwide (A).zip",
                      paste0(general_wd, "/created_files"))
    archive_write_dir("CE APR - DataLab - Systemwide (D).zip",
                      paste0(general_wd, "/created_files_2"))
    
    setwd(general_wd)
    
    unlink(paste0(getwd(), "/created_files/*"))
    unlink(paste0(getwd(), "/created_files_2/*"))

}

