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
compare_to_last <- FALSE
if (compare_to_last) {
  compare_to_dir <- choose.dir()}
combining_files <- TRUE

source("DataLab.R")

starting_env <- environment()
# environment <- starting_env
items_to_keep <- c("items_to_keep", ls())

# this is for testing variations in CoC code in test kit data,
# commented out for QA
# ProjectCoC <- ProjectCoC %>%
#   mutate(CoCCode = case_when(
#     ProjectID %in% c(340, 780, 1647) ~ "XX-500",
#     TRUE ~ CoCCode)) %>%
#   union(hold_ProjectCoC %>%
#           filter(ProjectID == 1647))

# need to modify this per AAQ 204781
CE_element_projects <- Project %>%
  inner_join(Enrollment %>%
               filter(EnrollmentID %in%
                        union(Assessment$EnrollmentID, 
                              Event$EnrollmentID)) %>%
               select(ProjectID) %>%
               distinct(),
             by = "ProjectID")

CE_element_projects <- CE_element_projects %>%
  filter(ContinuumProject == 1)


multi_CoC_projects <- ProjectCoC %>%
  group_by(ProjectID) %>%
  summarise(CoCs = uniqueN(CoCCode)) %>%
  filter(CoCs > 1)


CE_element_projects_CoC <- CE_element_projects %>%
  filter(ProjectID %in% 
           ProjectCoC$ProjectID[ProjectCoC$CoCCode == relevant_CoC]) %>%
  mutate(multi_CoC = ProjectID %in% multi_CoC_projects$ProjectID)


enrollment_CoC_over_time <- Enrollment %>%
  select(EnrollmentID, HouseholdID, EntryDate) %>%
  left_join(Exit %>%
              select(EnrollmentID, ExitDate),
            by = "EnrollmentID") %>%
  inner_join(EnrollmentCoC %>%
               select(HouseholdID, CoCCode, InformationDate), 
             by = "HouseholdID") %>%
  filter(CoCCode == relevant_CoC &
           InformationDate >= EntryDate &
           (InformationDate <= ExitDate |
              is.na(ExitDate))) %>%
  select(-c(EntryDate, ExitDate))

most_recent_assessment <- Assessment %>%
  inner_join(enrollment_CoC_over_time, 
             by = "EnrollmentID") %>%
  filter(AssessmentDate >= report_start_date &
           AssessmentDate <= report_end_date & 
           (InformationDate <= AssessmentDate |
              is.na(InformationDate))) %>%
  group_by(PersonalID) %>%
  arrange(desc(AssessmentDate)) %>%
  slice(1L) %>%
  ungroup()

# event_filtered <- Event %>%
#   inner_join(enrollment_CoC_over_time,
#             by = "EnrollmentID") %>%
#   filter(EventDate <= (report_end_date + days(90)) & 
#            (InformationDate <= EventDate |
#               is.na(InformationDate))) %>%
#   select(colnmaes(Event))


# get additional client information (age for reporting)
client_plus <- add_client_info(Enrollment) 

enrollment_data <- Enrollment %>%
  left_join(Exit %>%
              select(-PersonalID),
            by = "EnrollmentID") %>%
  left_join(Project %>%
              select(ProjectID, ProjectType, TrackingMethod, ProjectName),
            by = "ProjectID")

household_info <- get_household_info(enrollment_data)

enrollment_recent_assessment <- enrollment_data %>%
  inner_join(most_recent_assessment %>%
               select(HouseholdID, AssessmentDate, AssessmentType, PrioritizationStatus), 
             by = "HouseholdID") %>%
  filter(AssessmentDate >= EntryDate &
           (AssessmentDate <= ExitDate |
              is.na(ExitDate))) %>%
  left_join(client_plus, by = "PersonalID") %>%
  left_join(household_info, by = "PersonalID") %>%
  mutate(chronic = NA)

# Q4a
{
  Q4a <- program_information_table(enrollment_recent_assessment$ProjectID,
                                   enrollment_recent_assessment)
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
              by = "Group")
  
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
  clients_at_recent_assessment <- client_plus %>%
    select(-intersect(colnames(client_plus),
                      colnames(enrollment_recent_assessment)[colnames(enrollment_recent_assessment) != "PersonalID"])) %>%
    filter(PersonalID %in% enrollment_recent_assessment$PersonalID)
  
  hh_info_at_recent_assessment <- enrollment_recent_assessment %>%
    select(-colnames(household_info)[colnames(household_info) != "PersonalID"]) %>%
    left_join(get_household_info(enrollment_recent_assessment,
                                 clients_at_recent_assessment), 
              by = "PersonalID")
  
  Q9a_detail <- hh_info_at_recent_assessment %>%
    select(all_of(ce_detail_columns), AssessmentType) %>%
    mutate(assessment_type = case_when(AssessmentType == 1 ~ "Phone",
                                       AssessmentType == 2 ~ "Virtual",
                                       AssessmentType == 3 ~ "In-person")) 
  
  Q9a <- Q9a_detail %>%
    filter(RelationshipToHoH == 1) %>%
    return_household_groups(., assessment_type, 
                            c("Phone", "Virtual", "In-person")) %>%
    adorn_totals("row")
}

# Q9b
{
  Q9b_detail <- hh_info_at_recent_assessment %>%
    select(all_of(ce_detail_columns), PrioritizationStatus) %>%
    mutate(prioritization_status = case_when(PrioritizationStatus == 1 ~ "Placed on Prioritization List (Prioritized)",
                                       PrioritizationStatus == 2 ~ "Not Placed on Prioritization List")) 
  
  Q9b <- Q9b_detail %>%
    filter(RelationshipToHoH == 1) %>%
    return_household_groups(., prioritization_status, 
                            c("Placed on Prioritization List (Prioritized)", 
                              "Not Placed on Prioritization List")) %>%
    adorn_totals("row") %>%
    untabyl()
  
  Q9b[3, 2:6] <- Q9b[1, 2:6] / Q9b[3, 2:6]
  
}

# Q9c
{
  event_prefixes <- rep("Event_", length(Event))
  event_prefixes[[which(colnames(Event) == "PersonalID")]] <- ""
  
  individual_cutoff_dates <- hh_info_at_recent_assessment %>%
    select(PersonalID) %>%
    left_join(Assessment %>%
                filter(PersonalID %in% hh_info_at_recent_assessment$PersonalID &
                         AssessmentDate > report_end_date &
                         AssessmentDate <= report_end_date + days(90)) %>%
                select(PersonalID, AssessmentDate),
              by = "PersonalID") %>%
    rename(cutoff_date = AssessmentDate)
  
  Q9c_detail <- hh_info_at_recent_assessment %>%
    inner_join(Event %>%
                 filter(Event %in% 1:4) %>%
                 left_join(EventTypes, by = "Event") %>%
                 `colnames<-`(c(paste0(event_prefixes, colnames(Event)), "Label")), 
               by = "PersonalID") %>%
    left_join(individual_cutoff_dates, by = "PersonalID") %>%
    filter(RelationshipToHoH == 1 &
             Event_EventDate >= AssessmentDate &
             Event_EventDate <= report_end_date + days(90) &
             (is.na(cutoff_date) |
                Event_EventDate < cutoff_date)) %>%
    select(all_of(ce_detail_columns), Event_Event, Event_EnrollmentID, 
           Event_EventDate, Event_ProbSolDivRRResult, Label) %>%
    mutate(same_enrollment = Event_EnrollmentID == EnrollmentID,
           test = is.na(ExitDate)) %>%
    arrange(desc(same_enrollment), desc(Event_EventDate)) 
  
  Q9c_data <- Q9c_detail %>%
    group_by(PersonalID) %>%
    slice(1L) %>%
    ungroup() 
  
  Q9c <- Q9c_data %>%
    return_household_groups(., Label, EventTypes$Label[1:4]) %>%
    adorn_totals("row") %>%
    union(Q9c_data %>%
            filter(Event_ProbSolDivRRResult == 1) %>%
            mutate(Label = "Result: Client housed/Re-Housed in a safe alternative") %>%
            return_household_groups(., Label, 
                                    "Result: Client housed/Re-Housed in a safe alternative"))
  
  Q9c[7, 1:6] <- c("Percent of successful referrals to Problem Solving/Diversion/Rapid Resolution",
                   Q9c[5, 2:6] / Q9c[6, 2:6])
  Q9c <- ifnull(Q9c, 0)
  
}

# --------------------------------------

rm(list = ls()[ls() %nin% items_to_keep])
