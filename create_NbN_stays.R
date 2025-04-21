NbN_services <- slice(Services, 0)

if (exists("activity_information")) {
  NbN_stays <- activity_information %>%
    filter(ProjectID %in% NbN_projects) %>%
    inner_join(Enrollment %>%
                 select(EnrollmentID, PersonalID),
               by = "EnrollmentID")
} else {
  NbN_stays <- enrollment_data %>%
    filter(HouseholdID %in% enrollment_data$HouseholdID[enrollment_data$RelationshipToHoH == 1 &
                                                          enrollment_data$ProjectID %in% NbN_projects]) %>%
    mutate(end_of_timeframe = ifnull(ExitDate, report_end_date),
           days_enrolled = as.integer(trunc((EntryDate %--% end_of_timeframe) / days(1))))
}


NbN_stays$fraction <- sample(1:10, nrow(NbN_stays), replace = TRUE) / 10
NbN_stays <- NbN_stays %>%
  mutate(num_nights_stayed = round(fraction * (days_enrolled - 1), 0)) %>%
  filter(days_enrolled > 0 &
           num_nights_stayed > 0)

for(stay in 1:nrow(NbN_stays)) {
  stay_information <- NbN_stays[stay, ]
  nights_stayed <- stay_information$EntryDate + 
    sort(sample(0:stay_information$days_enrolled, 
                stay_information$num_nights_stayed))
  
  # if (length(nights_stayed) > 0) {
    bed_nights <- data.frame(EnrollmentID = stay_information$EnrollmentID,
                             PersonalID = stay_information$PersonalID,
                             DateProvided = nights_stayed,
                             DateCreated = nights_stayed,
                             DateUpdated = nights_stayed)
    
    NbN_services <- NbN_services %>% 
      full_join(bed_nights, 
                by = c("EnrollmentID", "PersonalID", "DateProvided",
                       "DateCreated", "DateUpdated"))
  # }

}

NbN_services$ServicesID <- seq.int(nrow(NbN_services))
NbN_services$RecordType <- 200
NbN_services$TypeProvided <- 200
NbN_services$UserID <- sample(User$UserID, nrow(NbN_services),
                              replace = TRUE)
NbN_services$ExportID <- Export$ExportID
NbN_services$DateCreated <- as.POSIXct(paste(NbN_services$DateCreated, "2pm"), format = "%Y-%m-%d %I%p")
NbN_services$DateUpdated <- as.POSIXct(paste(NbN_services$DateUpdated, "2pm"), format = "%Y-%m-%d %I%p")


Services$ServicesID <- as.integer(Services$ServicesID) 
Services <- trunc_userid(Services) %>%
  full_join(NbN_services, by = colnames(NbN_services)) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID)

Project <- Project %>%
  mutate(ProjectType = if_else(ProjectID %in% NbN_projects, 1, ProjectType))
