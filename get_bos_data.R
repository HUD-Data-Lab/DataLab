library(archive)
library(syn)
library(lexicon)
library(stringr)
library(stringi)
library(generator)

programs_to_get <- "all"
# programs_to_get <- "nbn"

source("local_functions.R")
source("datalab_functions.R")
source("DataLab_Lists.R")

unhashed_data <- file.choose()
# save_to <- choose.dir()

for (file in names(hmis_csvs)){
  
  data <- read_csv(archive_read(unhashed_data, paste0(file, ".csv")),
                   col_types = get(file, hmis_csvs)) 
  
  if ("CoCCode" %in% colnames(data)) {
    data <- data %>%
      mutate(CoCCode = gsub("IA", "XX", CoCCode)) 
  }
  
  if ("UserID" %in% colnames(data)) {
    data <- data %>%
      trunc_userid(.)
  }
  
  if(file == "Assessment") {
    data <- data %>%
      mutate(AssessmentType = ifnull(AssessmentType, 3))
  }
  
  assign(file, data)
  
  # write.csv(data, file = paste0(save_to, "\\", file, ".csv"), row.names = FALSE)
  
}

# start_date <- min(Exit$ExitDate)
start_date <- ymd("2021-10-1")
end_date <- ymd("2022-9-30")

Export$ExportDate <- Sys.time()
Export$ExportStartDate <- start_date
Export$ExportEndDate <- end_date

{
  funding_sources <- data.frame(matrix(
    c(1, "HUD: CoC – Homelessness Prevention (High Performing Comm. Only)",
      2, "HUD: CoC – Permanent Supportive Housing",
      3, "HUD: CoC – Rapid Re-Housing",
      4, "HUD: CoC – Supportive Services Only",
      5, "HUD: CoC – Transitional Housing",
      6, "HUD: CoC – Safe Haven",
      7, "HUD: CoC – Single Room Occupancy (SRO)",
      8, "HUD: ESG – Emergency Shelter (operating and/or essential services)",
      9, "HUD: ESG – Homelessness Prevention",
      10, "HUD: ESG – Rapid Rehousing",
      11, "HUD: ESG – Street Outreach",
      12, "HUD: Rural Housing Stability Assistance Program",
      13, "HUD: HOPWA – Hotel/Motel Vouchers",
      14, "HUD: HOPWA – Housing Information",
      15, "HUD: HOPWA – Permanent Housing (facility based or TBRA)",
      16, "HUD: HOPWA – Permanent Housing Placement",
      17, "HUD: HOPWA – Short-Term Rent, Mortgage, Utility assistance",
      18, "HUD: HOPWA – Short-Term Supportive Facility",
      19, "HUD: HOPWA – Transitional Housing (facility based or TBRA)",
      20, "HUD: HUD/VASH",
      21, "HHS: PATH – Street Outreach & Supportive Services Only",
      22, "HHS: RHY – Basic Center Program (prevention and shelter)",
      23, "HHS: RHY – Maternity Group Home for Pregnant and Parenting Youth",
      24, "HHS: RHY – Transitional Living Program",
      25, "HHS: RHY – Street Outreach Project",
      26, "HHS: RHY – Demonstration Project",
      27, "VA: CRS Contract Residential Services",
      30, "VA: Community Contract Safe Haven Program",
      33, "VA: Supportive Services for Veteran Families",
      34, "N/A",
      35, "HUD: Pay for Success",
      36, "HUD: Public and Indian Housing (PIH) Programs",
      37, "VA: Grant Per Diem – Bridge Housing",
      38, "VA: Grant Per Diem – Low Demand",
      39, "VA: Grant Per Diem – Hospital to Housing",
      40, "VA: Grant Per Diem – Clinical Treatment",
      41, "VA: Grant Per Diem – Service Intensive Transitional Housing",
      42, "VA: Grant Per Diem – Transition in Place",
      43, "HUD: CoC – Youth Homeless Demonstration Program (YHDP)",
      44, "HUD: CoC – Joint Component TH/RRH",
      45, "VA: Grant Per Diem – Case Management/Housing Retention",
      46, "Local or Other Funding Source (Please Specify)",
      47, "HUD: ESG – CV",
      48, "HUD: HOPWA – CV",
      49, "HUD: CoC – Joint Component RRH/PSH",
      50, "HUD: HOME",
      51, "HUD: HOME (ARP)",
      52, "HUD: PIH (Emergency Housing Voucher)"), ncol = 2, byrow = TRUE)) %>%
    'colnames<-'(c("Funder", "FunderName")) %>%
    mutate(Funder = as.integer(Funder))
}

active_enrollments <- Enrollment %>%
  left_join(Exit %>%
              filter(ExitDate <= end_date), 
            by = "EnrollmentID") %>%
  filter(EntryDate <= end_date &
           (is.na(ExitDate) |
              ExitDate >= start_date))

activity_information <- active_enrollments %>%
  filter(HouseholdID %in% active_enrollments$HouseholdID[active_enrollments$RelationshipToHoH == 1]) %>%
  mutate(end_of_timeframe = ifnull(ExitDate, end_date),
         days_enrolled = as.integer(trunc((EntryDate %--% end_of_timeframe) / days(1)))) %>%
  select(EnrollmentID, ProjectID, EntryDate, ExitDate, days_enrolled)

picking_projects <- Project %>%
  inner_join(activity_information %>%
              group_by(ProjectID) %>%
              summarize(enrollments = n(),
                        last_entry = max(EntryDate)),
            by = "ProjectID") %>%
  filter(is.na(OperatingEndDate) &
           ContinuumProject == 1 &
           HMISParticipatingProject == 1 &
           last_entry > start_date) %>%
  inner_join(Funder %>%
              filter((is.na(EndDate) |
                       EndDate > start_date) &
                       Funder != 34) %>%
              select(ProjectID, Funder) %>%
              distinct(), 
            by = "ProjectID",
            multiple = "all") %>%
  left_join(funding_sources, by = "Funder") %>%
  arrange(ProjectType) %>%
  select(ProjectType, ProjectID, ProjectName, TrackingMethod, enrollments, 
         last_entry, FunderName)

if (programs_to_get == "all") {
  project_matrix <- matrix(
    c(
      # 1386,	"DataLab - Coordinated Entry",
      1362,	"DataLab - ES-EE ESG I",
      # 549,	"DataLab - ES-EE ESG II (with CE elements)",
      # 1210,	"DataLab - ES-EE RHY",
      93,	"DataLab - ES-NbN ESG",
      1409,	"DataLab - HP ESG",
      780,	"DataLab - PSH CoC I",
      # 8,	"DataLab - PSH HOWPA",
      # 1487,	"DataLab - PSH VASH",
      1428,	"DataLab - RRH CoC I",
      1495,	"DataLab - RRH CoC II",
      1060,	"DataLab - RRH ESG I",
      # 1351,	"DataLab - RRH VA",
      # 14,	"Datalab - SH VA-HCHV",
      1647,	"DataLab - SO CoC",
      1419,	"DataLab - SO ESG",
      # 1321,	"DataLab - SO PATH",
      1615,	"DataLab - SSO CoC",
      388,	"DataLab - TH CoC",
      340,	"DataLab - TH ESG"#,
      # 21,	"DataLab - TH HOPWA",
      # 450,	"DataLab - TH RHY",
      # 1453,	"DataLab - TH VA"
      # 24,	"DataLab - TH YHDP"
    ), ncol = 2, byrow = TRUE
  )
} else if (programs_to_get == "nbn") {
  project_matrix <- matrix(
    c(5,	"DataLab - ES-NbN ESG"
    ), ncol = 2, byrow = TRUE
  )
}

datalab_projects <- Project %>%
  inner_join(data.frame(project_matrix) %>%
    `colnames<-`(c("ProjectID", "NewProjectName"))
  , by = "ProjectID") %>%
  mutate(ProjectName = NewProjectName,
         ProjectCommonName = substr(NewProjectName, 11, nchar(NewProjectName))) %>%
  select(-NewProjectName)

# for hashing Client and User
{
  words_for_names <- setNames(
    data.frame(c(
      syn("color")[n_words(syn("color")) <= 1],
      syn("vegetable")[n_words(syn("vegetable")) <= 1],
      syn("herb")[n_words(syn("herb")) <= 1],
      syn("jewel")[n_words(syn("jewel")) <= 1],
      syn("star")[n_words(syn("star")) <= 1],
      syn("fabric")[n_words(syn("fabric")) <= 1]
    )), c("word")) %>%
    distinct() %>%
    filter(!nsfw_word(word)) %>%
    mutate(word = str_to_title(word))
  }

Project <- trunc_userid(datalab_projects) %>%
  mutate(TrackingMethod = case_when(
    ProjectID == 93 ~ 3,
    TRUE ~ TrackingMethod))

ProjectCoC <- trunc_userid(ProjectCoC) %>%
  filter(ProjectID %in% Project$ProjectID) 
ProjectCoC$Address1 <- paste(stri_rand_strings(nrow(ProjectCoC), 3, pattern = "[1-9]"),
                              "Main St")


Enrollment <- trunc_userid(Enrollment) %>%
  filter(ProjectID %in% Project$ProjectID &
           EnrollmentID %in% activity_information$EnrollmentID &
           (ProjectID != 93 |
              EnrollmentID %in% activity_information$EnrollmentID[activity_information$days_enrolled >= 45]))

# sampled_enrollments <- Enrollment %>% 
#   group_by(ProjectID) %>% 
#   slice_sample(n = 30)
# 
# Enrollment <- trunc_userid(Enrollment) %>%
#   filter(HouseholdID %in% sampled_enrollments$HouseholdID)

Funder <- trunc_userid(Funder) %>%
  filter(ProjectID %in% Project$ProjectID) %>%
  # this is to create additional CoC-funded projects
  `colnames<-`(c(paste0(colnames(Funder), "_1"))) %>%
  mutate(Funder_1 = case_when(
      ProjectID_1 == 1615 ~ as.integer(4),
      ProjectID_1 == 1647 ~ as.integer(11),
      TRUE ~ Funder_1)) %>%
  `colnames<-`(colnames(Funder))

Funder$GrantID <- stri_rand_strings(nrow(Funder), 6, pattern = "[A-Z]")

Inventory <- trunc_userid(Inventory) %>%
  filter(ProjectID %in% Project$ProjectID) 

Organization <- trunc_userid(Organization) %>%
  filter(OrganizationID %in% Project$OrganizationID) 
part_1 <- c("Humanitarian", "People", "Care", "Hope", "United", "Dream", "Future")
part_2 <- c("Foundation", "Planet", "Community", "Services", "Place")
Organization$OrganizationCommonName <- sample(part_1, nrow(Organization), 
                                              replace = TRUE)
Organization$OrganizationName <- paste("The", Organization$OrganizationCommonName,
                                       sample(part_2, nrow(Organization), 
                                              replace = TRUE))

Client <- trunc_userid(Client) %>%
  filter(PersonalID %in% Enrollment$PersonalID)

User <- User %>%
  mutate(UserID = substr(UserID, 1, 2),
         UserFirstName = sample(words_for_names$word, nrow(User)),
         UserLastName = sample(words_for_names$word, nrow(User))
  ) %>%
  group_by(UserID) %>%
  slice(1L) %>%
  ungroup()
User$UserPhone <- r_phone_numbers(nrow(User))
User$UserEmail <- r_email_addresses(nrow(User))

# generate NbN services
NbN_projects <- 93
source("create_NbN_stays.R")

Assessment <- trunc_userid(Assessment) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) %>%
  left_join(Enrollment %>% 
              select(EnrollmentID, ProjectID),
            by = "EnrollmentID") %>%
  mutate(AssessmentLocation = ProjectID) %>%
  select(-ProjectID)

Exit <- trunc_userid(Exit) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

CurrentLivingSituation <- trunc_userid(CurrentLivingSituation) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID)  %>%
  mutate(LocationDetails = NA)

Disabilities <- trunc_userid(Disabilities) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

EmploymentEducation <- trunc_userid(EmploymentEducation) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

EnrollmentCoC <- trunc_userid(EnrollmentCoC) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

Event <- trunc_userid(Event) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

HealthAndDV <- trunc_userid(HealthAndDV) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

IncomeBenefits <- trunc_userid(IncomeBenefits) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

YouthEducationStatus <- trunc_userid(YouthEducationStatus) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 


# hashing Client file
{
  ssns <- Client %>%
    select(SSN) %>%
    filter(!is.na(SSN)) %>%
    distinct() %>%
    mutate(sequential = lapply(SSN, sequential_ssn),
           valid = 
             suppressWarnings(!is.na(as.numeric(SSN))) &
             nchar(SSN) == 9 &
             substr(SSN, 1, 3) != "000" &
             substr(SSN, 1, 3) != "666" &
             substr(SSN, 1, 1) != "9" &
             substr(SSN, 4, 5) != "00" &
             substr(SSN, 6, 9) != "0000" &
             SSN %nin% c("111111111", "222222222", "333333333",
                         "444444444", "555555555", "666666666",
                         "777777777", "888888888", "999999999") &
             sequential == FALSE
    ) %>%
    arrange(desc(valid))
  
  valid_ssns <- c()
  
  while (length(valid_ssns) < sum(ssns$valid) - 11) {
    possible_ssn <- sample(100000000:899999999, 1)
    if (!sequential_ssn(possible_ssn) &
        substr(possible_ssn, 1, 3) != "000" &
        substr(possible_ssn, 1, 3) != "666" &
        substr(possible_ssn, 1, 1) != "9" &
        substr(possible_ssn, 4, 5) != "00" &
        substr(possible_ssn, 6, 9) != "0000" &
        possible_ssn %nin% c("111111111", "222222222", "333333333",
                             "444444444", "555555555", "666666666",
                             "777777777", "888888888", "999999999",
                             valid_ssns)) {
      valid_ssns <- c(possible_ssn, valid_ssns)
    }
  }
  
  invalid_ssns <- c(NA, "xxx021232", "xxxxx9123", "92415x182", 000342842, 
                    666284132, 912358284, 382004382,
                    729440000, 888888888, 890123456)
  
  while (length(invalid_ssns) < nrow(ssns) - length(valid_ssns)) {
    possible_ssn <- sample(100000000:999999999, 1)
    if (sequential_ssn(possible_ssn) |
        substr(possible_ssn, 1, 3) == "000" |
        substr(possible_ssn, 1, 3) == "666" |
        substr(possible_ssn, 1, 1) == "9" |
        substr(possible_ssn, 4, 5) == "00" |
        substr(possible_ssn, 6, 9) == "0000" |
        possible_ssn %in% c("111111111", "222222222", "333333333",
                            "444444444", "555555555", "666666666",
                            "777777777", "888888888", "999999999")) {
      invalid_ssns <- c(invalid_ssns, possible_ssn)
    }
  }
  
  ssns$generated_ssns <- c(valid_ssns, invalid_ssns)
  
  
  Client <- Client %>%
    left_join(ssns, by = "SSN") %>%
    mutate(SSN = generated_ssns) %>%
    select(-c(sequential, valid, generated_ssns)) %>%
    mutate(new_FirstName = sample(words_for_names$word, nrow(Client), replace = TRUE),
           new_MiddleName = sample(words_for_names$word, nrow(Client), replace = TRUE),
           new_LastName = sample(words_for_names$word, nrow(Client), replace = TRUE),
           FirstInitial = sample(str_to_upper(letters), nrow(Client), replace = TRUE),
           MiddleInitial = sample(str_to_upper(letters), nrow(Client), replace = TRUE),
           LastInitial = sample(str_to_upper(letters), nrow(Client), replace = TRUE),
           FirstName = 
             if_else(
               nchar(FirstName) == 1,
               FirstInitial,
               new_FirstName),
           MiddleName = 
             case_when(
               nchar(MiddleName) == 1 ~ MiddleInitial,
               nchar(MiddleName) > 1 ~ new_MiddleName),
           LastName = 
             if_else(
               nchar(LastName) == 1,
               LastInitial,
               new_LastName),
           NameSuffix = case_when(
             !str_detect(NameSuffix, "Marty") &
               !str_detect(NameSuffix, "Josiah") ~ NameSuffix
           )) %>%
    select(-c(new_FirstName, new_MiddleName, new_LastName,
              FirstInitial, MiddleInitial, LastInitial)) %>%
    left_join(Enrollment %>%
                select(PersonalID, EntryDate) %>%
                group_by(PersonalID) %>%
                arrange(EntryDate) %>%
                slice(1L) %>%
                ungroup,
              by = "PersonalID") %>%
    mutate(days_to_shift = sample(-730:730, nrow(Client), replace = TRUE),
           DOB = if_else(ymd(EntryDate) - ymd(DOB + days_to_shift) < 0,
                         DOB - days_to_shift / 2,
                         DOB + days_to_shift)) %>%
    select(-c(EntryDate, days_to_shift))
}

# write to zipped folder
{
  for (file in names(hmis_csvs)) {
    write.csv(get(file), file.path(paste0("created_files/", file, ".csv")), 
              row.names=FALSE, na="")
  }
  
  archive_write_dir(paste0("DataLab - Hashed CSVs.zip"),
                    paste0(getwd(), "/created_files"))
  
  unlink(paste0(getwd(), "/created_files/*"))
}
