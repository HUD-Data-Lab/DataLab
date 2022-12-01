library(archive)
library(syn)
library(lexicon)
library(stringr)

programs_to_get <- "all"
# programs_to_get <- "nbn"

source("datalab_functions.R")

set.seed(2022)


hmis_csvs <- list(Affiliation = "cccTTcTc", 
                  Assessment = "cccDciiiTTcTc", 
                  AssessmentQuestions = "ccccciccTTcTc", 
                  AssessmentResults = "ccccccTTcTc",
                  Client = "ccccciciDiiiiiiiiiiiiiciiiiiiiiiiiiiTTcTc", 
                  CurrentLivingSituation = "cccDiciiiiicTTcTc", 
                  Disabilities = "cccDiiiiiiiiiiiTTcTc", 
                  EmploymentEducation = "cccDiiiiiiTTcTc",
                  Enrollment = "cccDciiiiiDiiiDDDiiiicccciiiDiiiiciiiiiiiiiiiiciiiiiiiiiiiiiiiiiiiiTTcTc", 
                  EnrollmentCoC = "cccccDciTTcTc", 
                  Event = "cccDiiiciDTTcTc",
                  Exit = "cccDiciiiiiiiiiiiiiiiiiiiiiiiiiDiiiiiiTTcTc",
                  Export = "ciccccciiTTTccciii", 
                  Funder = "cciccDDTTcTc", 
                  HealthAndDV = "cccDiiiiiiiDiiiiiTTcTc", 
                  IncomeBenefits = "cccDididididididididididididididididciiiiiiiciiiiiiiiiiiiiiiiiiiiciiiiiiiiTTcTc", 
                  Inventory = "ccciiiiiiiiiiiiDDTTcTc", 
                  Organization = "ccncTTcTn", 
                  Project = "ccccDDnnnnnnnnnTTcTc", 
                  ProjectCoC = "ccccccccciTTcTc",
                  Services = "cccDiicciciTTcTc", 
                  User = "ccccccTTTc", 
                  YouthEducationStatus = "cccDiiiiTTcTc")

iowa_bos_7z <- file.choose()
# save_to <- choose.dir()

for (file in names(hmis_csvs)){
  
  data <- read_csv(archive_read(iowa_bos_7z, paste0(file, ".csv")), 
                   col_types = get(file, hmis_csvs))
  
  assign(file, data)
  
  # write.csv(data, file = paste0(save_to, "\\", file, ".csv"), row.names = FALSE)
  
}

# old_first_names <- Client %>%
#   select(FirstName) %>%
#   distinct()
# 
# old_last_names <- Client %>%
#   select(LastName) %>%
#   distinct()

# words_for_names <- hash_grady_pos %>%
#   filter(!str_detect(word, "[:blank:]") &
#            !str_detect(word, "[:punct:]") &
#            !str_detect(word, "[:digit:]") &
#            nchar(word) >= 4) %>%
#   inner_join(hash_sentiment_jockers %>%                 # Originally downloaded from: http://icon.shef.ac.uk/Mob
#                full_join(hash_sentiment_senticnet,      # Cambria, E., Poria, S., Bajpai, R. and Schuller, B. SenticNet 4: A semantic resource for sentiment analysis based on conceptual primitives. In: COLING, pp. 2666-2677, Osaka (2016))
#                          by = c("x", "y")) %>%
#                filter(y >= 0),
#              by = c("word" = "x")) %>%
#   mutate(word = str_to_title(word)) %>%
#   group_by(word) %>%
#   slice(1L) %>%
#   ungroup()
# 
# new_first_names <- words_for_names %>%
#   filter(pos == "Adjective")
# 
# new_last_names <- words_for_names %>%
#   filter(pos == "Noun")
# 
# first_names <- cbind(old_first_names, 
#                      "NewFirstName" = sample(new_first_names$word, size = nrow(old_first_names), replace = TRUE))
# 
# last_names <- cbind(old_last_names, 
#                      "NewLastName" = sample(new_last_names$word, size = nrow(old_last_names), replace = TRUE))
# 
# new_names <- Client %>%
#   select(FirstName, LastName) %>%
#   left_join(first_names, by = "FirstName") %>%
#   left_join(last_names, by = "LastName") 

ssns <- Client %>%
  select(SSN) %>%
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

while (length(valid_ssns) < sum(ssns$valid)) {
  possible_ssn <- sample(100000000:899999999, 1)
  if (!sequential_ssn(possible_ssn) &
      substr(possible_ssn, 1, 3) != "000" &
      substr(possible_ssn, 1, 3) != "666" &
      substr(possible_ssn, 1, 1) != "9" &
      substr(possible_ssn, 4, 5) != "00" &
      substr(possible_ssn, 6, 9) != "0000" &
      possible_ssn %nin% c("111111111", "222222222", "333333333",
                  "444444444", "555555555", "666666666",
                  "777777777", "888888888", "999999999")) {
    valid_ssns <- c(possible_ssn, valid_ssns)
  }
}

invalid_ssns <- c(NA, "xxx021232", 9123, 92415182, 000342842, 
                  666284132, 912358284, 382004382,
                  729440000, 888888888, 890123456)

while (length(invalid_ssns) < nrow(ssns) - sum(ssns$valid)) {
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


words_for_names <- setNames(
  data.frame(c(
    syn("animal")[n_words(syn("animal")) <= 1],
    syn("color")[n_words(syn("color")) <= 1],
    syn("vegetable")[n_words(syn("vegetable")) <= 1],
    syn("herb")[n_words(syn("herb")) <= 1],
    syn("fruit")[n_words(syn("fruit")) <= 1],
    syn("jewel")[n_words(syn("jewel")) <= 1],
    syn("star")[n_words(syn("star")) <= 1],
    syn("fabric")[n_words(syn("fabric")) <= 1]
  )), c("word")) %>%
  distinct() %>%
  filter(
    !nsfw_word(word)
  ) %>%
  # left_join(hash_grady_pos, by = "word") %>%
  mutate(word = str_to_title(word))

hashed_client <- Client %>%
  left_join(ssns, by = "SSN") %>%
  mutate(SSN = generated_ssns) %>%
  select(-c(sequential, valid, generated_ssns)) %>%
  mutate(new_FirstName = sample(words_for_names$word, nrow(hashed_client), replace = TRUE),
         new_MiddleName = sample(words_for_names$word, nrow(hashed_client), replace = TRUE),
         new_LastName = sample(words_for_names$word, nrow(hashed_client), replace = TRUE),
         FirstInitial = sample(str_to_upper(letters), nrow(hashed_client), replace = TRUE),
         MiddleInitial = sample(str_to_upper(letters), nrow(hashed_client), replace = TRUE),
         LastInitial = sample(str_to_upper(letters), nrow(hashed_client), replace = TRUE),
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
  mutate(days_to_shift = sample(-730:730, nrow(hashed_client), replace = TRUE),
         DOB = if_else(ymd(EntryDate) - ymd(DOB + days_to_shift) < 0,
                             DOB - days_to_shift / 2,
                             DOB + days_to_shift)) %>%
  select(-c(EntryDate, days_to_shift))

start_date <- min(Exit$ExitDate)

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
  
picking_projects <- Project %>%
  left_join(Enrollment %>%
              left_join(Event %>%
                          group_by(EnrollmentID) %>%
                          summarize(events = n()),
                        by = "EnrollmentID") %>%
              group_by(ProjectID) %>%
              summarize(enrollments = n(),
                        last_entry = max(EntryDate),
                        events = sum(events, na.rm = TRUE)),
            by = "ProjectID") %>%
  filter((is.na(OperatingEndDate) |
           OperatingEndDate > start_date) &
           ContinuumProject == 1 &
           HMISParticipatingProject == 1) %>%
  inner_join(Funder %>%
              filter((is.na(EndDate) |
                       EndDate > start_date) &
                       Funder != 34) %>%
              select(ProjectID, Funder) %>%
              distinct(), 
            by = "ProjectID") %>%
  left_join(funding_sources, by = "Funder") %>%
  select(ProjectID, ProjectName, ProjectType, enrollments, events, last_entry, FunderName)

if (programs_to_get == "all") {
  project_matrix <- matrix(
    c(1386,	"DataLab - Coordinated Entry",
      1362,	"DataLab - ES-EE ESG I",
      549,	"DataLab - ES-EE ESG II (with CE elements)",
      1210,	"DataLab - ES-EE RHY",
      # 5,	"DataLab - ES-NbN ESG",
      1409,	"DataLab - HP ESG",
      780,	"DataLab - PSH CoC I",
      # 8,	"DataLab - PSH HOWPA",
      1487,	"DataLab - PSH VASH",
      1428,	"DataLab - RRH CoC I",
      1495,	"DataLab - RRH CoC II",
      1060,	"DataLab - RRH ESG I",
      1351,	"DataLab - RRH VA",
      # 14,	"Datalab - SH VA-HCHV",
      # 15,	"DataLab - SO CoC",
      1419,	"DataLab - SO ESG",
      1321,	"DataLab - SO PATH",
      # 18,	"DataLab - SSO CoC",
      388,	"DataLab - TH CoC",
      340,	"DataLab - TH ESG",
      # 21,	"DataLab - TH HOPWA",
      450,	"DataLab - TH RHY",
      1453,	"DataLab - TH VA"
      # 24,	"DataLab - TH YHDP"
    ), ncol = 2, byrow = TRUE
  )
} elseif (programs_to_get == "nbn") {
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


Project <- trunc_userid(datalab_projects)

ProjectCoC <- trunc_userid(ProjectCoC) %>%
  filter(ProjectID %in% Project$ProjectID) 

Enrollment <- trunc_userid(Enrollment) %>%
  filter(ProjectID %in% Project$ProjectID) 

sampled_enrollments <- Enrollment %>% 
  group_by(ProjectID) %>% 
  slice_sample(n=25)

Enrollment <- trunc_userid(Enrollment) %>%
  filter(HouseholdID %in% sampled_enrollments$HouseholdID)

Funder <- trunc_userid(Funder) %>%
  filter(ProjectID %in% Project$ProjectID) 

Inventory <- trunc_userid(Inventory) %>%
  filter(ProjectID %in% Project$ProjectID) 

Organization <- trunc_userid(Organization) %>%
  filter(OrganizationID %in% Project$OrganizationID) 

Client <- trunc_userid(hashed_client) %>%
  filter(PersonalID %in% Enrollment$PersonalID)

Services <- trunc_userid(Services) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

Assessment <- trunc_userid(Assessment) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

Exit <- trunc_userid(Exit) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

CurrentLivingSituation <- trunc_userid(CurrentLivingSituation) %>%
  filter(EnrollmentID %in% Enrollment$EnrollmentID) 

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

User <- User %>%
  mutate(UserID = substr(UserID, 1, 2),
         UserFirstName = sample(words_for_names$word, nrow(User)),
         UserLastName = sample(words_for_names$word, nrow(User)),
         UserPhone = NA,
         UserEmail = NA
  ) %>%
  group_by(UserID) %>%
  slice(1L) %>%
  ungroup()

# write to zipped folder
{
  for (file in names(hmis_csvs)) {
    write.csv(get(file), file.path(paste0("created_files/", file, ".csv")), row.names=FALSE)
  }
  
  archive_write_dir(paste0("DataLab - Hashed CSVs.zip"),
                    paste0(getwd(), "/created_files"))
  
  unlink(paste0(getwd(), "/created_files/*"))
}
