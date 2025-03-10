
# selection <- Organization %>% 
#   select(OrganizationID, OrganizationName) %>% 
#   filter(OrganizationID %in% enough_types$OrganizationID) %>%
#   left_join(Project %>% 
#               filter(ProjectType %in% c(0, 1, 2, 3, 13)) %>%
#               select(ProjectID, ProjectName, OrganizationID), 
#             by = "OrganizationID") %>% 
#   left_join(Enrollment %>%
#   # left_join(get_enrollments %>% 
#               select(EnrollmentID, ProjectID),
#             by = "ProjectID") %>%
#   left_join(Exit %>%
#               select(ExitID, EnrollmentID),
#             by = "EnrollmentID") %>%
#   group_by(OrganizationName, ProjectName, ProjectID) %>%
#   summarize(enrollments = n_distinct(EnrollmentID),
#             exits = n_distinct(ExitID)) %>%
#   filter(enrollments >= 10) %>%
#   arrange(OrganizationName, ProjectName)
# 
# enough_types <- Project %>% 
#   filter(ProjectType %in% c(0, 1, 2, 3, 13)) %>%
#   left_join(Enrollment %>% 
#               select(EnrollmentID, ProjectID),
#             by = "ProjectID") %>%
#   group_by(OrganizationID, ProjectName, ProjectType) %>%
#   summarize(enrollments = n_distinct(EnrollmentID)) %>%
#   filter(enrollments >= 20) %>%
#   group_by(OrganizationID) %>%
#   summarise(types = n_distinct(ProjectType)) %>%
#   filter(types >= 4)

#source("datalab_functions.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/datalab_functions.R")
#source("DataLab_Lists.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_Lists.R")

file_source <- file.choose()

for (file in names(hmis_csvs_fy24)){
  
  data <- read_csv(utils::unzip(file_source, paste0(file, ".csv")),
                   col_types = get(file, hmis_csvs_fy24))
  
  if (exists(file)) {
    data <- get(file) %>%
      full_join(data, by = intersect(colnames(get(file)),
                                     colnames(data)))
  } 
  
  if("UserID" %in% colnames(data)) {
    data <- data %>%
      mutate(UserID = 1)
  }
  
  assign(file, data)
  
  file.remove(paste0(file, ".csv"))
}

User <- User %>%
  slice_head(n = 1)

CurrentLivingSituation <- CurrentLivingSituation %>%
  slice_head(n = 0)

new_orgs <- Organization %>%
  slice_head(n = 5)
  # slice_head(n = 1)

selection <- Project %>% 
  filter(ProjectType %in% c(0, 2, 3, 13)) %>%
  # filter(ProjectType == 4) %>%
  select(ProjectID, ProjectName, ProjectType) %>% 
  left_join(Enrollment %>%
              # left_join(get_enrollments %>% 
              select(EnrollmentID, ProjectID),
            by = "ProjectID") %>%
  left_join(Exit %>%
              select(ExitID, EnrollmentID),
            by = "EnrollmentID") %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarize(enrollments = n_distinct(EnrollmentID),
            exits = n_distinct(ExitID)) %>%
  filter(enrollments >= 20) %>%
  ungroup() %>%
  group_by(ProjectType) %>%
  # slice_head(n = 5) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  # arrange(ProjectName) %>%
  mutate(new_OrganizationID = rep(new_orgs$OrganizationID, 4),
  # mutate(new_OrganizationID = new_orgs$OrganizationID,
         project_type_string = case_when(
           ProjectType == 0 ~ "ES",
           ProjectType == 2 ~ "TH",
           ProjectType == 3 ~ "PSH",
           ProjectType == 4 ~ "SO",
           ProjectType == 13 ~ "RRH")) %>%
  left_join(Organization %>%
              select(OrganizationID, OrganizationName),
            by = c("new_OrganizationID" = "OrganizationID")) %>%
  mutate(new_ProjectName = paste(
    OrganizationName, "-",
    project_type_string)) 

Project <- Project %>%
  inner_join(selection %>%
              select(ProjectID, new_ProjectName, new_OrganizationID),
            by = "ProjectID") %>%
  mutate(ProjectName = new_ProjectName,
         OrganizationID = new_OrganizationID,
         ProjectCommonName = str_replace(ProjectName, "anization", "")) %>%
  select(-c(new_ProjectName, new_OrganizationID)) 

get_enrollments <- Enrollment %>%
  filter(ProjectID %in% selection$ProjectID) %>%
  group_by(ProjectID) %>%
  slice_head(n = 50)

Enrollment <- Enrollment %>%
  filter(HouseholdID %in% get_enrollments$HouseholdID)


library(archive)

folder_name <- paste("Test Kit", format(Sys.Date(), "%m.%d.%y"))

if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

if (!dir.exists("created_files")) {
  dir.create("created_files")
}

if (!dir.exists("created_files_2")) {
  dir.create("created_files_2")
}

if (!dir.exists(paste0(folder_name, "/HMIS CSVs"))) {
  dir.create(paste0(folder_name, "/HMIS CSVs"))
}

write_csvs_for(Project$ProjectID, zip_title = "Mini Zip Set - One SO",
               write_to <- paste0(folder_name, "/HMIS CSVs"))
