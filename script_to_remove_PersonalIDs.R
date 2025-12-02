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

library(archive)
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/datalab_functions.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_Lists.R")

# select the HMIS zip file you need to remove people from
hmis_csv <- file.choose()
# select an .xlsx or .csv file with a PersonalID column listing the people who need to be removed
people_to_remove_file <- file.choose()
# select the folder you'd like the cleaned zip file to be written to
save_to_location <- choose.dir()

if (!grepl("csv", tolower(people_to_remove_file)) &
    !grepl("xlsx", tolower(people_to_remove_file))) {
  warning("Please provide a .csv or .xlsx file with a PersonalID column listing the people who need to be removed")
} else if (grepl("csv", tolower(people_to_remove_file))) {
  people_to_remove <- read_csv(people_to_remove_file, show_col_types = FALSE)
} else {
  people_to_remove <- readxl::read_excel(people_to_remove_file)
}

if ("PersonalID" %in% colnames(people_to_remove)) {
  people_to_remove_list <- people_to_remove$PersonalID
} else {
  warning("No PersonalID column is present in provided spreadsheet")
}

for (file in names(hmis_csvs_fy26)){
  
  data <- read_csv(utils::unzip(hmis_csv, paste0(file, ".csv")),
                   col_types = get(file, hmis_csvs_fy26))
  
  if ("PersonalID" %in% colnames(data)) {
    data <- data %>% 
      filter(PersonalID %nin% people_to_remove_list)
  } else if ("EnrollmentID" %in% colnames(data)) {
    data <- data %>% 
      filter(EnrollmentID %in% Enrollment$EnrollmentID)
  }
  
  assign(file, data)
  file.remove(paste0(file, ".csv"))
}

for (file in unique(CSV_columns$File)) {
  write.csv(get(file) %>%
              mutate(
                across(colnames(file)[str_locate_all(pattern = "T",
                                                     get(file,
                                                         hmis_csvs_fy26))[[1]][,1]],
                       ~ format(., format = "%Y-%m-%d %H:%M:%S")),
                across(colnames(file)[str_locate_all(pattern = "D",
                                                     get(file,
                                                         hmis_csvs_fy26))[[1]][,1]],
                       ~ format(., format = "%Y-%m-%d")),
                across(colnames(file)[str_locate_all(pattern = "d",
                                                     get(file,
                                                         hmis_csvs_fy26))[[1]][,1]],
                       ~ if_else(. > 0, sprintf("%.2f", .), NA))),
            file.path(paste0(save_to_location, "/", file, ".csv")),
            row.names=FALSE, na = "",
            quote = which(as.character(lapply(get(file), class)) %nin%
                            c("integer", "numeric")))
}

archive_write_dir(paste0(save_to_location, "/HMIS CSV - People Removed - ", Sys.Date(), ".zip"),
                  save_to_location)

unlink(list.files(save_to_location, full.names = TRUE)[list.files(save_to_location) != 
                                                         paste0("HMIS CSV - People Removed - ", Sys.Date(), ".zip")])

