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

#source("datalab_functions.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/datalab_functions.R")
#source("DataLab_Lists.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_Lists.R")

file_source <- file.choose()

for (file in names(hmis_csvs_fy26)){
  
  data <- read_csv(utils::unzip(file_source, paste0(file, ".csv")),
                   col_types = get(file, hmis_csvs_fy26))
  
  if (exists(file)) {
    data <- get(file) %>%
      full_join(data, by = intersect(colnames(get(file)),
                                     colnames(data)))
  } 
  
  assign(file, data)
  
  file.remove(paste0(file, ".csv"))
}

Enrollment <- Enrollment %>%
  # select(-c("TranslationNeeded", "PreferredLanguage", "PreferredLanguageDifferent"))
  select(-c("LastPermanentStreet", "LastPermanentCity", "LastPermanentZIP" ))
Services <- Services %>%
  select(-InformationDate)

source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_hc_variables.R")
