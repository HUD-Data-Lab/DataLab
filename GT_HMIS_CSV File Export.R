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

kit_type <- "new_kit" # can switch from new_kit to old_kit
combining_files <- kit_type == "old_kit"

source("DataLab.R")
source("datalab_functions.R")
source("DataLab_Lists.R")

folder_name <- paste("HUD CSV Export", format(Sys.Date(), "%m.%d.%y")) #Creates a new folder with current date

if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

if (!dir.exists(paste0(folder_name, "/HMIS CSVs"))) {
  dir.create(paste0(folder_name, "/HMIS CSVs"))
}

general_wd <- getwd()
setwd(paste0(general_wd, "/", folder_name))
CSV_File_name <- names(hmis_csvs)

for (theFilename in CSV_File_name) {
  to_write <- get(theFilename)
  
  write_csv(to_write, file =  file.path(paste0("HMIS CSVs/", theFilename, ".csv")), na ="")
}

archive_write_dir("HMIS_CSV_export_sample.zip",paste0(general_wd, "/", folder_name, "/HMIS CSVs")) #Zips all the CSV export files into one folder
unlink("HMIS_CSV_export_sample.zip") #Not sure what this does, added it because it was part of the example in ?archive_write_dir

