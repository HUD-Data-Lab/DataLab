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

source("datalab_functions.R")

new_dir <-  choose.dir()
compare_to_dir <- choose.dir()

new_zips <- list.files(new_dir)[seq(1, length(list.files(new_dir)), by = 2)]
compare_to_zips <- list.files(compare_to_dir)[seq(1, length(list.files(compare_to_dir)), by = 2)]

differences <- data.frame(Report = character(), 
                          File = character(),
                          Difference = character())

for (report in union(new_zips, compare_to_zips)) {
  if (report %nin% new_zips) {
    differences <- rbind(differences,
                         c(report, "", "report missing from new directory"))
  } else if (report %nin% compare_to_zips) {
    differences <- rbind(differences,
                         c(report, "", "report missing from old directory"))
  } else {
  
    new_file_names <- unzip(paste0(new_dir, "\\", report), list = TRUE)$Name
    compare_to_file_names <- unzip(paste0(compare_to_dir, "\\", report), list = TRUE)$Name
    
    for (question in union(new_file_names, compare_to_file_names)) {
      if (question %nin% new_file_names) {
        difference <- "question missing from new files"
      } else if (question %nin% compare_to_file_names) {
        difference <- "question missing from old files"
      } else {
        new_data <- suppressMessages(
          read_csv(unzip(paste0(new_dir, "\\", report), question),
                   show_col_types = FALSE))
        compare_to_data <- suppressMessages(
          read_csv(unzip(paste0(compare_to_dir, "\\", report), question),
                   show_col_types = FALSE))
        file.remove(question)
        
        difference <- !isTRUE(all.equal(new_data, compare_to_data))
        
        if (difference) {
          assign(paste0(question, "_N_", report), new_data)
          assign(paste0(question, "_CT_", report), compare_to_data)
        }
        
      }
      
      if (difference != FALSE) {
        differences <- rbind(differences,
                             c(report, question, difference))
      }
    }
  }
}

write.csv(differences, paste0("differences ", Sys.Date(), ".csv"))
