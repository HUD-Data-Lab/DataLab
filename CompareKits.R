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
library(readr)

##  zips logic
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
        
        # this compares just the numbers
        new_data <- unname(new_data[2:nrow(new_data), 2:ncol(new_data)])
        compare_to_data <- unname(compare_to_data[2:nrow(compare_to_data), 2:ncol(compare_to_data)])
        
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


##  workbook logic
new_file <-  file.choose()
compare_to_file <- file.choose()

new_sheets <- excel_sheets(new_file)
compare_to_sheets <- excel_sheets(compare_to_file)

differences <- data.frame(Tab = character(), 
                          Difference = character())

for (tab in union(new_sheets, compare_to_sheets)) {
  if (tab %nin% new_sheets) {
    differences <- rbind(differences,
                         c(tab, "", "tab missing from new file"))
  } else if (tab %nin% compare_to_sheets) {
    differences <- rbind(differences,
                         c(tab, "", "tab missing from old file"))
  } else {

    new_data <- suppressMessages(read_xlsx(new_file, tab))
    compare_to_data <- suppressMessages(read_xlsx(compare_to_file, tab))
    
    # this compares just the numbers
    # new_data <- unname(new_data[2:nrow(new_data), 2:ncol(new_data)])
    # compare_to_data <- unname(compare_to_data[2:nrow(compare_to_data), 2:ncol(compare_to_data)])
    
    difference <- !isTRUE(all.equal(new_data, compare_to_data))
    
    if (difference) {
      assign(paste0(tab, "_N"), new_data)
      assign(paste0(tab, "_CT"), compare_to_data)
    }
    
    if (difference != FALSE) {
      differences <- rbind(differences,
                           c(tab, difference))
    }
  }
}

write.csv(differences, paste0("differences ", Sys.Date(), ".csv"))


##  scratchwork
new_tables <- ls()[sapply(ls(),function(t) is.data.frame(get(t))) &
                    str_detect(ls(), "csv_N_APR")]

for (i in 1:length(new_tables)) {
  if(is.na(sum(get(new_tables[i])[1]))) {
    print(
      paste(
        new_tables[i],
        sum(get(new_tables[i])[1])
      )
    )
  }
}

