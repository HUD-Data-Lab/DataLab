spec_file_source <- file.choose()

for (file in names(hmis_csvs_fy24)){
  
  data <- read_csv(utils::unzip(file_source, paste0(file, ".csv")),
                   show_col_types = FALSE) 
  
  headers <- data.frame(
    file_name = rep(file, length(colnames(data))),
    header = colnames(data))
  
  if (file == names(hmis_csvs_fy24)[1]) {
    all_headers <- headers
  } else {
    all_headers <- all_headers %>%
      union(headers)
  }
  
  file.remove(paste0(file, ".csv"))
}

testing_file_source <- file.choose()

for (file in names(hmis_csvs_fy24)){
  
  data <- read_csv(utils::unzip(file_source, paste0(file, ".csv")),
                   show_col_types = FALSE) 
  
  headers <- data.frame(
    file_name = rep(file, length(colnames(data))),
    header = colnames(data))
  
  if (file == names(hmis_csvs_fy24)[1]) {
    test_headers <- headers
  } else {
    test_headers <- test_headers %>%
      union(headers)
  }
  
  file.remove(paste0(file, ".csv"))
}

combination <- all_headers %>%
  mutate(original = "x") %>%
  full_join(test_headers %>%
              mutate(testing = "x"),
            by = colnames(all_headers)) %>%
  filter(is.na(original) | 
           is.na(testing))

write.table(combination, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
