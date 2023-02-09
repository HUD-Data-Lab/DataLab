library(tidyverse)
library(fedmatch)

client <- read.csv(file.choose())

num_groups <- 20
columns_to_keep <- c("PersonalID", "SSN", "DOB", "FullName")

client_1 <- client %>%
  mutate(FullName = paste(FirstName, LastName)) %>%
  select(all_of(columns_to_keep))

client_2 <- client_1 %>%
  `colnames<-`(paste0(columns_to_keep, "_2")) 

hold_1 <- client_1 %>% 
  mutate(SSN = str_sub(SSN, -4)) %>%
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)

hold_2 <- client_2 %>%
  mutate(SSN_2 = str_sub(SSN_2, -4)) %>%
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)


for (group in 1:(num_groups - 1)) {
  for (second_group in group:num_groups) {
    fuzzy_name_result <- merge_plus(data1 = hold_1[[group]], 
                                    data2 = hold_2[[second_group]],
                                    by.x = "FullName",
                                    by.y = "FullName_2", match_type = "fuzzy", 
                                    unique_key_1 = "PersonalID",
                                    unique_key_2 = "PersonalID_2")
    
    fuzzy_SSN_result <- merge_plus(data1 = hold_1[[group]], 
                                   data2 = hold_2[[second_group]],
                                   by.x = "SSN",
                                   by.y = "SSN_2", match_type = "fuzzy", 
                                   unique_key_1 = "PersonalID",
                                   unique_key_2 = "PersonalID_2")
    
    fuzzy_DOB_result <- merge_plus(data1 = hold_1[[group]], 
                                   data2 = hold_2[[second_group]],
                                   by.x = "DOB",
                                   by.y = "DOB_2", match_type = "fuzzy", 
                                   unique_key_1 = "PersonalID",
                                   unique_key_2 = "PersonalID_2")
    
    matches <- fuzzy_name_result$matches %>%
      select(PersonalID, PersonalID_2) %>%
      mutate(weight = 2) %>%
      union_all(fuzzy_SSN_result$matches %>%
                  select(PersonalID, PersonalID_2) %>%
                  mutate(weight = 1)) %>%
      union_all(fuzzy_DOB_result$matches %>%
                  select(PersonalID, PersonalID_2) %>%
                  mutate(weight = 1)) %>%
      filter(PersonalID != PersonalID_2) %>%
      group_by(PersonalID, PersonalID_2) %>%
      summarise(matches = sum(weight)) %>%
      filter(matches > 2)
      
    if(exists("all_matches")) {
      all_matches <- all_matches %>%
        union(matches)
    } else {
      all_matches <- matches
    }
      
  }
}

review_matches <- all_matches %>%
  left_join(client_1, by = "PersonalID") %>%
  left_join(client_2, by = "PersonalID_2")


