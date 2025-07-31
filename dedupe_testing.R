library(tidyverse)
library(fedmatch)

client <- read.csv(file.choose())

client <- client[sample(1:nrow(client)),]

gender_columns <- c("Woman", "Man", "NonBinary", "CulturallySpecific", 
                    "Transgender", "Questioning", "DifferentIdentity", 
                    "GenderNone")
columns_to_keep <- c("PersonalID", "SSN", "DOB", "FullName",
                     gender_columns)
                     
num_groups <- round(nrow(client) / 2000)

client_1 <- client %>%
  mutate(FullName = paste(FirstName, LastName)) %>%
  select(all_of(columns_to_keep))

client_2 <- client_1 %>%
  `colnames<-`(paste0(columns_to_keep, "_2"))

hold_2 <- client_2 %>%
  mutate(SSN_2 = str_sub(SSN_2,-4)) %>%
  group_by((row_number() - 1) %/% (n() / num_groups)) %>%
  nest %>% pull(data)

########################
##  original version  ##
########################

{
  hold_1 <- client_1 %>%
    mutate(SSN = str_sub(SSN,-4)) %>%
    group_by((row_number() - 1) %/% (n() / num_groups)) %>%
    nest %>% pull(data)
  
  for (group in 1:(num_groups - 1)) {
    for (second_group in group:num_groups) {
      fuzzy_name_result <- merge_plus(
        data1 = hold_1[[group]],
        data2 = hold_2[[second_group]],
        by.x = "FullName",
        by.y = "FullName_2",
        match_type = "fuzzy",
        unique_key_1 = "PersonalID",
        unique_key_2 = "PersonalID_2"
      )
      
      fuzzy_SSN_result <- merge_plus(
        data1 = hold_1[[group]],
        data2 = hold_2[[second_group]],
        by.x = "SSN",
        by.y = "SSN_2",
        match_type = "fuzzy",
        unique_key_1 = "PersonalID",
        unique_key_2 = "PersonalID_2"
      )
      
      fuzzy_DOB_result <- merge_plus(
        data1 = hold_1[[group]],
        data2 = hold_2[[second_group]],
        by.x = "DOB",
        by.y = "DOB_2",
        match_type = "fuzzy",
        unique_key_1 = "PersonalID",
        unique_key_2 = "PersonalID_2"
      )
      
      matches <- fuzzy_name_result$matches %>%
        select(PersonalID, PersonalID_2) %>%
        mutate(weight = 2) %>%
        union_all(
          fuzzy_SSN_result$matches %>%
            select(PersonalID, PersonalID_2) %>%
            mutate(weight = 1)
        ) %>%
        union_all(
          fuzzy_DOB_result$matches %>%
            select(PersonalID, PersonalID_2) %>%
            mutate(weight = 1)
        ) %>%
        filter(PersonalID != PersonalID_2) %>%
        group_by(PersonalID, PersonalID_2) %>%
        summarise(matches = sum(weight)) %>%
        filter(matches > 2)
      
      if (exists("all_matches")) {
        all_matches <- all_matches %>%
          union(matches)
      } else {
        all_matches <- matches
      }
      
    }
  }
}

########################
##  sampling version  ##
########################

{
  percent_to_include <- 10
  
  hold_1 <-
    client_1[1:round(nrow(client) * (percent_to_include / 100)),] %>%
    mutate(SSN = str_sub(SSN,-4))
  
  for (group in 1:(num_groups - 1)) {
    
    fuzzy_name_result <- merge_plus(
      data1 = hold_1,
      data2 = hold_2[[group]],
      by.x = "FullName",
      by.y = "FullName_2",
      match_type = "fuzzy",
      unique_key_1 = "PersonalID",
      unique_key_2 = "PersonalID_2"
    )
    
    matched_names <- hold_2[[group]] %>%
      filter(PersonalID_2 %in% fuzzy_name_result$matches$PersonalID_2)
    
    fuzzy_SSN_result <- merge_plus(
      data1 = hold_1,
      data2 = matched_names,
      by.x = "SSN",
      by.y = "SSN_2",
      match_type = "fuzzy",
      unique_key_1 = "PersonalID",
      unique_key_2 = "PersonalID_2"
    )
    
    fuzzy_DOB_result <- merge_plus(
      data1 = hold_1,
      data2 = matched_names,
      by.x = "DOB",
      by.y = "DOB_2",
      match_type = "fuzzy",
      unique_key_1 = "PersonalID",
      unique_key_2 = "PersonalID_2"
    )
    
    matches <- fuzzy_name_result$matches %>%
      select(PersonalID, PersonalID_2) %>%
      mutate(weight = 2) %>%
      union_all(
        fuzzy_SSN_result$matches %>%
          select(PersonalID, PersonalID_2) %>%
          mutate(weight = 1)
      ) %>%
      union_all(
        fuzzy_DOB_result$matches %>%
          select(PersonalID, PersonalID_2) %>%
          mutate(weight = 1)
      ) %>%
      filter(PersonalID != PersonalID_2) %>%
      group_by(PersonalID, PersonalID_2) %>%
      summarise(matches = sum(weight)) %>%
      filter(matches > 2)
    
    if (exists("all_matches")) {
      all_matches <- all_matches %>%
        union(matches)
    } else {
      all_matches <- matches
    }
  }
}


###########################
##  new version 2.27.25  ##
###########################

{
  percent_to_include <- 10
  
  # Subset client_1 to a percentage and process SSN
  hold_1 <- client_1 %>%
    slice(1:round(nrow(client) * (percent_to_include / 100))) %>%
    mutate(SSN = str_sub(SSN, -4))
  
  # Function to perform fuzzy matching for a given group index
  match_function <- function(group_idx) {
    # Fuzzy matching on FullName
    fuzzy_name <- merge_plus(
      data1 = hold_1,
      data2 = hold_2[[group_idx]],
      by.x = "FullName",
      by.y = "FullName_2",
      match_type = "fuzzy",
      unique_key_1 = "PersonalID",
      unique_key_2 = "PersonalID_2"
    )
    
    gender_matching <- fuzzy_name$matches %>%
      filter(0 <
               if_else(is.na(Woman / Woman_2), 0, 1) +
               if_else(is.na(Man / Man_2), 0, 1) +
               if_else(is.na(NonBinary / NonBinary_2), 0, 1) +
               if_else(is.na(CulturallySpecific / CulturallySpecific_2), 0, 1) +
               if_else(is.na(Transgender / Transgender_2), 0, 1) +
               if_else(is.na(Questioning / Questioning_2), 0, 1) +
               if_else(is.na(DifferentIdentity / DifferentIdentity_2), 0, 1) +
               if_else(is.na(GenderNone / GenderNone_2), 0, 1))
    print(paste("agree rate", round(nrow(gender_matching) / nrow(fuzzy_name$matches)
                              * 100, 2)))
    
    # Fuzzy matching on SSN
    fuzzy_ssn <- merge_plus(
      data1 = hold_1,
      data2 = hold_2[[group_idx]],
      by.x = "SSN",
      by.y = "SSN_2",
      match_type = "fuzzy",
      unique_key_1 = "PersonalID",
      unique_key_2 = "PersonalID_2"
    )
    
    # Fuzzy matching on DOB
    fuzzy_dob <- merge_plus(
      data1 = hold_1,
      data2 = hold_2[[group_idx]],
      by.x = "DOB",
      by.y = "DOB_2",
      match_type = "fuzzy",
      unique_key_1 = "PersonalID",
      unique_key_2 = "PersonalID_2"
    )
    
    # Combine matches with respective weights and filter/summarise
    bind_rows(
      fuzzy_name$matches %>% select(PersonalID, PersonalID_2) %>% mutate(weight = 2),
      gender_matching  %>% select(PersonalID, PersonalID_2) %>% mutate(weight = 1),
      fuzzy_ssn$matches  %>% select(PersonalID, PersonalID_2) %>% mutate(weight = 1),
      fuzzy_dob$matches  %>% select(PersonalID, PersonalID_2) %>% mutate(weight = 1)
    ) %>%
      filter(PersonalID != PersonalID_2) %>%
      group_by(PersonalID, PersonalID_2) %>%
      summarise(matches = sum(weight), .groups = "drop") %>%
      filter(matches > 3)
  }
  
  # Apply matching function to all groups (1 to num_groups-1) and combine results
  all_matches_1 <- map_dfr(1:(num_groups - 1), match_function)
}

###################################################

review_matches <- all_matches %>%
  left_join(client_1, by = "PersonalID") %>%
  left_join(client_2, by = "PersonalID_2")


identified <- nrow(all_matches_1) / nrow(hold_1)
