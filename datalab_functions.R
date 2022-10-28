library(janitor)
library(data.table)
library(lubridate)
library(tidyverse)

`%nin%` = Negate(`%in%`)

trunc_userid <- function(df) {
  df %>%
    mutate(UserID = substr(UserID, 1, 2))
}



# identifies words in list to exclude
# recommend leaving this collapsed
nsfw_word <- function(test_word) {
  test_word %in% profanity_alvarez |
    test_word %in% profanity_arr_bad |
    test_word %in% profanity_banned |
    test_word %in% profanity_racist |
    test_word %in% profanity_zac_anger |
    test_word %in% c("ass", "concupiscent", "lascivious", "lecherous", 
                     "lewd", "libidinous", "lubricious", "lustful", 
                     "man-eater", "obscene", "orgiastic", "priapic", 
                     "randy", "negro", "cannibal") |
    str_detect(test_word, "beast") |
    str_detect(test_word, "brut") |
    str_detect(test_word, "erot") |
    str_detect(test_word, "flesh") |
    str_detect(test_word, "sex") |
    str_detect(test_word, "christ") |
    str_detect(test_word, "breed") |
    str_detect(test_word, "dead") |
    str_detect(test_word, "maniac") |
    str_detect(test_word, "murder") |
    str_detect(test_word, "pervert") |
    str_detect(test_word, "phile") |
    str_detect(test_word, "blood") |
    str_detect(test_word, "demon") |
    str_detect(test_word, "devil") |
    str_detect(test_word, "dyk")
}

sequential_ssn <- function(test_ssn) {
  if (suppressWarnings(is.na(as.numeric(test_ssn)))) {
    TRUE
  } else {
    for (i in 1:nchar(test_ssn)) {
      if (i < nchar(test_ssn)) {
        j <- i + 1
        diff <- abs(
          as.numeric(substr(test_ssn, j, j)) - as.numeric(substr(test_ssn, i, i))
        )
        if (i == 1) {
          diff_list <- diff
        }
        else {
          diff_list <- c(diff_list, diff)
        }
      }
    }
    sum(diff_list == 1) == 8 |
      (sum(diff_list == 1) == 7 &
         sum(diff_list %in% c(8,9)) == 1)
  }
}

# valid cocs
{
  valid_cocs <- c(
    "AK-500",	"GA-502",	"MN-505",	"OR-501",	"CA-531",	"LA-509",	"NJ-513",	"TX-624",
    "AK-501",	"GA-503",	"MN-506",	"OR-502",	"CA-600",	"MA-500",	"NJ-514",	"TX-700",
    "AL-500",	"GA-504",	"MN-508",	"OR-503",	"CA-601",	"MA-502",	"NJ-515",	"TX-701",
    "AL-501",	"GA-505",	"MN-509",	"OR-504",	"CA-602",	"MA-503",	"NJ-516",	"UT-500",
    "AL-502",	"GA-506",	"MN-511",	"OR-505",	"CA-603",	"MA-504",	"NM-500",	"UT-503",
    "AL-503",	"GA-507",	"MO-500",	"OR-506",	"CA-604",	"MA-505",	"NM-501",	"UT-504",
    "AL-504",	"GA-508",	"MO-501",	"OR-507",	"CA-606",	"MA-506",	"NV-500",	"VA-500",
    "AL-505",	"GU-500",	"MO-503",	"PA-500",	"CA-607",	"MA-507",	"NV-501",	"VA-501",
    "AL-506",	"HI-500",	"MO-600",	"PA-501",	"CA-608",	"MA-509",	"NV-502",	"VA-502",
    "AL-507",	"HI-501",	"MO-602",	"PA-502",	"CA-609",	"MA-510",	"NY-500",	"VA-503",
    "AR-500",	"IA-500",	"MO-603",	"PA-503",	"CA-611",	"MA-511",	"NY-501",	"VA-504",
    "AR-501",	"IA-501",	"MO-604",	"PA-504",	"CA-612",	"MA-515",	"NY-503",	"VA-505",
    "AR-503",	"IA-502",	"MO-606",	"PA-505",	"CA-613",	"MA-516",	"NY-505",	"VA-507",
    "AR-505",	"ID-500",	"MP-500",	"PA-506",	"CA-614",	"MA-519",	"NY-507",	"VA-508",
    "AR-508",	"ID-501",	"MS-500",	"PA-508",	"CO-500",	"MD-501",	"NY-508",	"VA-513",
    "AS-500",	"IL-500",	"MS-501",	"PA-509",	"CO-503",	"MD-502",	"NY-510",	"VA-514",
    "AZ-500",	"IL-501",	"MS-503",	"PA-510",	"CO-504",	"MD-503",	"NY-511",	"VA-521",
    "AZ-501",	"IL-502",	"MT-500",	"PA-511",	"CO-505",	"MD-504",	"NY-512",	"VA-600",
    "AZ-502",	"IL-503",	"NC-500",	"PA-512",	"CT-503",	"MD-505",	"NY-513",	"VA-601",
    "CA-500",	"IL-504",	"NC-501",	"PA-600",	"CT-505",	"MD-506",	"NY-514",	"VA-602",
    "CA-501",	"IL-506",	"NC-502",	"PA-601",	"DC-500",	"MD-509",	"NY-518",	"VA-603",
    "CA-502",	"IL-507",	"NC-503",	"PA-603",	"DE-500",	"MD-511",	"NY-519",	"VA-604",
    "CA-503",	"IL-508",	"NC-504",	"PA-605",	"FL-500",	"MD-513",	"NY-520",	"VI-500",
    "CA-504",	"IL-510",	"NC-505",	"PR-502",	"FL-501",	"MD-514",	"NY-522",	"VT-500",
    "CA-505",	"IL-511",	"NC-506",	"PR-503",	"FL-502",	"MD-601",	"NY-523",	"VT-501",
    "CA-506",	"IL-512",	"NC-507",	"RI-500",	"FL-503",	"ME-500",	"NY-525",	"WA-500",
    "CA-507",	"IL-513",	"NC-509",	"SC-500",	"FL-504",	"MI-500",	"NY-600",	"WA-501",
    "CA-508",	"IL-514",	"NC-511",	"SC-501",	"FL-505",	"MI-501",	"NY-601",	"WA-502",
    "CA-509",	"IL-515",	"NC-513",	"SC-502",	"FL-506",	"MI-502",	"NY-602",	"WA-503",
    "CA-510",	"IL-516",	"NC-516",	"SC-503",	"FL-507",	"MI-503",	"NY-603",	"WA-504",
    "CA-511",	"IL-517",	"ND-500",	"SD-500",	"FL-508",	"MI-504",	"NY-604",	"WA-508",
    "CA-512",	"IL-518",	"NE-500",	"TN-500",	"FL-509",	"MI-505",	"NY-606",	"WI-500",
    "CA-513",	"IL-519",	"NE-501",	"TN-501",	"FL-510",	"MI-506",	"NY-608",	"WI-501",
    "CA-514",	"IL-520",	"NE-502",	"TN-502",	"FL-511",	"MI-507",	"OH-500",	"WI-502",
    "CA-515",	"IN-502",	"NH-500",	"TN-503",	"FL-512",	"MI-508",	"OH-501",	"WI-503",
    "CA-516",	"IN-503",	"NH-501",	"TN-504",	"FL-513",	"MI-509",	"OH-502",	"WV-500",
    "CA-517",	"KS-502",	"NH-502",	"TN-506",	"FL-514",	"MI-510",	"OH-503",	"WV-501",
    "CA-518",	"KS-503",	"NJ-500",	"TN-507",	"FL-515",	"MI-511",	"OH-504",	"WV-503",
    "CA-519",	"KS-505",	"NJ-501",	"TN-509",	"FL-517",	"MI-512",	"OH-505",	"WV-508",
    "CA-520",	"KS-507",	"NJ-502",	"TN-510",	"FL-518",	"MI-514",	"OH-506",	"WY-500",
    "CA-521",	"KY-500",	"NJ-503",	"TN-512",	"FL-519",	"MI-515",	"OH-507",	"OK-506",
    "CA-522",	"KY-501",	"NJ-504",	"TX-500",	"FL-520",	"MI-516",	"OH-508",	"OK-507",
    "CA-523",	"KY-502",	"NJ-506",	"TX-503",	"FL-600",	"MI-517",	"OK-500",	"OR-500",
    "CA-524",	"LA-500",	"NJ-507",	"TX-600",	"FL-601",	"MI-518",	"OK-501",	"MN-502",
    "CA-525",	"LA-502",	"NJ-508",	"TX-601",	"FL-602",	"MI-519",	"OK-502",	"MN-503",
    "CA-526",	"LA-503",	"NJ-509",	"TX-603",	"FL-603",	"MI-523",	"OK-503",	"MN-504",
    "CA-527",	"LA-505",	"NJ-510",	"TX-604",	"FL-604",	"MN-500",	"OK-504",	"GA-500",
    "CA-529",	"LA-506",	"NJ-511",	"TX-607",	"FL-605",	"MN-501",	"OK-505",	"GA-501",
    "CA-530",	"LA-507",	"NJ-512",	"TX-611",	"FL-606"			
  )
}

# provides distinct client counts within households for any given grouping 
return_household_groups <- function(APR_dataframe, grouped_by, missing_group_name = NA) {

  possible_groups <- c(total = 0, without_children = 0, children_and_adults = 0,
                       only_children = 0, unknown = 0)
  
  potential_table <- APR_dataframe %>%
    group_by({{grouped_by}}) %>%
    summarise(total = n_distinct(PersonalID),
              without_children = n_distinct(PersonalID[household_type == "AdultsOnly"]),
              children_and_adults = n_distinct(PersonalID[household_type == "AdultsAndChildren"]),
              only_children = n_distinct(PersonalID[household_type == "ChildrenOnly"]),
              unknown = n_distinct(PersonalID[household_type == "Unknown"])) %>% 
    # I don't remember why I added the next two lines, they seem redundant
    # but I know they mattered at some point? Need to revisit and verify
    pivot_longer(!{{grouped_by}}, names_to = "Group", values_to = "values") %>%
    pivot_wider(names_from = "Group", values_from = "values") %>% 
    add_column(!!!possible_groups[!names(possible_groups) %in% names(.)]) %>%
    select(c({{grouped_by}}, names(possible_groups)))
  
  if(nrow(potential_table) == 0) {
    if(is.na(missing_group_name)) warning("You are missing a row name")
    
    potential_table <- APR_dataframe %>%
      select({{grouped_by}}) %>%
      distinct() %>% 
      add_column(!!!possible_groups[!names(possible_groups) %in% names(.)]) %>%
      select(c({{grouped_by}}, names(possible_groups)))
    
    potential_table[1,1] = missing_group_name
  }
  
  potential_table[is.na(potential_table)] <- 0
  # for(j in seq_along(potential_table)){
  #   set(potential_table, i = which(is.na(potential_table[[j]]) &
  #                                    is.numeric(potential_table[[j]])), j = j, value = 0)
  # }
  
  potential_table %>%
    arrange({{grouped_by}})
}

# gets last Wednesday of January, April, July, and October
# reports longer than a year return the last possible date for each month
create_pit_dates <- function(start_date, end_date) {
  all_days_in_report <- seq(start_date, end_date, "day")
  
  all_days_table <- data.frame(day = all_days_in_report, 
                               month = months(all_days_in_report), 
                               weekday = weekdays(all_days_in_report))
  
  all_days_table <- all_days_table[all_days_table$weekday == "Wednesday",]
  
  aggregate(day ~ month, all_days_table, function(x){tail(x,1)}) %>%
    filter(month %in% c("January", "April", "July", "October")) %>%
    arrange(desc(day)) %>%
    group_by(month) %>%
    slice(1L) %>%
    ungroup()
}

# used in Q13a-c2
condition_count_groups <- function(enrollments_and_conditions) {
  enrollments_and_conditions %>%
    mutate(disability_count_group = if_else(
      is.na(disability_count) | 
        disability_count == 0,
      case_when(
        DisablingCondition == 0 ~ "None",
        DisablingCondition == 1 ~ "Unknown",
        DisablingCondition %in% c(8, 9) ~ "DK/R",
        TRUE ~ "DNC"),
      case_when(
        disability_count == 1 ~ "OneCondition",
        disability_count == 2 ~ "TwoConditions",
        TRUE ~ "ThreeOrMoreConditions")),
      disability_count_group = factor(disability_count_group,
                                      ordered = TRUE,
                                      levels = c("None", "OneCondition", "TwoConditions",
                                                 "ThreeOrMoreConditions", "Unknown",
                                                 "DK/R", "DNC")))
}

determine_total_income <- function(enrollments_and_income, annual = FALSE) {
  
  if(annual) {
    enrollments_and_income <- enrollments_and_income %>%
      left_join(annual_assessment_dates, by = "HouseholdID")
  }
  
  total_income <- enrollments_and_income %>%
    mutate(amounts_combined = if_else(Earned == 1 & EarnedAmount > 0, EarnedAmount, 0) +
             if_else(Unemployment == 1 & UnemploymentAmount > 0, UnemploymentAmount, 0) +
             if_else(SSI == 1 & SSIAmount > 0, SSIAmount, 0) +
             if_else(SSDI == 1 & SSDIAmount > 0, SSDIAmount, 0) +
             if_else(VADisabilityService == 1 & VADisabilityServiceAmount > 0, VADisabilityServiceAmount, 0) +
             if_else(VADisabilityNonService == 1 & VADisabilityNonServiceAmount > 0, VADisabilityNonServiceAmount, 0) +
             if_else(PrivateDisability == 1 & PrivateDisabilityAmount > 0, PrivateDisabilityAmount, 0) +
             if_else(WorkersComp == 1 & WorkersCompAmount > 0, WorkersCompAmount, 0) +
             if_else(TANF == 1 & TANFAmount > 0, TANFAmount, 0) +
             if_else(GA == 1 & GAAmount > 0, GAAmount, 0) +
             if_else(SocSecRetirement == 1 & SocSecRetirementAmount > 0, SocSecRetirementAmount, 0) +
             if_else(Pension == 1 & PensionAmount > 0, PensionAmount, 0) +
             if_else(ChildSupport == 1 & ChildSupportAmount > 0, ChildSupportAmount, 0) +
             if_else(Alimony == 1 & AlimonyAmount > 0, AlimonyAmount, 0) +
             if_else(OtherIncomeSource == 1 & OtherIncomeAmount > 0, OtherIncomeAmount, 0),
           calculated_total_income = case_when(!is.na(TotalMonthlyIncome) &
                                                 TotalMonthlyIncome > 0 ~ TotalMonthlyIncome,
                                               amounts_combined > 0 ~ amounts_combined,
                                               IncomeFromAnySource == 0 |
                                                 (TotalMonthlyIncome <= 0 &
                                                    (is.na(IncomeFromAnySource) |
                                                       IncomeFromAnySource == 1)) ~ 0),
           total_income_group = case_when(
             is.na(calculated_total_income) ~ if_else(
               IncomeFromAnySource %in% c(8, 9), "DK/R", "DNC"),
             calculated_total_income == 0 ~ "No Income",
             calculated_total_income <= 150 ~ "$1 - $150",
             calculated_total_income <= 250 ~ "$151 - $250",
             calculated_total_income <= 500 ~ "$251 - $500",
             calculated_total_income <= 1000 ~ "$501 - $1,000",
             calculated_total_income <= 1500 ~ "$1,001 - $1,500",
             calculated_total_income <= 2000 ~ "$1,501 - $2,000",
             TRUE ~ "$2,001"))
  
  if(annual) {
    total_income <- total_income %>%
      mutate(total_income_group = case_when(
        is.na(annual_due) ~ "No Annual Required",
        !is.na(annual_due) &
          is.na(IncomeBenefitsID) ~ "Required Annual Missing",
        TRUE ~ total_income_group))
  }
  total_income
}

create_income_groups <- function(enrollments_total_income, annual = FALSE) {
  income_categories <- c("No Income", "$1 - $150", "$151 - $250", 
                         "$251 - $500", "$501 - $1,000", 
                         "$1,001 - $1,500", "$1,501 - $2,000", 
                         "$2,001", "DK/R", "DNC")
  
  annual_income_categories <- c(income_categories, "No Annual Required", "Required Annual Missing")
  
  if(annual) {
    income_categories <- annual_income_categories
  }
  
  income_groups <- enrollments_total_income %>%
    group_by(total_income_group) %>%
    summarise(Income = n_distinct(PersonalID)) %>%
    full_join(as.data.frame(income_categories) %>%
                `colnames<-`(c("total_income_group")),
              by = "total_income_group") %>%
    mutate(total_income_group = factor(total_income_group, ordered = TRUE,
                                       levels = annual_income_categories))
  
  income_groups[is.na(income_groups)] <- 0
  
  income_groups %>%
    arrange(total_income_group)
}

get_annual_id <- function(enrollments, assessments, assessment_identifier) {
  
  annual_data <- enrollments %>%
    inner_join(assessments %>%
                 filter(DataCollectionStage == 5) %>%
                 select(EnrollmentID, InformationDate, {{assessment_identifier}}),
               by = c("EnrollmentID" = "EnrollmentID")) %>%
    inner_join(annual_assessment_dates, by = "HouseholdID") %>%
    filter(trunc((annual_due %--% InformationDate) / days(1)) >= -30 &
             trunc((annual_due %--% InformationDate) / days(1)) <= 30) %>%
    arrange(desc(InformationDate)) %>%
    group_by(EnrollmentID) %>%
    slice(1L) %>%
    ungroup() %>%
    select(EnrollmentID, {{assessment_identifier}})
  
  enrollments %>%
    left_join(annual_data, by = "EnrollmentID")
}

keep_adults_only <- function(enrollment_data) {
  enrollment_data %>%
    inner_join(client_plus %>%
                 filter(age_group == "adult") %>%
                 select(PersonalID),
               by = "PersonalID")
}

keep_adults_and_hoh_only <- function(enrollment_data) {
  enrollment_data %>%
    inner_join(client_plus %>%
                 filter(age_group == "adult") %>%
                 select(PersonalID) %>%
                 union(recent_household_enrollment %>%
                         filter(RelationshipToHoH == 1) %>%
                         select(PersonalID)),
               by = "PersonalID")
}
