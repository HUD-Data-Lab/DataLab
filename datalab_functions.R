# install and/or load packages
if(!require(readxl)) {
  install.packages("readxl"); 
}
  library(readxl)
if(!require(janitor)) {
  library(janitor)
  install.packages("janitor"); 
}

if(!require(data.table)) {
  install.packages("data.table"); 
  library(data.table)
}
if(!require(lubridate)) {
  install.packages("lubridate"); 
  library(lubridate)
}
if(!require(tidyverse)) {
  install.packages("tidyverse"); 
  library(tidyverse)
}
if(exists("run_locally")) {
  if(!require(archive)) {
    install.packages("archive");
    library(archive)
  }
}

set.seed(2022)

# install.packages("remotes")
# remotes::install_github("COHHIO/HMIS")
# library(HMIS)

`%nin%` = Negate(`%in%`)

ifnull <- function(value, replace_with) {
  if(is.data.frame(value)) {
    replace_with_class <- case_when(
      class(replace_with) %in% c("integer", "numeric") ~ c("integer", "numeric"), 
      TRUE ~ c(class(replace_with))
    )
    
    for (relevant_column in 
         colnames(value)[sapply(value, class) %in% replace_with_class]) {
      value[[relevant_column]] <- ifnull(value[[relevant_column]], replace_with)
    }
  } else {
    if(length(value) > 0) {
      value[is.na(value) | is.nan(value) | 
              is.infinite(value) 
            # | value == "NaN"
            ] <- replace_with
    } 
  }
  # as.numeric(value)
  value
}

trunc_userid <- function(df) {
  df %>%
    mutate(UserID = substr(UserID, 1, 2))
}

entry_annual_exit <- c("entry", "annual", "exit")

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


# provides distinct client counts within households for any given grouping 
return_household_groups <- function(APR_dataframe, grouped_by = grouped_by, 
                                    group_list = group_list,
                                    split_by_age = FALSE) {
  # browser()

  potential_table <- APR_dataframe %>%
    group_by({{grouped_by}})
  
  if (split_by_age) {
    
    household_group_list <- split_household_group_list
    
    potential_table <- potential_table %>%
      summarise(Total = n_distinct(PersonalID, na.rm = TRUE),
                Without.Children = n_distinct(PersonalID[household_type == "AdultsOnly"], 
                                              na.rm = TRUE),
                Adults.in.HH.with.Children.and.Adults = n_distinct(PersonalID[household_type == "AdultsAndChildren" &
                                                                                age_group == "Adults"], 
                                                                   na.rm = TRUE),
                Children.in.HH.with.Children.and.Adults = n_distinct(PersonalID[household_type == "AdultsAndChildren" &
                                                                                  age_group == "Children"], 
                                                                     na.rm = TRUE),
                With.Only.Children = n_distinct(PersonalID[household_type == "ChildrenOnly"], 
                                                na.rm = TRUE),
                Unknown.Household.Type = n_distinct(PersonalID[household_type == "Unknown"], 
                                                    na.rm = TRUE)) 
  } else {
    potential_table <- potential_table %>%
      group_by({{grouped_by}}) %>%
      summarise(Total = n_distinct(PersonalID, na.rm = TRUE),
                Without.Children = n_distinct(PersonalID[household_type == "AdultsOnly"], 
                                              na.rm = TRUE),
                With.Children.And.Adults = n_distinct(PersonalID[household_type == "AdultsAndChildren"], 
                                                      na.rm = TRUE),
                With.Only.Children = n_distinct(PersonalID[household_type == "ChildrenOnly"], 
                                                na.rm = TRUE),
                Unknown.Household.Type = n_distinct(PersonalID[household_type == "Unknown"], 
                                                    na.rm = TRUE)) 
  }
  
  potential_table <- potential_table %>%
    # I don't remember why I added the next two lines, they seem redundant
    # but I know they mattered at some point? Need to revisit and verify
    pivot_longer(!{{grouped_by}}, names_to = "Group", values_to = "values") %>%
    pivot_wider(names_from = "Group", values_from = "values") %>%
    add_column(!!!household_group_list[!names(household_group_list) %in% names(.)]) %>%
    select(c({{grouped_by}}, names(household_group_list)))
  
  deparsed_grouped_by <- deparse(substitute(grouped_by))
  
  as.data.frame(group_list) %>%
    rename({{grouped_by}} := group_list) %>%
    full_join(potential_table, by = deparsed_grouped_by) %>%
    ifnull(., 0)
  
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
        DisablingCondition %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
        TRUE ~ "Data.Not.Collected"),
      case_when(
        disability_count == 1 ~ "One Condition",
        disability_count == 2 ~ "Two Conditions",
        TRUE ~ "Three Or More Conditions")))
}

determine_total_income <- function(enrollments_and_income, annual = FALSE) {
  
  if(annual) {
    enrollments_and_income <- enrollments_and_income %>%
      left_join(annual_assessment_dates, by = "HouseholdID")
  }
  
  total_income <- enrollments_and_income %>%
    mutate(earned_income = ifnull(
      if_else(Earned == 1 & EarnedAmount > 0, EarnedAmount, 0), 0),
           amounts_combined = 
        ifnull(if_else(Earned == 1 & EarnedAmount > 0, EarnedAmount, 0), 0) +
        ifnull(if_else(Unemployment == 1 & UnemploymentAmount > 0, UnemploymentAmount, 0), 0) +
        ifnull(if_else(SSI == 1 & SSIAmount > 0, SSIAmount, 0), 0) +
        ifnull(if_else(SSDI == 1 & SSDIAmount > 0, SSDIAmount, 0), 0) +
        ifnull(if_else(VADisabilityService == 1 & VADisabilityServiceAmount > 0, VADisabilityServiceAmount, 0), 0) +
        ifnull(if_else(VADisabilityNonService == 1 & VADisabilityNonServiceAmount > 0, VADisabilityNonServiceAmount, 0), 0) +
        ifnull(if_else(PrivateDisability == 1 & PrivateDisabilityAmount > 0, PrivateDisabilityAmount, 0), 0) +
        ifnull(if_else(WorkersComp == 1 & WorkersCompAmount > 0, WorkersCompAmount, 0), 0) +
        ifnull(if_else(TANF == 1 & TANFAmount > 0, TANFAmount, 0), 0) +
        ifnull(if_else(GA == 1 & GAAmount > 0, GAAmount, 0), 0) +
        ifnull(if_else(SocSecRetirement == 1 & SocSecRetirementAmount > 0, SocSecRetirementAmount, 0), 0) +
        ifnull(if_else(Pension == 1 & PensionAmount > 0, PensionAmount, 0), 0) +
        ifnull(if_else(ChildSupport == 1 & ChildSupportAmount > 0, ChildSupportAmount, 0), 0) +
        ifnull(if_else(Alimony == 1 & AlimonyAmount > 0, AlimonyAmount, 0), 0) +
        ifnull(if_else(OtherIncomeSource == 1 & OtherIncomeAmount > 0, OtherIncomeAmount, 0), 0),
           #  use this line for systems that auto-calculate total monthly income
           calculated_total_income = case_when(!is.na(TotalMonthlyIncome) &
                                                 TotalMonthlyIncome > 0 ~ TotalMonthlyIncome,
                                               amounts_combined > 0 ~ amounts_combined,
                                               IncomeFromAnySource == 0 |
                                                 (TotalMonthlyIncome <= 0 &
                                                    (is.na(IncomeFromAnySource) |
                                                       IncomeFromAnySource == 1)) ~ 0),
           total_income_group = case_when(
             is.na(calculated_total_income) ~ if_else(
               IncomeFromAnySource %in% c(8, 9), "Client.Does.Not.Know.or.Prefers.Not.to.Answer", "Data.Not.Collected"),
             calculated_total_income == 0 ~ "No Income",
             calculated_total_income <= 150 ~ "$1 - $150",
             calculated_total_income <= 250 ~ "$151 - $250",
             calculated_total_income <= 500 ~ "$251 - $500",
             calculated_total_income <= 1000 ~ "$501 - $1,000",
             calculated_total_income <= 1500 ~ "$1,001 - $1,500",
             calculated_total_income <= 2000 ~ "$1,501 - $2,000",
             TRUE ~ "$2,001+"))
  
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
  
  if(annual) {
    income_amount_categories <- annual_income_amount_categories
  }
  
  income_groups <- enrollments_total_income %>%
    group_by(total_income_group) %>%
    summarise(Income = n_distinct(PersonalID, na.rm = TRUE)) %>%
    full_join(as.data.frame(income_amount_categories) %>%
                `colnames<-`(c("total_income_group")),
              by = "total_income_group") %>%
    mutate(total_income_group = factor(total_income_group, ordered = TRUE,
                                       levels = annual_income_amount_categories)) %>%
    ifnull(., 0)
  
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
                 filter(age_group == "Adults") %>%
                 select(PersonalID),
               by = "PersonalID")
}

keep_adults_and_hoh_only <- function(enrollment_data) {
  enrollment_data %>%
    inner_join(client_plus %>%
                 filter(age_group == "Adults") %>%
                 select(PersonalID) %>%
                 union(enrollment_data %>%
                         filter(RelationshipToHoH == 1) %>%
                         select(PersonalID)),
               by = "PersonalID")
}

# used in Q18
categorize_income <- function(enrollments_total_income, annual = FALSE) {

  if(annual) {
    income_type_categories <- annual_income_type_categories
  }
  
  income_category_table <- enrollments_total_income %>%
    mutate(income_category = case_when(
      earned_income > 0 &
        # modified this line to account for clients who have a specific earned 
        # income record that is greater than their entered total income;
        # only applies to systems that do not auto-calculate
        earned_income >= calculated_total_income ~ "Adults with Only Earned Income (i.e., Employment Income)",
      earned_income > 0 &
        earned_income < calculated_total_income ~ "Adults with Both Earned and Other Income",
      (earned_income == 0 |
         is.na(earned_income)) &
        calculated_total_income > 0 ~ "Adults with Only Other Income",
      calculated_total_income == 0 ~ "Adults with No Income",
      is.na(calculated_total_income) &
        !annual ~ "Missing Income Information",
      TRUE ~ total_income_group
    )) %>%
    group_by(income_category) %>%
    summarise(Income = n()) %>%
    full_join(as.data.frame(income_type_categories) %>%
                `colnames<-`(c("income_category")),
              by = "income_category") %>%
    mutate(income_category = factor(income_category, ordered = TRUE,
                                       levels = annual_income_type_categories)) %>%
    ifnull(., 0)
  
  income_category_table %>%
    arrange(income_category)
}

# used in Q19
get_income_type_changes <- function(income_categories, income_type, compare_to) {
  
  people <- income_categories %>%
    left_join(get(paste0(compare_to, "_income_for_changes")), by = "PersonalID") %>%
    summarise(lost_source = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) > 0 &
                                                    get(paste0(compare_to, "_", income_type, "_amount")) == 0],
                                       na.rm = TRUE),
              retained_decreased = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) > get(paste0(compare_to, "_", income_type, "_amount")) &
                                                           get(paste0(compare_to, "_", income_type, "_amount")) > 0], 
                                              na.rm = TRUE),
              retained_same = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) == get(paste0(compare_to, "_", income_type, "_amount")) &
                                                      get(paste0(compare_to, "_", income_type, "_amount")) > 0], 
                                         na.rm = TRUE),
              retained_increased = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) < get(paste0(compare_to, "_", income_type, "_amount")) &
                                                           get(paste0("entry_", income_type, "_amount")) > 0], 
                                              na.rm = TRUE),
              gained_source = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) == 0 &
                                                      get(paste0(compare_to, "_", income_type, "_amount")) > 0], 
                                         na.rm = TRUE),
              did_not_have_source = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) == 0 &
                                                            get(paste0(compare_to, "_", income_type, "_amount")) == 0], 
                                               na.rm = TRUE),
              total_adults = n_distinct(PersonalID, na.rm = TRUE)
    ) %>%
    mutate(gained_or_increased = retained_increased + gained_source,
           percent_accomplished = gained_or_increased / total_adults)
  
    amounts <- income_categories %>%
            left_join(get(paste0(compare_to, "_income_for_changes")), by = "PersonalID") %>%
            mutate(
              income_change = get(paste0(compare_to, "_", income_type, "_amount")) -
                get(paste0("entry_", income_type, "_amount")),
              lost_source = case_when(
                get(paste0("entry_", income_type, "_amount")) > 0 &
                  get(paste0(compare_to, "_", income_type, "_amount")) == 0 ~ income_change),
              retained_decreased = case_when(
                get(paste0("entry_", income_type, "_amount")) > get(paste0(compare_to, "_", income_type, "_amount")) &
                  get(paste0(compare_to, "_", income_type, "_amount")) > 0 ~ income_change),
              retained_increased = case_when(
                get(paste0("entry_", income_type, "_amount")) < get(paste0(compare_to, "_", income_type, "_amount")) &
                  get(paste0("entry_", income_type, "_amount")) > 0 ~ income_change),
              gained_source = case_when(
                get(paste0("entry_", income_type, "_amount")) == 0 &
                  get(paste0(compare_to, "_", income_type, "_amount")) > 0 ~ income_change),
              ) %>%
              summarise(lost_source = if_else(
                people$lost_source > 0, mean(lost_source, na.rm = TRUE), 0),
                        retained_decreased = if_else(
                          people$retained_decreased > 0, mean(retained_decreased, na.rm = TRUE), 0),
                        retained_same = NA,
                        retained_increased = if_else(
                          people$retained_increased > 0, mean(retained_increased, na.rm = TRUE), 0),
                        gained_source = if_else(
                          people$gained_source > 0, mean(gained_source, na.rm = TRUE), 0),
                        did_not_have_source = NA,
                        total_adults = if_else(income_type == "total",
                                               mean(income_change, na.rm = TRUE), 0)
              ) %>%
      mutate(gained_or_increased = ifnull(((people$retained_increased * ifnull(retained_increased, 0)) +
                                             (people$gained_source * ifnull(gained_source, 0))) / 
                                            (people$retained_increased + people$gained_source), 0),
             percent_accomplished = NA)
    
    people %>% 
      union(amounts)
}

# used in Q17 and Q20
pivot_existing_only <- function(data, group_name, client_label) {
  data[data != 1] <- 0
  
  data %>%
    mutate(total_row = "") %>%
    select(c(total_row, colnames(data))) %>%
    adorn_totals("row") %>%
    filter(total_row == "Total") %>%
    pivot_longer(!total_row, names_to = "Group", values_to = "values") %>%
    select(-total_row) %>%
    `colnames<-`(c(group_name, paste0(client_label, "Clients")))
  
}

# used in Q22 questions
# used in Q5
add_length_of_time_groups <- function(data, start_date, end_date, report_type,
                                      in_project = TRUE) { 
  start_date <- enquo(start_date) #What is enquo and why are we using it for the date variables?
  end_date <- enquo(end_date) 
  
  if(in_project) { 
    nbn_data <- data %>%
      filter(ProjectID %in% Project$ProjectID[Project$ProjectType == 1]) %>% 
      left_join(all_bed_nights, # The all_bed_nights object for SPM is different from all_bed_nights in DataLab.R.
                by = "EnrollmentID",
                multiple = "all") %>%
      group_by(EnrollmentID) %>%
      summarise(nbn_number_of_days = n_distinct(na.omit(ymd(DateProvided)), #Count distinct number of days that are not NA. What is the difference between na.omit and na.rm?
                                                na.rm = TRUE)) %>%
      ungroup() %>%
      select(EnrollmentID, nbn_number_of_days)
    
    time_groups <- data %>%
      left_join(nbn_data, by = "EnrollmentID") %>%
      mutate(number_of_days = 
               if_else(
                 ProjectID %in% Project$ProjectID[Project$ProjectType == 1], #Identify project IDs that are a nbn shelter
                 nbn_number_of_days, #If true return number of days
                 as.integer(trunc((!!start_date %--% !!end_date) / days(1))))) #what does !! do? What is this doing? 
  } else {
    time_groups <- data %>%
      mutate(number_of_days = as.integer(trunc((!!start_date %--% !!end_date) / days(1)))) #If not a nbn shelter use this calculation for number_of_days
  }
  
  time_groups <- time_groups %>%
    mutate(number_of_days_group = case_when(
             is.na(number_of_days) |
               !!start_date > !!end_date ~ "Data.Not.Collected",
             number_of_days <= 7 ~ "0 to 7 days",
             number_of_days <= 14 ~ "8 to 14 days",
             number_of_days <= 21 ~ "15 to 21 days",
             number_of_days <= 30 ~ "22 to 30 days",
             number_of_days <= 60 ~ "31 to 60 days",
             number_of_days <= 90 ~ "61 to 90 days",
             number_of_days <= 180 ~ "91 to 180 days",
             number_of_days <= 365 ~ "181 to 365 days",
             number_of_days <= 730 ~ "366 to 730 days (1-2 Yrs)",
             number_of_days <= 1095 ~ "731 to 1,095 days (2-3 Yrs)",
             number_of_days <= 1460 ~ "1,096 to 1,460 days (3-4 Yrs)",
             number_of_days <= 1825 ~ "1,461 to 1,825 days (4-5 Yrs)",
             TRUE ~ "More than 1,825 days (>5 Yrs)"))
           
  if(report_type == "APR") {
    time_groups <- time_groups %>%
      mutate(number_of_days_group = case_when(
        between(number_of_days, 0, 30) ~ "30 days or less", # Q22 for APR changes days categories. This modifies the categories for the APR 
        TRUE ~ number_of_days_group))
  } else if(report_type == "days_prior_to_housing") {
    time_groups <- time_groups %>%
      mutate(number_of_days_group = case_when(
        #between(number_of_days, 61, 180) ~ "61 to 180 days",    # Specs updated age categories to be 61 to 90 and 91 to 180
        number_of_days >= 731 ~ "731 days or more",
        TRUE ~ number_of_days_group)) 
  } # Set up different age categories for Q22d(?)
    # else {
    #time_groups <- time_groups %>%
    #  mutate(number_of_days_group = case_when(
    #    #between(number_of_days, 61, 180) ~ "61 to 180 days",    # Specs updated age categories to be 61 to 90 and 91 to 180
    #    number_of_days >= 731 ~ "731 days or more",
    #    TRUE ~ number_of_days_group))}
  data %>%
    left_join(time_groups %>%
                select(EnrollmentID, number_of_days, number_of_days_group),
              by = "EnrollmentID")
}

# also used in Q22 questions
length_of_time_groups <- function(report_type = "", 
                                  column_name) {
  if(report_type == "APR") {
    rows <- c("30 days or less", "31 to 60 days", "61 to 90 days", "91 to 180 days", 
      "181 to 365 days", "366 to 730 days (1-2 Yrs)", "731 to 1,095 days (2-3 Yrs)",
      "1,096 to 1,460 days (3-4 Yrs)", "1,461 to 1,825 days (4-5 Yrs)",
      "More than 1,825 days (>5 Yrs)")
  } else if(report_type == "days_prior_to_housing") {
    rows <- c("0 to 7 days", "8 to 14 days", "15 to 21 days", "22 to 30 days", 
              "31 to 60 days", "61 to 90 days", "91 to 180 days", "181 to 365 days", 
              "366 to 730 days (1-2 Yrs)", "731 days or more", 
              "Not yet moved into housing", "Data.Not.Collected")
  } else if (report_type == "CAPER"){
    rows <- c("0 to 7 days", "8 to 14 days", "15 to 21 days", "22 to 30 days",  #This is for CAPER Specific age categories. 
              "31 to 60 days", "61 to 90 days", "91 to 180 days", 
              "181 to 365 days", "366 to 730 days (1-2 Yrs)", "731 days or more")
  } else {
    rows <- c("0 to 7 days", "8 to 14 days", "15 to 21 days", "22 to 30 days",  #This is for CAPER Specific age categories. 
              "31 to 60 days", "61 to 90 days", "91 to 180 days", 
              "181 to 365 days", "366 to 730 days (1-2 Yrs)", 
              # "731 days or more",
    "731 to 1,095 days (2-3 Yrs)",
    "1,096 to 1,460 days (3-4 Yrs)", "1,461 to 1,825 days (4-5 Yrs)",
    "More than 1,825 days (>5 Yrs)")
    }
  
  as.data.frame(rows) %>%
    rename(!!column_name := rows)
}

# used in Q22c
return_household_averages <- function(data, field_to_average, 
                                      column_label, new_row_name) {
  field_to_average <- enquo(field_to_average)
  
  household_type <- c(Total = 0, Without.Children = 0, 
                      With.Children.And.Adults = 0, With.Only.Children = 0, 
                      Unknown.Household.Type = 0)
  
  potential_table <- as.data.frame(household_group_list) %>%
    left_join(data %>%
    group_by(household_type) %>%
    summarise(field_average = mean({{field_to_average}})) %>%
    mutate(column_label = {{new_row_name}}), by = "household_type") #%>%
    # select(c({{column_label}}, names(household_group_list)))
  
  potential_table
}

# used in Q18, Q26f
create_income_categories <- function(included_enrollments) {
  
  for(period in entry_annual_exit) {
    
    data <- get(paste0(period, "_income")) %>%
      filter(EnrollmentID %in% included_enrollments$EnrollmentID) %>%
      determine_total_income(., annual = period == "annual") %>%
      categorize_income(., annual = period == "annual") %>%
      `colnames<-`(c("income_category", paste0(period, "Income")))
    
    assign(paste0(period, "_income_categories"), data)
  }
  
  entry_income_categories %>%
    full_join(annual_income_categories, by = "income_category") %>%
    left_join(exit_income_categories, by = "income_category")
  
}

# used in Q17, Q26g
create_income_sources <- function(included_enrollments) {
  for(period in entry_annual_exit) {
    
    data <- get(paste0(period, "_income")) %>%
      filter(EnrollmentID %in% included_enrollments$EnrollmentID) %>%
      select(Earned, Unemployment, SSI, SSDI, VADisabilityService, 
             VADisabilityNonService, PrivateDisability, WorkersComp, TANF,
             GA, SocSecRetirement, Pension, ChildSupport, Alimony,
             OtherIncomeSource) %>%
      pivot_existing_only(., "IncomeGroup", period) %>%
      left_join(IncomeTypes, by = "IncomeGroup") %>%
      mutate(IncomeGroup = OfficialIncomeName) %>%
      select(-OfficialIncomeName)
    
    assign(paste0(period, "_income_sources"), data)
  }
  
  created_income_sources <- entry_income_sources %>%
    left_join(annual_income_sources, by = "IncomeGroup") %>%
    left_join(exit_income_sources, by = "IncomeGroup") %>%
    rbind(., income_information_present(included_enrollments)) %>%
    rename(Income.at.Start = entryClients,
           Income.at.Latest.Annual.Assessment.for.Stayers = annualClients,
           Income.at.Exit.for.Leavers = exitClients)
}


# check for income information presence
income_information_present <- function(included_enrollments) {
  
  entry_annual_income_present <- intersect(
    intersect(
      entry_income$PersonalID[entry_income$IncomeFromAnySource %in% c(0, 1)],
      annual_income$PersonalID[annual_income$IncomeFromAnySource %in% c(0, 1)]),
    included_enrollments$PersonalID
  ) 
  
  entry_exit_income_present <- intersect(
    intersect(
      entry_income$PersonalID[entry_income$IncomeFromAnySource %in% c(0, 1)],
      exit_income$PersonalID[exit_income$IncomeFromAnySource %in% c(0, 1)]),
    included_enrollments$PersonalID
  )
  
  c("Adults with Income Information at Start and Annual Assessment/Exit", NA,
    length(entry_annual_income_present),
    length(entry_exit_income_present))
}

# create age group table
create_age_groups <- function(filtered_recent_program_enrollment,
                              chronic = FALSE) {
  
  if (chronic) {
    detailed_age_group_list <- c("0 - 17", detailed_age_group_list[4:11])
    
    filtered_recent_program_enrollment <- filtered_recent_program_enrollment %>%
      mutate(detailed_age_group = case_when(
        detailed_age_group %nin% detailed_age_group_list ~ "0 - 17",
        TRUE ~ detailed_age_group
      ))
  }
  
  filtered_recent_program_enrollment %>%
    return_household_groups(., detailed_age_group, detailed_age_group_list) %>%
    adorn_totals("row")
}


# create gender group table
create_gender_groups <- function(filtered_recent_program_enrollment) {
  
 filtered_recent_program_enrollment %>%
    left_join(gender_info,
              by = all_of(names(gender_columns))) %>%
    mutate(across(
      all_of(names(gender_columns)),
      ~ as.numeric(.)),
      gender_count = rowSums(across(all_of(names(gender_columns))),
                              na.rm = TRUE),
      gender_tabulation = case_when(
        gender_count %in% 1:2 ~ gender_name_list,
        gender_count > 2 ~ "More than 2 Gender Identities Selected",
        GenderNone %in% c(8, 9) ~ "Client Doesn’t Know/Prefers Not to Answer",
        TRUE ~ "Data Not Collected"
      )
    ) %>%
    return_household_groups(., gender_tabulation, 
                            c(gender_info$gender_name_list, 
                              "More than 2 Gender Identities Selected",
                              "Client Doesn’t Know/Prefers Not to Answer",
                              "Data Not Collected")) %>%
    adorn_totals("row")
}


# create benefit group table
create_benefit_groups <- function(included_enrollments) {
  for(period in benefit_entry_annual_exit) {
    
    data <- get(paste0(period, "_income")) %>%
      keep_adults_only() %>%
      filter(EnrollmentID %in% included_enrollments$EnrollmentID &
               (period != "exit" |
                  paste(HouseholdID, ExitDate) %in% paste(
                    included_enrollments$HouseholdID[included_enrollments$RelationshipToHoH == 1],
                    included_enrollments$ExitDate[included_enrollments$RelationshipToHoH == 1]))) %>%
      select(all_of(benefit_list)) %>%
      pivot_existing_only(., "BenefitGroup", period) %>%
      left_join(BenefitTypes, by = "BenefitGroup") %>%
      mutate(BenefitGroup = OfficialBenefitName) %>%
      select(-OfficialBenefitName)
    
    assign(
      paste0(
        entry_annual_exit[which(benefit_entry_annual_exit == period)],
        "_benefit_sources"), 
      data)
  }
  
  entry_benefit_sources %>%
    left_join(annual_benefit_sources, by = "BenefitGroup") %>%
    left_join(exit_benefit_sources, by = "BenefitGroup") %>%
    rename(Benefit.at.Start = entryClients,
           Benefit.at.Latest.Annual.Assessment.for.Stayers = benefit_annualClients,
           Benefit.at.Exit.for.Leavers = exitClients)
}


# create destination table for APR/CAPER
create_destination_groups <- function(included_enrollments) {
  for(residence_type in c("Homeless", "Institutional","Temporary", "Permanent",  
                          "Other")) {
    residences_to_include <- ResidenceUses %>%
      filter(APR_LocationGroup == residence_type &
               !is.na(LocationDescription) &
               Destination) %>%
      mutate(LocationDescription = case_when(
        Location %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
        # Location %in% c(30, 99) ~ "Data.Not.Collected",
        TRUE ~ LocationDescription))
    
    group_of_residences <- included_enrollments %>%
      inner_join(residences_to_include, by = c("Destination" = "Location"))  %>%
      return_household_groups(., LocationDescription, unique(residences_to_include$LocationDescription)) %>%
      full_join(residences_to_include %>%
                  # filter(Location %nin% c(9, 30)), 
                  filter(Location != 9), 
                by = "LocationDescription") %>%
      arrange(APR_LocationOrder) %>%
      adorn_totals("row") %>%
      ifnull(., 0) %>%
      mutate(LocationDescription = case_when(
        LocationDescription == "Total" ~ paste(str_to_title(residence_type), "Subtotal"),
        TRUE ~ LocationDescription))
    
    group_title_row <- group_of_residences[0,]
    group_title_row[1,1] <- paste(residence_type, "Situations")
    
    group_of_residences <- rbind(group_title_row, group_of_residences)
    
    if (residence_type == "Homeless") {
      destination_group <- group_of_residences
    } else {
      destination_group <- destination_group %>%
        union(group_of_residences)
    }
  }
  
  exit_categories <- data.frame(Outcome = possible_outcomes) %>%
    left_join(included_enrollments %>%
                mutate(ProjectType = case_when(
                  ProjectType == 12 ~ 3,
                  TRUE ~ ProjectType)) %>%
                inner_join(DestinationClassification, by = c("Destination" = "Location",
                                                             "ProjectType" = "ProjectType")) %>%
                return_household_groups(., Outcome, possible_outcomes),
              by = "Outcome") %>%
    adorn_totals("row") %>%
    pivot_longer(., cols = -Outcome) %>%
    pivot_wider(., names_from = Outcome) %>%
    mutate(Percentage = P / (Total - X)) %>%
    rename(group = name) %>%
    pivot_longer(., cols = -group) %>%
    pivot_wider(., names_from = group) %>%
    rename(LocationDescription = name) %>%
    ifnull(., 0)
  
  
  destination_group %>%
    select(LocationDescription, Total, Without.Children,
           With.Children.And.Adults, With.Only.Children, Unknown.Household.Type) %>%
    union(exit_categories %>%
            filter(LocationDescription == "Total")) %>%
    union(exit_categories %>%
            filter(LocationDescription %in% c("P", "X", "Percentage"))) %>%
    mutate(LocationDescription = case_when(
      LocationDescription == "P" ~ "Total persons exiting to positive housing destinations",
      LocationDescription == "X" ~ "Total persons whose destinations excluded them from the calculation",
      TRUE ~ LocationDescription))
}


# create prior residence table, used for Q15 and Q27d

create_prior_residence_groups <- function(included_enrollments) {
  for(residence_type in c("Homeless", "Institutional", "Temporary",
                          "Permanent", "Other")) {
    residences_to_include <- ResidenceUses %>%
      filter(APR_LocationGroup == residence_type &
               !is.na(LocationDescription) &
               PriorLivingSituation &
               Location != 9) %>%
      arrange(APR_LocationOrder) %>%
      mutate(LocationDescription = case_when(
        Location == 8 ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
        TRUE ~ LocationDescription))
    
    group_of_residences <- included_enrollments %>%
      mutate(LivingSituation = case_when(LivingSituation == 9 ~ as.integer(8), 
                                         is.na(LivingSituation) ~ as.integer(99),
                                         TRUE ~ LivingSituation)) %>%
      inner_join(residences_to_include, by = c("LivingSituation" = "Location")) %>%
      return_household_groups(., LocationDescription, residences_to_include$LocationDescription) %>%
      adorn_totals("row") %>%
      mutate(LocationDescription = case_when(
        LocationDescription == "Total" ~ paste(residence_type, "Subtotal"),
        TRUE ~ LocationDescription
      ))
    
    group_title_row <- group_of_residences[0,]
    group_title_row[1,1] <- paste(residence_type, "Situations")
    
    group_of_residences <- rbind(group_title_row, group_of_residences)
    
    if (exists(("residence_table"))) {
      residence_table <- residence_table %>%
        union(group_of_residences)
    } else {
      residence_table <- group_of_residences
    }
  }
  
  residence_table <- residence_table %>%
    select(LocationDescription, Total, Without.Children,
           With.Children.And.Adults, With.Only.Children, Unknown.Household.Type) %>%
    ifnull(., 0)
  
  total_row <- residence_table %>%
    filter(grepl("Subtotal", LocationDescription)) %>%
    select(-LocationDescription) %>%
    colSums() %>%
    t() %>%
    as.data.frame() %>%
    mutate(LocationDescription = "Total") %>%
    select(colnames(residence_table))
  
  rbind(residence_table, total_row)
}

# creates the biggest table--income x household type x disabling condition
{
  income_hh_type_disabling_condition_table <- function(exit_income,
                                                       youth = FALSE) {
    
    hh_type_labels <- c(AO = "Without.Children", 
                      AC = "With.Children.And.Adults", 
                      CO = "With.Only.Children", 
                      UK = "Unknown.Household.Type")
    
    selected_cols <- c()
    if (youth) {
      hh_type_groups <- unname(hh_type_labels)
      hh_types_to_keep <- names(hh_type_labels)
      exit_income <- exit_income %>%
        filter(youth == 1)
      group_label <- "Youth"
    } else {
      hh_type_groups <- unname(hh_type_labels)[c(1, 2, 4)]
      hh_types_to_keep <- names(hh_type_labels)[c(1, 2, 4)]
      exit_income <- exit_income %>%
        keep_adults_only()
      group_label <- "Adult"
    }
    
    condition_presence_list <- c(paste(group_label, "with Disabling Condition"),
                                 paste(group_label, "without Disabling Condition"),
                                 paste0("Total ", group_label, "s"))
    
    total_label <- paste0("Unduplicated.Total.", group_label, "s")
    
    for(condition_presence in condition_presence_list) {
      
      filtered_income_information <- exit_income %>%
        filter(IncomeFromAnySource %in% c(0, 1) &
                 ((condition_presence == condition_presence_list[1] &
                     DisablingCondition == 1) |
                    (condition_presence == condition_presence_list[2] &
                       DisablingCondition == 0) |
                    (condition_presence == condition_presence_list[3] &
                       DisablingCondition %in% c(0, 1)))) %>%
        # mutate(Other.Source = if_else(
          # Unemployment == 1 |
          #   VADisabilityNonService == 1 |
          #   GA == 1 |
          #   Alimony == 1 |
            # OtherIncomeSource == 1, 1, 0)) %>%
        select(c(PersonalID, household_type, IncomeFromAnySource, 
                 all_of(IncomeTypes$IncomeGroup))) 
      
      long_income_information <- filtered_income_information %>%
        pivot_longer(!c(PersonalID, household_type, IncomeFromAnySource)) %>%
        mutate(row_name = "row")
      
      unduplicated_total <- cbind(name = total_label, 
                                  long_income_information %>%
                                    mutate(row_name = "total") %>%
                                    return_household_groups(., row_name, "total") %>%
                                    select(-row_name))
      
      filtered_has_income <- long_income_information %>%
        filter(value == 1)
      
      unduplicated_with_income <- filtered_has_income %>%
        mutate(row_name = "has income") %>%
        return_household_groups(., row_name, "has income")
      
      unduplicated_without_income <- filtered_income_information %>%
        filter(IncomeFromAnySource == 0) %>%
        mutate(name = "No.Sources") %>%
        return_household_groups(., name, "No.Sources")
      
      income_hh_type_disabling_condition_data <- as.data.frame(income_rows_to_show) %>%
        rename(name = income_rows_to_show) %>%
        left_join(filtered_has_income %>%
                    return_household_groups(., name, IncomeTypes$IncomeGroup),
                  by = "name") %>%
        rbind(., unduplicated_without_income) %>%
        rbind(., unduplicated_total) %>%
        `colnames<-`(c("name", "Total", paste0(names(hh_type_labels), ": ", 
                                      condition_presence))) %>%
        ifnull(., 0)
      
      if(condition_presence == condition_presence_list[1]) {
        income_hh_type_disabling_condition <- income_hh_type_disabling_condition_data
      } else {
        income_hh_type_disabling_condition <- income_hh_type_disabling_condition %>%
          left_join(income_hh_type_disabling_condition_data, by = "name")
      }
    }
    
    for(household_type in hh_types_to_keep) {
      income_hh_type_disabling_condition <- income_hh_type_disabling_condition %>%
        mutate(!!paste0(household_type, ": % with Disabling Condition by Source") := ifnull(
          get(paste0(household_type, ": ", condition_presence_list[1])) / 
            get(paste0(household_type, ": ", condition_presence_list[3])), 0)
        )
      
      for (disabling_condition_present in c(condition_presence_list, 
                                            "% with Disabling Condition by Source")) {
          selected_cols <- c(selected_cols, paste0(household_type, ": ",
                                                   disabling_condition_present))
      }
    }
    
    income_hh_type_disabling_condition <- income_hh_type_disabling_condition %>%
      select(c(name, all_of(selected_cols)) )
    
    no_income_row <- match(TRUE, income_hh_type_disabling_condition$name == "No.Sources")
    total_row <- match(TRUE, income_hh_type_disabling_condition$name == total_label)
    

    
    income_hh_type_disabling_condition <- income_hh_type_disabling_condition %>%
      left_join(IncomeTypes,
                by = c("name" = "IncomeGroup")) %>%
      mutate(name = case_when(
        !is.na(OfficialIncomeName) ~ OfficialIncomeName,
        TRUE ~ name)) %>%
      select(-OfficialIncomeName)
    
    income_hh_type_disabling_condition[, grep("%", 
                                              colnames(income_hh_type_disabling_condition))] <- decimal_format(
                                                income_hh_type_disabling_condition[, grep("%", 
                                                                                          colnames(income_hh_type_disabling_condition))], 4)
    income_hh_type_disabling_condition[income_hh_type_disabling_condition$name == total_label, 
                                       grep("%", colnames(income_hh_type_disabling_condition))] <- NA
    
    income_hh_type_disabling_condition
  }
}

# create contact table for Q9 contacts and engagements
create_contact_table <- function(filtered_enrollments, first_CLS_group,
                                 all_CLS_for_Q9) {
  
  data <- filtered_enrollments %>%
    left_join(first_CLS_group, by = "EnrollmentID") %>%
    inner_join(all_CLS_for_Q9 %>%
                group_by(EnrollmentID) %>%
                summarise(Contacts = n()) %>%
                mutate(ContactGroup = case_when(
                  Contacts == 1 ~ "Once",
                  Contacts <= 5 ~ "2-5 times",
                  Contacts <= 9 ~ "6-9 times",
                  TRUE ~ "10+ times")),
              by = "EnrollmentID") %>% 
    group_by(ContactGroup) %>%
    summarise(All.Persons.Contacted = n_distinct(PersonalID, na.rm = TRUE),
              First.contact.NOT.staying.on.the.Streets.ES.or.SH = n_distinct(PersonalID[CLS_group == "Not LH"], 
                                                                             na.rm = TRUE),
              First.contact.WAS.staying.on.the.Streets.ES.or.SH = n_distinct(PersonalID[CLS_group == "LH"], 
                                                                             na.rm = TRUE),
              First.contact.Worker.unable.to.determine = n_distinct(PersonalID[CLS_group == "Unknown"], 
                                                                    na.rm = TRUE)) %>%
    adorn_totals("row") #
  
  as.data.frame(contact_groups) %>%
    `colnames<-`(c("ContactGroup")) %>% 
    full_join(data, by = "ContactGroup") %>%
    ifnull(., 0)
}

# generate table with general summary information about the project
create_summary_table <- function(filtered_enrollments, column_name) {
  
  long_stayers <- filtered_enrollments %>%
    add_length_of_time_groups(., EntryDate,
                              ifnull(ExitDate, ymd(report_end_date) + days(1)),
                              "APR") %>%
    filter(number_of_days >= 365 &
             is.na(ExitDate) &
             (age_group == "Adults" |
                RelationshipToHoH == 1))
  
  filtered_enrollments %>%
    # left_join(client_plus, by = "PersonalID") %>%
    # left_join(chronicity_data, by = "EnrollmentID") %>%
    summarise(Total.number.of.persons.served = n_distinct(PersonalID, na.rm = TRUE),
              Number.of.adults.age.18.or.over = uniqueN(na.omit(PersonalID[age_group == "Adults"])),
              Number.of.children.under.age.18 = uniqueN(na.omit(PersonalID[age_group == "Children"])),
              Number.of.persons.with.unknown.age = uniqueN(na.omit(PersonalID[age_group %in% c("Client.Does.Not.Know.or.Prefers.Not.to.Answer", "Data.Not.Collected")])), #Removed refused and entered prefers not to answer
              Number.of.leavers = uniqueN(na.omit(PersonalID[!is.na(ExitDate)])),
              Number.of.adult.leavers = uniqueN(na.omit(PersonalID[age_group == "Adults" &
                                                             !is.na(ExitDate)])),
              Number.of.adult.and.head.of.household.leavers = uniqueN(na.omit((PersonalID[(age_group == "Adults" |
                                                                                     RelationshipToHoH == 1) &
                                                                                    !is.na(ExitDate)]))),
              Number.of.stayers = uniqueN(na.omit(PersonalID[is.na(ExitDate)])),
              Number.of.adult.stayers = uniqueN(na.omit(PersonalID[age_group == "Adults" &
                                                             is.na(ExitDate)])),
              Number.of.veterans = uniqueN(na.omit(PersonalID[new_veteran_status == 1])),
              Number.of.chronically.homeless.persons = uniqueN(na.omit(PersonalID[chronic == "Y"])),
              Number.of.youth.under.age.25 = uniqueN(na.omit(PersonalID[youth == 1])),
              Number.of.parenting.youth.under.age.25.with.children = uniqueN(na.omit(PersonalID[has_children == 1 & youth == 1])),
              Number.of.adult.heads.of.household = uniqueN(na.omit(PersonalID[age_group == "Adults" &
                                                                        RelationshipToHoH == 1])),
              Number.of.child.and.unknown.age.heads.of.household = uniqueN(na.omit(PersonalID[age_group != "Adults" &
                                                                                        RelationshipToHoH == 1])),
              Heads.of.households.and.adult.stayers.in.the.project.365.days.or.more = uniqueN(
                na.omit(PersonalID[PersonalID %in% long_stayers$PersonalID |
                                     (age_group == "Adults" &
                                        HouseholdID %in% long_stayers$HouseholdID[long_stayers$RelationshipToHoH == 1])]))
              ) %>% 
    mutate(rowname = column_name) %>% 
    pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
    pivot_wider(names_from = "rowname", values_from = "values")
}

# used in Q6f of the APR & CAPER, Q7 of the DQ report from the glossary
create_inactive_table <- function(dq_enrollments,
                                  activity_events,
                                  activity_type) {
  
  if (activity_type %nin% c("bed night", "contact")) {
    stop("The argument \"activity_type\" can only be \"bed night\" or \"contact\"")
  }
  
  included_activity_type <- intersect(
    c("InformationDate", "DateProvided"),
    colnames(activity_events)
  )
  
  activity_type_header <- if_else(activity_type == "contact",
                                  "Contact (Adults and Heads of Household in Street Outreach or ES – NBN)",
                                  "Bed Night (All clients in ES – NBN)")
  
  dq_enrollments %>%
    filter(is.na(ExitDate) &
             trunc((EntryDate %--% report_end_date) / days(1)) >= 90 &
             ((ProjectType == 1) |
                (activity_type == "contact" &
                   ProjectType == 4))) %>%
    left_join(activity_events %>%
                select(c("EnrollmentID", all_of(included_activity_type))) %>%
                `colnames<-`(c("EnrollmentID", "included_activity_type")),
              by = "EnrollmentID") %>%
    summarise(Data.Element = activity_type_header,
              Number.of.Records = n_distinct(PersonalID, na.rm = TRUE),
              Number.of.Inactive.Records = n_distinct(PersonalID[is.na(included_activity_type)], 
                                                      na.rm = TRUE)) %>%
    ifnull(., 0) %>%
    mutate(Percent.of.Inactive.Records = Number.of.Inactive.Records / Number.of.Records)
}

# used to write files with correct formatting
set_hud_format <- function(data_for_csv,
                           ignore_row_names = FALSE,
                           for_datatable = FALSE) {
  
  first_col_name <- names(data_for_csv)[1]
  if(!ignore_row_names) {
    data_for_csv <- data_for_csv %>%
      mutate(!!first_col_name := gsub(".", " ", get(first_col_name), fixed = TRUE),
             !!first_col_name := case_when(
               get(first_col_name) == "Client Does Not Know or Refused" ~
                 "Client Doesn't Know/Refused",
               TRUE ~ get(first_col_name)))
  }

  new_header <- names(data_for_csv)[2:length(data_for_csv)]
  new_header[new_header == "Client.Does.Not.Know.or.Prefers.Not.to.Answer"] <- "Client Doesn't Know/Refused" # Flagging this for follow-up: FY2024 removed refused ----
  
  if(for_datatable) {
    data_for_csv %>%
      `colnames<-`(gsub(".", " ", colnames(data_for_csv), fixed = TRUE))
  } else {
    data_for_csv %>%
    `colnames<-`(c("", gsub(".", " ", new_header, fixed = TRUE)))
  }
}


# write csvs for projects
write_csvs_for <- function(project_ids, zip_title, write_to) {
  
  if(missing(zip_title)) {
    zip_title <- Project$ProjectName[Project$ProjectID %in% project_ids][1]
  } 
  
  enrollment_info <- get("Enrollment") %>%
    filter(ProjectID %in% project_ids) %>%
    select(EnrollmentID, PersonalID, UserID)
  
  org_info <- get("Project")%>%
    filter(ProjectID %in% project_ids) %>%
    select(OrganizationID)
  
  for (file in names(hmis_csvs_fy24)) {
    
    data <- get(file)
    
    if ("ProjectID" %in% colnames(data)) {
      data <- data %>%
        filter(ProjectID %in% project_ids)
    } else if ("EnrollmentID" %in% colnames(data)) {
      data <- data %>%
        filter(EnrollmentID %in% enrollment_info$EnrollmentID)
    } else if ("PersonalID" %in% colnames(data)) {
      data <- data %>%
        filter(PersonalID %in% enrollment_info$PersonalID)
    } else if ("OrganizationID" %in% colnames(data)) {
      data <- data %>%
        filter(OrganizationID %in% org_info$OrganizationID)
    } else if ("UserID" %in% colnames(data)) {
      data <- data %>%
        filter(UserID %in% enrollment_info$UserID)
    } else if (file != "Export") {
      stop(paste(file))
    }
    
    if ("enroll_DateCreated" %in% colnames(data)) {
      data <- data %>%
        rename(DateCreated = enroll_DateCreated)
    }
    
    if ("exit_DateCreated" %in% colnames(data)) {
      data <- data %>%
        rename(DateCreated = exit_DateCreated)
      
    }
    
    write.csv(data %>%
                mutate(
                  across(colnames(data)[str_locate_all(pattern = "T",
                                                       get(file,
                                                           hmis_csvs_fy24))[[1]][,1]],
                         ~ format(., format = "%Y-%m-%d %H:%M:%S")),
                  across(colnames(data)[str_locate_all(pattern = "D",
                                                       get(file,
                                                           hmis_csvs_fy24))[[1]][,1]],
                         ~ format(., format = "%Y-%m-%d")),
                  across(colnames(data)[str_locate_all(pattern = "d",
                                                       get(file,
                                                           hmis_csvs_fy24))[[1]][,1]],
                         ~ if_else(. > 0, sprintf("%.2f", .), NA))),
              file.path(paste0("created_files/", file, ".csv")),
              row.names=FALSE, na = "",
              quote = which(as.character(lapply(get(file), class)) %nin%
                              c("integer", "numeric")))
  }
  
  general_wd <- getwd()
  setwd(paste0(general_wd, "/", write_to))
  archive_write_dir(paste0(zip_title, ".zip"),
                    paste0(general_wd, "/created_files"))
  setwd(general_wd)
  unlink(paste0(getwd(), "/created_files/*"))
  
}


# make table for 'time in program' questions
create_lot_table <- function(filtered_enrollments) {
  filtered_enrollments %>%
    select(c("ProjectID", all_of(housing_program_detail_columns),
             "household_type")) %>%
    add_length_of_time_groups(., EntryDate, 
                              ifnull(ExitDate, ymd(report_end_date) + days(1)),
                              "APR") %>%
    add_length_of_time_groups(., EntryDate, 
                              ifnull(ExitDate, ymd(report_end_date) + days(1)),
                              "CAPER") %>%
    select(-number_of_days.y) %>%
    rename(days_enrolled = number_of_days.x,
           APR_enrollment_length_group = number_of_days_group.x,
           CAPER_enrollment_length_group = number_of_days_group.y)
}

# make table for 'time to house' questions
create_time_to_move_in <- function(filtered_enrollments) {
  test <- filtered_enrollments %>%
    select(c("ProjectType", "ProjectID", 
             all_of(housing_program_detail_columns), "household_type")) %>%
    filter(ProjectType %in% c(3, 13) & 
             ((HoH_HMID >= report_start_date &
                 HoH_HMID <= report_end_date) |
                (is.na(HoH_HMID) &
                   ExitDate <= report_end_date))) %>%
    mutate(move_in_date = case_when(
      ymd(EntryDate) > ymd(HoH_HMID) ~ EntryDate,
      TRUE ~ HoH_HMID)) %>%
    add_length_of_time_groups(., EntryDate, move_in_date, "days_prior_to_housing") %>%
    rename(days_to_house = number_of_days,
           housing_length_group = number_of_days_group)
}

# make table for 'time homeless before housing' questions
create_time_prior_to_housing <- function(filtered_enrollments) {
  filtered_enrollments %>%
    filter(ProjectType %in% c(0, 1, 2, 3, 8, 9, 13)) %>%
    mutate(housing_date = case_when(
      ProjectType %nin% c(3, 9, 13) ~ EntryDate,
      TRUE ~ MoveInDateAdj),
      homelessness_start_date = case_when(
        age < 18 &
          EntryDate == HoH_EntryDate ~ HoH_ADHS,
        (is.na(age) | age >= 18) &
          DateToStreetESSH <= EntryDate ~ DateToStreetESSH)) %>%
    add_length_of_time_groups(., homelessness_start_date, housing_date, 
                              "days_prior_to_housing", in_project = FALSE) %>%
    rename(days_prior_to_housing = number_of_days_group)
}

# get additional client info
add_client_info <- function(filtered_enrollments) { #Create function to add client info
  
  if(exists("all_program_enrollments")) {      #all_program_enrollments object created on line 61 of APS_and_CAPER_FY24.
    enrollments_to_use <- all_program_enrollments
  } else {
    enrollments_to_use <- filtered_enrollments #filtered enrollments is the DF we use for the function (i.e., add_client_info(DF_example))
  }
  
  enrollments_to_use %>%
    filter(HouseholdID %in% filtered_enrollments$HouseholdID) %>% #select household IDs from either all program enrollments or DF selected
    mutate(date_for_age = (if_else( 
      EntryDate <= report_start_date,
      report_start_date, # Return Report Start date if true
      EntryDate))) %>% # Return Entry Date if False
    # select(PersonalID, date_for_age, HouseholdID, RelationshipToHoH) %>%
    # distinct() %>%
    inner_join(Client, by = "PersonalID") %>%  #Why inner_join? Doesn't inner join only keep the observations enrollments_to_use that match in Client?
    mutate(age = trunc((DOB %--% date_for_age) / years(1)), #Where is DOB created? What is %--% date_for_age? Did you just format the 1 to be a 1 year?
           detailed_age_group = case_when(age < 5 ~ "Under 5",
                                          age <= 12 ~ "5-12",
                                          age <= 17 ~ "13-17",
                                          age <= 24 ~ "18-24",
                                          age <= 34 ~ "25-34",
                                          age <= 44 ~ "35-44",
                                          age <= 54 ~ "45-54",
                                          age <= 64 ~ "55-64",
                                          age >= 65 ~ "65+",
                                          !is.na(DOBDataQuality) &
                                            DOBDataQuality %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
                                          TRUE ~ "Data.Not.Collected"), 
           age_group = case_when(age >= 18 ~ "Adults",
                                 age < 18 ~ "Children",
                                 TRUE ~ detailed_age_group), #What does TRUE do?
           new_veteran_status = if_else(
             age_group == "Children", as.integer(0), VeteranStatus)) %>% #If "Children" return 0 otherwise Veteran Status
    # get a second opinion on when to apply the household type calcs
    group_by(HouseholdID) %>% 
    mutate(oldest_age = max(age, na.rm = TRUE), # I think this is returning the -inf error.
           youth_household = if_else(oldest_age <= 24 & 
                                       oldest_age >= 0, 1, 0),
           youth = if_else(youth_household & age >= 12, 1, 0),
           has_children = max(
             if_else(age >= 0 &
                       age < 18 &
                       RelationshipToHoH == 2,
                     1, 0))) %>%
    ungroup() %>%
    filter(EnrollmentID %in% filtered_enrollments$EnrollmentID) %>%
    select(PersonalID, age, age_group, detailed_age_group, VeteranStatus, 
           youth_household, youth, has_children, new_veteran_status, DOB
           # ,
           # Woman, Man, NonBinary, CulturallySpecific, Transgender, 
           # Questioning, DifferentIdentity, GenderNone, AmIndAKNative,
           # Asian, BlackAfAmerican, HispanicLatinaeo, MidEastNAfrican,
           # NativeHIPacific, White, RaceNone
           )
}

# returns program information table for CoC APR, CAPER, and CE APR
program_information_table <- function(project_list, filtered_enrollments) {
  
  Project %>%
    filter(ProjectID %in% project_list) %>% 
    select(OrganizationID, ProjectName, ProjectID, ProjectType,RRHSubType,
           ResidentialAffiliation) %>%
    left_join(Organization %>%
                select(OrganizationID, OrganizationName,
                       VictimServiceProvider), 
              by = "OrganizationID") %>%
    left_join(CEParticipation %>% #Added CEParticiaption to match FY2024 APR/CAPER specs
                select(ProjectID,AccessPoint),
              by="ProjectID") %>% 
    left_join(Affiliation %>%
                select(ProjectID, ResProjectID) %>%
                group_by(ProjectID) %>%
                summarise(affiliated_with = toString(ResProjectID)) %>%  #What is this doing? why group_by and summarise during the join?
                ungroup(),
              by = "ProjectID") %>%
    left_join(ProjectCoC %>%
                select(ProjectID, CoCCode, Geocode) %>% 
                group_by(ProjectID) %>%
                summarise(coc_codes = toString(CoCCode),
                          geocodes = toString(Geocode)) %>% 
                ungroup(),
              by = "ProjectID") %>%
    left_join(filtered_enrollments %>%
                group_by(ProjectID) %>%
                summarise(active_clients = n_distinct(PersonalID, na.rm = TRUE),
                          active_households = n_distinct(PersonalID[RelationshipToHoH == 1], 
                                                         na.rm = TRUE)),
              by = "ProjectID") %>%
    mutate(software_name = "generated by Data Lab",
           report_start_date = report_start_date,
           report_end_date = report_end_date) %>%
    select(OrganizationName, OrganizationID, ProjectName, ProjectID,
           ProjectType,RRHSubType,AccessPoint,ResidentialAffiliation, affiliated_with, #Included RRHSubType,AccessPoint here
           coc_codes, geocodes, VictimServiceProvider,
           software_name, report_start_date, report_end_date,
           active_clients, active_households) %>%
    `colnames<-`(c("Organization Name", "Organization ID",
                   "Project Name", "Project ID",
                   "HMIS Project Type", "RRH Subtype", "Coordinated Entry Access Point", # I added two columns to match changes to the APR_CAPER Specs
                   "Affiliated with a residential project",
                   "Project IDs of affiliations", "CoC Number", 
                   "Geocode", "Victim Service Provider",
                   "HMIS Software Name and Version Number", "Report Start Date",
                   "Report End Date", "Total Active Clients",
                   "Total Active Households"))
}

get_household_info <- function(filtered_enrollments, 
                               client_table = client_plus,
                               return_type = "person") {
  
  data <- filtered_enrollments %>%
    inner_join(client_table, by = "PersonalID") %>%
    group_by(HouseholdID) %>%
    mutate(adults = max(if_else(age_group == "Adults", 1, 0)),
           children = max(if_else(age_group == "Children", 1, 0)),
           unknown = max(if_else(age_group %in% c("Client.Does.Not.Know.or.Prefers.Not.to.Answer", "Data.Not.Collected"), 1, 0)),
           HoH_HMID = ifnull(suppressWarnings(min(case_when(
             RelationshipToHoH == 1 &
               ProjectType %in% c(3, 13) ~ MoveInDate
           ), na.rm = TRUE)), NA),
           HoH_ADHS = suppressWarnings(min(case_when(
             RelationshipToHoH == 1 ~ DateToStreetESSH
           ), na.rm = TRUE)),
           HoH_EntryDate = suppressWarnings(min(case_when(
             RelationshipToHoH == 1 ~ EntryDate
           ), na.rm = TRUE))) %>%
    ungroup() %>%
    # may need to add a case_when(HoH_HMID > EntryDate ~ NULL)
    # condition here, pending AAQ 205645
    mutate(household_type = case_when(
      adults == 1 &
        children == 1 ~ "AdultsAndChildren",
      unknown == 1 ~ "Unknown",
      adults == 1 ~ "AdultsOnly",
      TRUE ~ "ChildrenOnly"
    )) 
  
  if (return_type == "person") {
    data %>%
      select(PersonalID, household_type, HoH_HMID, HoH_ADHS, HoH_EntryDate)
  } else {
    data %>%
      select(HouseholdID, household_type, HoH_HMID, HoH_ADHS, HoH_EntryDate) %>%
      distinct()
  }
}


# create first DQ table in glossary
create_dq_Q1 <- function(filtered_enrollments) {  # Changed all references of Client.Does.Not.Know.or.Prefers.Not.to.Answer to Client.Does.Not.Know.or.Prefers.Not.to.Answer
  DQ1_data <- filtered_enrollments %>%
    inner_join(Client %>%
                 select(-c(ExportID, DOB)), by = "PersonalID")
  
  DQ1_name <- DQ1_data %>%
    mutate(dq_flag = case_when(
      NameDataQuality %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer", # Changed from Client.Does.Not.Know.or.Prefers.Not.to.Answer
      NameDataQuality == 99 |
        is.na(FirstName) |
        is.na(LastName) ~ "Information.Missing",
      NameDataQuality == 2 ~ "Data.Issues",
      TRUE ~ "OK")) %>%
    select(PersonalID, FirstName, LastName, NameDataQuality, dq_flag)
  
  DQ1_ssn <- DQ1_data %>%
    mutate(sequential = lapply(SSN, sequential_ssn),
           dq_flag = case_when(
             SSNDataQuality %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer", # changed from Client.Does.Not.Know.or.Prefers.Not.to.Answer
             SSNDataQuality == 99 |
               #  below is what the data standards currently say
               # is.na(SSNDataQuality) ~ "Information.Missing",
               #  below is how the data standards are generally interpreted
               is.na(SSN) ~ "Information.Missing",
             #  no change here--these all apply because the SSN has to be
             #  exported as 9 digits
             SSNDataQuality == 2 |
               suppressWarnings(is.na(as.numeric(SSN))) |
               nchar(SSN) != 9 |
               substr(SSN, 1, 3) == "000" |
               substr(SSN, 1, 3) == "666" |
               substr(SSN, 1, 1) == "9" |
               substr(SSN, 4, 5) == "00" |
               substr(SSN, 6, 9) == "0000" |
               SSN %in% c("111111111", "222222222", "333333333",
                          "444444444", "555555555", "666666666",
                          "777777777", "888888888", "999999999") |
               sequential == TRUE
             ~ "Data.Issues",
             TRUE ~ "OK")) %>%
    select(PersonalID, SSN, SSNDataQuality, dq_flag)
  
  DQ1_dob <- DQ1_data %>%
    mutate(dq_flag = case_when(
      DOBDataQuality %in% c(8, 9) &
        is.na(DOB) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
      is.na(DOBDataQuality) |
        (DOBDataQuality == 99 &
           is.na(DOB)) ~ "Information.Missing",
      DOBDataQuality == 2 |
        (DOBDataQuality %in% c(8, 9, 99) &
           !is.na(DOB)) |
        DOB < mdy("1/1/1915") |
        DOB > DateCreated |
        (DOB >= EntryDate &
           (age_group == "Adults" |
              RelationshipToHoH == 1)) ~ "Data.Issues",
      TRUE ~ "OK")) %>%
    select(PersonalID, DOB, DOBDataQuality, DateCreated, age_group,
           RelationshipToHoH, dq_flag)
  
  DQ1_race <- DQ1_data %>%
    mutate(dq_flag = case_when(
      RaceNone %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
      RaceNone == 99 |
        (AmIndAKNative == 0 &
           Asian == 0 &
           BlackAfAmerican == 0 &
           HispanicLatinaeo == 0 &
           MidEastNAfrican == 0 &
           NativeHIPacific == 0 &
           White == 0) ~ "Information.Missing",
      TRUE ~ "OK")) %>%
    select(PersonalID, AmIndAKNative, Asian, BlackAfAmerican,
           HispanicLatinaeo, MidEastNAfrican,
           NativeHIPacific, White, RaceNone, dq_flag)
  
  DQ1_gender <- DQ1_data %>%
    mutate(dq_flag = case_when(
      GenderNone %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
      GenderNone == 99 |
        (Woman == 0 &
           Man == 0 &
           NonBinary == 0 &
           CulturallySpecific == 0 &
           Transgender == 0 &
           Questioning == 0 &
           !(DifferentIdentity == 1 &
               !is.na(DifferentIdentityText))) ~ "Information.Missing",
      TRUE ~ "OK")) %>%
    select(PersonalID, Woman, Man, NonBinary, CulturallySpecific, Transgender, 
           Questioning, DifferentIdentity, DifferentIdentityText, GenderNone, 
           dq_flag)
  
  columns <- c("DataElement", "Client.Does.Not.Know.or.Prefers.Not.to.Answer", 
               "Information.Missing", "Data.Issues", "OK")
  
  DQ1 <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), columns) %>%
    mutate(across(DataElement, factor)) 
  
  DQ1_detail <- filtered_enrollments %>%
    select(all_of(standard_detail_columns))
  
  elements <- list("Name", "SSN", "DOB", "Race", "Gender")
  
  for (element in elements) {
    table <- get(paste0("DQ1_", tolower(element))) #this loop creates the table for the data-elements
    
    detail_title <- paste0(element, "_DQ")
    DQ1_detail <- DQ1_detail %>%
      left_join(table %>%
                  select(-intersect(
                    colnames(DQ1_detail),
                    colnames(table)[colnames(table) != "PersonalID"])), 
                by = "PersonalID") %>%
      rename({{detail_title}} := dq_flag)
    
    table <- table %>%
      group_by(dq_flag) %>%
      summarise(Clients = n()) %>% 
      pivot_wider(names_from = "dq_flag", values_from = "Clients") %>%
      mutate(DataElement = element)
    
    DQ1 <- DQ1 %>%
      full_join(table, by = intersect(columns, colnames(table)))
    
    rm(table)
    
    if (exists("error_clients")) {
      error_clients <- error_clients %>%
        union(get(paste0("DQ1_", tolower(element))) %>%
                filter(dq_flag != "OK") %>%
                select(PersonalID))
      
    } else {
      error_clients <- get(paste0("DQ1_", tolower(element))) %>%
        filter(dq_flag != "OK") %>%
        select(PersonalID)
    }
  }
  
  DQ1 <- DQ1 %>%
    select(-OK) %>%
    mutate("Client.Does.Not.Know.or.Prefers.Not.to.Answer" = if_else(is.na(`Client.Does.Not.Know.or.Prefers.Not.to.Answer`), 
                                                     0, as.double(`Client.Does.Not.Know.or.Prefers.Not.to.Answer`)),
           Information.Missing = if_else(is.na(Information.Missing), 
                                         0, as.double(Information.Missing)),
           Data.Issues = if_else(is.na(Data.Issues), 
                                 0, as.double(Data.Issues)),
           Total = `Client.Does.Not.Know.or.Prefers.Not.to.Answer` + Information.Missing + Data.Issues) %>%
    add_row(DataElement = "Overall Score", 
            `Client.Does.Not.Know.or.Prefers.Not.to.Answer` = 0, Information.Missing = 0, Data.Issues = 0, 
            Total = nrow(unique(error_clients))) %>%
    mutate("% of Issue Rate" = decimal_format(Total / Q5a$Count.of.Clients.for.DQ[1], 4)) #changed from ErrorRate to"% of Issue Rate"
  
  DQ1[DQ1$DataElement == "Overall Score", c("Client.Does.Not.Know.or.Prefers.Not.to.Answer",
                                            "Information.Missing")] <- NA
  DQ1$Data.Issues[4:6] <- NA
  
  DQ1_results <- list()
  DQ1_results[[1]] <- DQ1
  DQ1_results[[2]] <- DQ1_detail
  DQ1_results
}

# Q8a on the CoC APR, CAPER, and CE APR
households_served_table <- function(filtered_enrollments,
                                    type = "CoC APR") {
  
  if (type == "CE APR") {
    hh_served_detail <- filtered_enrollments %>%
      select(all_of(standard_detail_columns)) %>%
      mutate(count_as_household = RelationshipToHoH == 1)
    
    hh_served <- hh_served_detail %>%
      filter(count_as_household) %>% 
      mutate(client_group = "Total Households") %>%
      return_household_groups(., client_group, "Total Households") 
    
    hh_served[2,1] <- "For PSH & RRH – the total households served who moved into housing"
    
  } else {
    hh_served_detail <- filtered_enrollments %>%
      select(all_of(housing_program_detail_columns)) %>%
      mutate(count_as_household = RelationshipToHoH == 1,
             count_as_move_in_household = RelationshipToHoH == 1 &
               HoH_HMID <= report_end_date)
    
    hh_served_all <- hh_served_detail %>%
      filter(count_as_household) %>% 
      mutate(client_group = "Total Households") %>%
      return_household_groups(., client_group, "Total Households") 
    
    hh_served_moved_in <- hh_served_detail %>%
      filter(count_as_move_in_household) %>% 
      mutate(client_group = "For PSH & RRH – the total households 
served who moved into housing") %>%
      return_household_groups(., client_group, "For PSH & RRH – the total households 
served who moved into housing") 
    
    hh_served <- hh_served_all %>%
      union(hh_served_moved_in)
  }
  
  hh_served_results <- list()
  hh_served_results[[1]] <- hh_served
  hh_served_results[[2]] <- hh_served_detail
  hh_served_results
}

get_relevant_events <- function(filtered_enrollments) {
  
  event_prefixes <- rep("Event_", length(Event))
  event_prefixes[[which(colnames(Event) == "PersonalID")]] <- ""
  
  individual_cutoff_dates <- filtered_enrollments %>%
    select(PersonalID) %>%
    left_join(Assessment %>%
                filter(PersonalID %in% filtered_enrollments$PersonalID &
                         AssessmentDate > report_end_date &
                         AssessmentDate <= report_end_date + days(90)) %>%
                select(PersonalID, AssessmentDate),
              by = "PersonalID") %>%
    group_by(PersonalID) %>%
    arrange(desc(AssessmentDate)) %>%
    slice(1L) %>%
    ungroup() %>%
    rename(cutoff_date = AssessmentDate)
  
  filtered_enrollments %>%
    inner_join(Event %>%
                 left_join(EventTypes, by = "Event") %>%
                 `colnames<-`(c(paste0(event_prefixes, colnames(Event)), "Label")), 
               by = "PersonalID") %>%
    left_join(individual_cutoff_dates, by = "PersonalID") %>%
    filter(RelationshipToHoH == 1 &
             Event_EventDate >= AssessmentDate &
             Event_EventDate <= report_end_date + days(90) &
             (is.na(cutoff_date) |
                Event_EventDate < cutoff_date)) %>%
    select(all_of(ce_detail_columns), Event_Event, Event_EnrollmentID, 
           Event_EventDate, Event_ProbSolDivRRResult, Event_ReferralResult,
           Event_ReferralCaseManageAfter, Label) %>%
    mutate(same_enrollment = Event_EnrollmentID == EnrollmentID,
           test = is.na(ExitDate)) %>%
    arrange(desc(same_enrollment), desc(Event_EventDate)) 
}

decimal_format <- function(value, decimal_places = 2) {
  if(is.data.frame(value)) {
    for (relevant_column in 
         colnames(value)[sapply(value, class) %in% c("integer", "numeric")]) {
      value[[relevant_column]] <- decimal_format(value[[relevant_column]],
                                                 decimal_places)
    }
  } else {
    if(length(value) > 0) {
      value <- ifnull(value, 0)
      value <- trimws(format(round(value, decimal_places), 
                             nsmall = decimal_places))
    } 
  }
  value
}


# provides client counts by race for a given grouping
return_race_groups <- function(APR_dataframe, grouped_by = grouped_by, 
                                    group_list = group_list,
                                    split_by_age = FALSE) {
  
  potential_table <- APR_dataframe %>%
    group_by({{grouped_by}}) %>%
      select({{grouped_by}}, all_of(unname(race_columns))) %>%
    summarize(across(all_of(unname(race_columns)),
                     #  we can use sum() here instead of a distinct count
                     #  because these columns are non-nullable binary fields
                     #  and we're working with a deduplicated data
                     ~ sum(., na.rm = TRUE))) %>% 
    # rename_at(vars(names$vars), ~ names$labels)
    rename(!!!race_columns)
  
  deparsed_grouped_by <- deparse(substitute(grouped_by))

  as.data.frame(group_list) %>%
    rename({{grouped_by}} := group_list) %>%
    full_join(potential_table, by = deparsed_grouped_by) %>%
    ifnull(., 0)
  
  # potential_table
  
}

# adds chronic information to a dataframe 
# column `chronic` includes levels Y, N, Information.Missing,
#   and Client.Does.Not.Know.or.Prefers.Not.to.Answer

add_chronicity_data <- function(df_of_active_enrollments,
                                f_Enrollment = Enrollment,
                                f_Project = Project,
                                f_Client = Client,
                                f_report_start_date = report_start_date) {
  
  additional_disability_check <- disability_table %>%
    filter(DataCollectionStage == 1 &
             DisabilityResponse != 0 &
             indefinite_and_impairs &
             EnrollmentID %in% df_of_active_enrollments$EnrollmentID) %>%
    group_by(EnrollmentID) %>%
    summarise() %>%
    ungroup() %>%
    mutate(has_disability = 1)
  
  chronic_individual <- f_Enrollment %>%
    filter(EnrollmentID %in% df_of_active_enrollments$EnrollmentID) %>%
    select(EnrollmentID, DisablingCondition, ProjectID, EntryDate,
           LivingSituation, LOSUnderThreshold, PreviousStreetESSH,
           DateToStreetESSH, TimesHomelessPastThreeYears,
           MonthsHomelessPastThreeYears) %>%
    left_join(additional_disability_check, 
              by = "EnrollmentID") %>%
    left_join(f_Project %>%
                select(ProjectID, ProjectType),
              by = "ProjectID") %>%
    mutate(
      disabling_condition_for_chronic = case_when(
        DisablingCondition == 1 |
          has_disability == 1 ~ "Y",
        DisablingCondition == 0 ~ "N",
        DisablingCondition %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
        TRUE ~ "Information.Missing"),
      homeless_year_prior = trunc((DateToStreetESSH %--% EntryDate) / years(1)) >= 1 &
        !is.na(DateToStreetESSH),
      four_or_more_times = case_when(
        TimesHomelessPastThreeYears == 4 ~ "Y",
        TimesHomelessPastThreeYears %in% c(1, 2, 3) ~ "N",
        TimesHomelessPastThreeYears %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
        TRUE ~ "Information.Missing"),
      twelve_or_more_months = case_when(
        MonthsHomelessPastThreeYears >= 112 ~ "Y",
        MonthsHomelessPastThreeYears %in% 101:111 ~ "N",
        MonthsHomelessPastThreeYears %in% c(8, 9) ~ "Client.Does.Not.Know.or.Prefers.Not.to.Answer",
        TRUE ~ "Information.Missing"
      ),
      chronic = case_when(
        disabling_condition_for_chronic != "Y" ~ disabling_condition_for_chronic,
        ProjectType %in% c(0, 1, 4, 8) ~ case_when(
          homeless_year_prior ~ "Y",
          four_or_more_times != "Y" ~ four_or_more_times,
          TRUE ~ twelve_or_more_months),
        is.na(LivingSituation) ~ "Information.Missing",
        LivingSituation %in% 100:199 ~ case_when(
          homeless_year_prior ~ "Y",
          four_or_more_times != "Y" ~ four_or_more_times,
          TRUE ~ twelve_or_more_months),
        LivingSituation %in% c(0:99, 200:499) ~ case_when(
          is.na(LOSUnderThreshold) ~ "Information.Missing",
          LOSUnderThreshold == 0 ~ "N",
          is.na(PreviousStreetESSH) ~ "Information.Missing",
          PreviousStreetESSH == 0 ~ "N",
          homeless_year_prior ~ "Y",
          four_or_more_times != "Y" ~ four_or_more_times,
          TRUE ~ twelve_or_more_months),
        TRUE ~ "ERROR")) %>%
    select(EnrollmentID, chronic)
  
  chronic_household <- f_Enrollment %>%
    filter(HouseholdID %in% df_of_active_enrollments$HouseholdID) %>%
    select(PersonalID, EnrollmentID, HouseholdID, EntryDate, RelationshipToHoH) %>%
    left_join(f_Client %>%
                select(PersonalID, DOB), 
              by = "PersonalID") %>%
    mutate(date_for_age = (if_else(
      EntryDate <= f_report_start_date,
      f_report_start_date,
      EntryDate)),
      age = trunc((DOB %--% date_for_age) / years(1)),
      chronic_age_group = case_when(age >= 18 ~ "adult",
                                    age < 18 ~ "child",
                                    TRUE ~ "unknown")) %>%
    left_join(chronic_individual %>%
                mutate(numeric_chronic = case_when(
                  chronic == "Y" ~ 1,
                  chronic == "N" ~ 2,
                  chronic == "Client.Does.Not.Know.or.Prefers.Not.to.Answer" ~ 3,
                  chronic == "Information.Missing" ~ 4
                )), 
              by = "EnrollmentID") %>%
    group_by(HouseholdID) %>%
    mutate(first_entry_date = min(EntryDate)) %>%
    filter(EntryDate == first_entry_date) %>%
    mutate(
      min_chronicity = min(numeric_chronic, na.rm = TRUE),
      HoH_chronicity = min(case_when(RelationshipToHoH == 1 ~ numeric_chronic), 
                           na.rm = TRUE),
      HoH_or_adult_chronicity = min(case_when(
        RelationshipToHoH == 1 |
          chronic_age_group == "adult" ~ numeric_chronic), 
        na.rm = TRUE),
      new_chronic = factor(
        case_when(
          min_chronicity == 1 ~ min_chronicity,
          (HoH_or_adult_chronicity == 2 &
             chronic_age_group == "child") ~ HoH_or_adult_chronicity,
          HoH_or_adult_chronicity %in% c(3, 4) ~ HoH_chronicity,
          TRUE ~ numeric_chronic
        ), 
        levels = c(1, 2, 3, 4),
        labels = c("Y", "N", "Client.Does.Not.Know.or.Prefers.Not.to.Answer", "Information.Missing"))) %>%
    ungroup() %>%
    select(EnrollmentID, new_chronic)
  
  chronicity_data <- chronic_household %>%
    full_join(chronic_individual,
              by = "EnrollmentID") %>%
    mutate(chronic = if_else(is.na(new_chronic), chronic, as.character(new_chronic))) %>%
    select(-new_chronic)
  
  df_of_active_enrollments %>%
    left_join(chronicity_data,
              by = "EnrollmentID")
  
}
