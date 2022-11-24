library(readxl)
library(janitor)
library(data.table)
library(lubridate)
library(tidyverse)
library(archive)

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
      value[is.na(value) | is.nan(value) | is.infinite(value)] <- replace_with
    } 
  }
  value
}

household_group_list <- c(Total = 0, Without.Children = 0, 
                          With.Children.And.Adults = 0, With.Only.Children = 0, 
                          Unknown.Household.Type = 0)

trunc_userid <- function(df) {
  df %>%
    mutate(UserID = substr(UserID, 1, 2))
}

entry_annual_exit <- c("entry", "annual", "exit")

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


# provides distinct client counts within households for any given grouping 
return_household_groups <- function(APR_dataframe, grouped_by = grouped_by, 
                                    group_list = group_list) {

  potential_table <- APR_dataframe %>%
    group_by({{grouped_by}}) %>%
    summarise(Total = n_distinct(PersonalID),
              Without.Children = n_distinct(PersonalID[household_type == "AdultsOnly"]),
              With.Children.And.Adults = n_distinct(PersonalID[household_type == "AdultsAndChildren"]),
              With.Only.Children = n_distinct(PersonalID[household_type == "ChildrenOnly"]),
              Unknown.Household.Type = n_distinct(PersonalID[household_type == "Unknown"])) %>% 
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
        DisablingCondition %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
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
    mutate(earned_income = if_else(Earned == 1 & EarnedAmount > 0, EarnedAmount, 0),
           amounts_combined = if_else(Earned == 1 & EarnedAmount > 0, EarnedAmount, 0) +
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
               IncomeFromAnySource %in% c(8, 9), "Does.Not.Know.or.Refused", "Data.Not.Collected"),
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
  
  if(annual) {
    income_amount_categories <- annual_income_amount_categories
  }
  
  income_groups <- enrollments_total_income %>%
    group_by(total_income_group) %>%
    summarise(Income = n_distinct(PersonalID)) %>%
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
                 union(recent_household_enrollment %>%
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
        earned_income == calculated_total_income ~ "Adults with Only Earned Income (i.e., Employment Income)",
      earned_income > 0 &
        earned_income < calculated_total_income ~ "Adults with Both Earned and Other Income",
      earned_income == 0 &
        calculated_total_income > 0 ~ "Adults with Only Other Income",
      calculated_total_income == 0 ~ "Adults with No Income",
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
                                                    get(paste0(compare_to, "_", income_type, "_amount")) == 0]),
              retained_decreased = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) > get(paste0(compare_to, "_", income_type, "_amount")) &
                                                           get(paste0(compare_to, "_", income_type, "_amount")) > 0]),
              retained_same = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) == get(paste0(compare_to, "_", income_type, "_amount")) &
                                                      get(paste0(compare_to, "_", income_type, "_amount")) > 0]),
              retained_increased = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) < get(paste0(compare_to, "_", income_type, "_amount")) &
                                                           get(paste0("entry_", income_type, "_amount")) > 0]),
              gained_source = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) == 0 &
                                                      get(paste0(compare_to, "_", income_type, "_amount")) > 0]),
              did_not_have_source = n_distinct(PersonalID[get(paste0("entry_", income_type, "_amount")) == 0 &
                                                            get(paste0(compare_to, "_", income_type, "_amount")) == 0]),
              total_adults = n_distinct(PersonalID)
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
add_length_of_time_groups <- function(data, start_date, end_date, report_type) {
  start_date <- enquo(start_date) 
  end_date <- enquo(end_date) 
  
  time_groups <- data %>%
    mutate(number_of_days = 
             trunc((!!start_date %--% !!end_date) / days(1)),
           number_of_days_group = case_when(
             is.na(number_of_days) ~ "Data.Not.Collected",
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
        between(number_of_days, 0, 30) ~ "30 days or less",
        TRUE ~ number_of_days_group))
  } else if(report_type == "days_prior_to_housing") {
    time_groups <- time_groups %>%
      mutate(number_of_days_group = case_when(
        between(number_of_days, 61, 180) ~ "61 to 180 days",
        number_of_days >= 731 ~ "731 days or more",
        TRUE ~ number_of_days_group))
  }
  data %>%
    left_join(time_groups %>%
                select(EnrollmentID, number_of_days, number_of_days_group),
              by = "EnrollmentID")
}

# also used in Q22 questions
length_of_time_groups <- function(report_type, column_name) {
  if(report_type == "APR") {
    rows <- c("30 days or less", "31 to 60 days", "61 to 90 days", "91 to 180 days", 
      "181 to 365 days", "366 to 730 days (1-2 Yrs)", "731 to 1,095 days (2-3 Yrs)",
      "1,096 to 1,460 days (3-4 Yrs)", "1,461 to 1,825 days (4-5 Yrs)",
      "More than 1,825 days (>5 Yrs)")
  } else if(report_type == "days_prior_to_housing") {
    rows <- c("0 to 7 days", "8 to 14 days", "15 to 21 days", "22 to 30 days", 
              "31 to 60 days", "61 to 180 days", "181 to 365 days", 
              "366 to 730 days (1-2 Yrs)", "731 days or more", "Not yet moved into housing", "Data.Not.Collected")
  } else {
    rows <- c("0 to 7 days", "8 to 14 days", "15 to 21 days", "22 to 30 days", 
              "31 to 60 days", "61 to 90 days", "91 to 180 days", 
              "181 to 365 days", "366 to 730 days (1-2 Yrs)", "731 to 1,095 days (2-3 Yrs)",
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
create_age_groups <- function(filtered_recent_household_enrollment,
                              chronic = FALSE) {
  
  if (chronic) {
    detailed_age_group_list <- c("0 - 17", detailed_age_group_list[4:11])
    
    filtered_recent_household_enrollment <- filtered_recent_household_enrollment %>%
      mutate(detailed_age_group = case_when(
        detailed_age_group %nin% detailed_age_group_list ~ "0 - 17",
        TRUE ~ detailed_age_group
      ))
  }
  
  filtered_recent_household_enrollment %>%
    return_household_groups(., detailed_age_group, detailed_age_group_list) %>%
    adorn_totals("row")
}


# create gender group table
create_gender_groups <- function(filtered_recent_household_enrollment) {
  filtered_recent_household_enrollment %>%
    return_household_groups(., gender_combined, gender_list) %>%
    adorn_totals("row")
}


# create benefit group table
create_benefit_groups <- function(included_enrollments) {
  for(period in entry_annual_exit) {
    
    data <- get(paste0(period, "_income")) %>%
      keep_adults_only() %>%
      filter(EnrollmentID %in% included_enrollments$EnrollmentID) %>%
      select(all_of(benefit_list)) %>%
      pivot_existing_only(., "BenefitGroup", period) %>%
      left_join(BenefitTypes, by = "BenefitGroup") %>%
      mutate(BenefitGroup = OfficialBenefitName) %>%
      select(-OfficialBenefitName)
    
    assign(paste0(period, "_benefit_sources"), data)
  }
  
  entry_benefit_sources %>%
    left_join(annual_benefit_sources, by = "BenefitGroup") %>%
    left_join(exit_benefit_sources, by = "BenefitGroup") %>%
    rename(Benefit.at.Start = entryClients,
           Benefit.at.Latest.Annual.Assessment.for.Stayers = annualClients,
           Benefit.at.Exit.for.Leavers = exitClients)
}


# create destination table for APR/CAPER
create_destination_groups <- function(included_enrollments) {
  for(residence_type in c("permanent", "temporary", "institution", "other")) {
    residences_to_include <- ResidenceUses %>%
      filter(APR_ExitLocationGroup == residence_type &
               !is.na(LocationDescription)) 
    
    group_of_residences <- included_enrollments %>%
      inner_join(residences_to_include, by = c("Destination" = "Location")) %>%
      return_household_groups(., LocationDescription, residences_to_include$LocationDescription) %>%
      full_join(residences_to_include, by = "LocationDescription") %>%
      arrange(APR_ExitOrder) %>%
      adorn_totals("row") %>%
      ifnull(., 0) 
    
    group_title_row <- group_of_residences[0,]
    group_title_row[1,1] <- str_to_title(residence_type)
    
    group_of_residences <- rbind(group_title_row, group_of_residences)
    
    if (residence_type == "permanent") {
      destination_group <- group_of_residences
    } else {
      destination_group <- destination_group %>%
        union(group_of_residences)
    }
  }
  
  exit_categories <- data.frame(Outcome = possible_outcomes) %>%
    left_join(included_enrollments %>%
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
    mutate(LocationDescription = if_else(
      LocationDescription == "Total", "Subtotal", LocationDescription
    )) %>%
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
  for(residence_type in unique(na.omit(ResidenceUses$APR_PriorLocationGroup))) {
    residences_to_include <- ResidenceUses %>%
      filter(APR_PriorLocationGroup == residence_type &
               !is.na(LocationDescription) &
               !is.na(APR_PriorOrder)) %>%
      arrange(APR_PriorOrder) %>%
      mutate(LocationDescription = case_when(
        Location %in% c(8, 9) ~ "Does.Not.Know.or.Refused",
        TRUE ~ LocationDescription))
    
    group_of_residences <- included_enrollments %>%
      inner_join(residences_to_include, by = c("LivingSituation" = "Location")) %>%
      return_household_groups(., LocationDescription, residences_to_include$LocationDescription) %>%
      adorn_totals("row")
    
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
    mutate(LocationDescription = if_else(
      LocationDescription == "Total", "Subtotal", LocationDescription
    )) %>%
    ifnull(., 0)
  
  total_row <- residence_table %>%
    filter(LocationDescription == "Subtotal") %>%
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
    
    selected_cols <- c()
    if (youth) {
      hh_type_groups <- c("Without.Children", "With.Children.And.Adults", 
                          "With.Only.Children", "Unknown.Household.Type")
      exit_income <- exit_income %>%
        filter(youth == 1)
    } else {
      hh_type_groups <- c("Without.Children", "With.Children.And.Adults", 
                          "Unknown.Household.Type")
      exit_income <- exit_income %>%
        keep_adults_only()
    }
    
    for(condition_presence in c("disabling_condition",
                                "no_disabling_condition",
                                "all")) {
      
      filtered_income_information <- exit_income %>%
        filter((condition_presence == "disabling_condition" &
                  DisablingCondition == 1) |
                 (condition_presence == "no_disabling_condition" &
                    DisablingCondition == 0) |
                 (condition_presence == "all" &
                    DisablingCondition %in% c(0, 1))) %>%
        mutate(other_income_created = if_else(
          Unemployment == 1 |
            VADisabilityNonService == 1 |
            GA == 1 |
            Alimony == 1 |
            OtherIncomeSource == 1, 1, 0)) %>%
        select(c(PersonalID, household_type, all_of(income_rows_to_show))) %>%
        pivot_longer(!c(PersonalID, household_type)) %>%
        mutate(row_name = "row")
      
      unduplicated_total <- cbind(name = "total_adults", 
                                  filtered_income_information %>%
                                    mutate(row_name = "total") %>%
                                    return_household_groups(., row_name, "total") %>%
                                    select(-row_name))
      
      filtered_has_income <- filtered_income_information %>%
        filter(value == 1)
      
      unduplicated_with_income <- filtered_has_income %>%
        mutate(row_name = "has income") %>%
        return_household_groups(., row_name, "has income")
      
      unduplicated_without_income <- as.data.frame(
        cbind(name = "no_income", unduplicated_total[2:6] - unduplicated_with_income[2:6]))
      
      income_hh_type_disabling_condition_data <- as.data.frame(income_rows_to_show) %>%
        rename(name = income_rows_to_show) %>%
        left_join(filtered_has_income %>%
                    return_household_groups(., name, IncomeTypes$IncomeGroup),
                  by = "name") %>%
        rbind(., unduplicated_without_income) %>%
        rbind(., unduplicated_total) %>%
        `colnames<-`(c("name", paste0(condition_presence, "_", colnames(.)[2:6]))) %>%
        ifnull(., 0)
      
      if(condition_presence == "disabling_condition") {
        income_hh_type_disabling_condition <- income_hh_type_disabling_condition_data
      } else {
        income_hh_type_disabling_condition <- income_hh_type_disabling_condition %>%
          left_join(income_hh_type_disabling_condition_data, by = "name")
      }
    }
    
    for(household_type in hh_type_groups) {
      income_hh_type_disabling_condition <- income_hh_type_disabling_condition %>%
        mutate(!!paste0(household_type, "_percent") := ifnull(
          get(paste0("disabling_condition_", household_type)) / 
            get(paste0("all_", household_type)), 0)
        )
      
      for (disabling_condition_present in c("disabling_condition", "no_disabling_condition",
                                            "all", "percent")) {
        if (disabling_condition_present == "percent") {
          selected_cols <- c(selected_cols, paste0(household_type, "_",
                                                   disabling_condition_present))
        } else {
          selected_cols <- c(selected_cols, paste0(disabling_condition_present, "_",
                                                   household_type))
        }
      }
    }
    
    income_hh_type_disabling_condition <- income_hh_type_disabling_condition %>%
      select(c(name, all_of(selected_cols)) )
    
    no_income_row <- match(TRUE, income_hh_type_disabling_condition$name == "no_income")
    total_row <- match(TRUE, income_hh_type_disabling_condition$name == "total_adults")
    
    income_hh_type_disabling_condition$Without.Children_percent[c(no_income_row, total_row)] <- NA
    income_hh_type_disabling_condition$With.Children.And.Adults_percent[c(no_income_row, total_row)] <- NA
    income_hh_type_disabling_condition$Unknown.Household.Type_percent[c(no_income_row, total_row)] <- NA
    
    income_hh_type_disabling_condition
  }
}

# create contact table for Q9 contacts and engagements
create_contact_table <- function(filtered_enrollments, first_CLS_group,
                                 all_CLS_for_Q9) {
  
  data <- filtered_enrollments %>%
    left_join(first_CLS_group, by = "EnrollmentID") %>%
    left_join(all_CLS_for_Q9 %>%
                group_by(EnrollmentID) %>%
                summarise(Contacts = n()) %>%
                mutate(ContactGroup = case_when(
                  Contacts == 1 ~ "Once",
                  Contacts <= 5 ~ "2-5 times",
                  Contacts <= 9 ~ "6-9 times",
                  TRUE ~ "10+ times")),
              by = "EnrollmentID") %>% 
    group_by(ContactGroup) %>%
    summarise(all_persons_contacted = n_distinct(PersonalID),
              first_contact_not_LH = n_distinct(PersonalID[CLS_group == "Not LH"]),
              first_contact_LH = n_distinct(PersonalID[CLS_group == "LH"]),
              first_contact_unknown = n_distinct(PersonalID[CLS_group == "Unknown"])) %>%
    adorn_totals("row")
  
  as.data.frame(contact_groups) %>%
    `colnames<-`(c("ContactGroup")) %>% 
    full_join(data, by = "ContactGroup") %>%
    ifnull(., 0)
}

# generate table with general summary information about the project
create_summary_table <- function(filtered_enrollments, column_name) {
  filtered_enrollments %>%
    left_join(client_plus, by = "PersonalID") %>%
    left_join(chronicity_data, by = "EnrollmentID") %>%
    summarise(Total.number.of.persons.served = n_distinct(PersonalID),
              Number.of.adults.age.18.or.over = uniqueN(PersonalID[age_group == "Adults"]),
              Number.of.children.under.age.18 = uniqueN(PersonalID[age_group == "Children"]),
              Number.of.persons.with.unknown.age = uniqueN(PersonalID[age_group %in% c("Does.Not.Know.or.Refused", "Data.Not.Collected")]),
              Number.of.leavers = uniqueN(PersonalID[!is.na(ExitDate)]),
              Number.of.adult.leavers = uniqueN(PersonalID[age_group == "Adults" &
                                                             !is.na(ExitDate)]),
              Number.of.adult.and.head.of.household.leavers = uniqueN((PersonalID[(age_group == "Adults" |
                                                                                     RelationshipToHoH == 1) &
                                                                                    !is.na(ExitDate)])),
              Number.of.stayers = uniqueN(PersonalID[is.na(ExitDate)]),
              Number.of.adult.stayers = uniqueN(PersonalID[age_group == "Adults" &
                                                             is.na(ExitDate)]),
              Number.of.veterans = uniqueN(PersonalID[new_veteran_status == 1]),
              Number.of.chronically.homeless.persons = uniqueN(PersonalID[chronic == "Y"]),
              Number.of.youth.under.age.25 = uniqueN(PersonalID[youth == 1]),
              # can't figure out why this one isn't working
              Number.of.parenting.youth.under.age.25.with.children = uniqueN(PersonalID[has_children == 1 & youth == 1]),
              Number.of.adult.heads.of.household = uniqueN(PersonalID[age_group == "Adults" &
                                                                        RelationshipToHoH == 1]),
              Number.of.child.and.unknown.age.heads.of.household = uniqueN(PersonalID[age_group != "Adults" &
                                                                                        RelationshipToHoH == 1]),
              Heads.of.households.and.adult.stayers.in.the.project.365.days.or.more = uniqueN(PersonalID[is.na(ExitDate) &
                                                                                                           trunc((EntryDate %--% report_end_date) / days(1)) >= 365 &
                                                                                                           (age_group == "Adults" |
                                                                                                              RelationshipToHoH == 1)])
    ) %>% 
    mutate(rowname = column_name) %>% 
    pivot_longer(!rowname, names_to = "Group", values_to = "values") %>% 
    pivot_wider(names_from = "rowname", values_from = "values")
}
