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

items_to_keep <- c(items_to_keep,
                   c("spm_2", "spm_2_dq"))
# lookback <- 730
lookback <- 90
spm_2_rows <- paste0("Exit.was.from.", c("SO", "ES", "TH", "SH", "PH"))
spm_2_colnames <- c("spm_2_rows",
                    "Total.Number.of.Persons.who.Exited.to.a.Permanent.Housing.Destination.(2.Years.Prior)",
                    "Number.Returning.to.Homelessness.in.Less.than.6.Months.(0.-.180.days)",
                    "Percentage.of.Returns.in.Less.than.6.Months.(0.-.180.days)",
                    "Number.Returning.to.Homelessness.from.6.to.12.Months.(181.-.365.days)",
                    "Percentage.of.Returns.from.6.to.12.Months.(181.-.365.days)",
                    "Number.Returning.to.Homelessness.from.13.to.24.Months.(366.-.730.days)",
                    "Percentage.of.Returns.from.13.to.24.Months.(366.-.730.days)",
                    "Number.of.Returns.in.2.Years",
                    "Percentage.of.Returns.in.2.Years")

return_program_types <- c(0, 1, 2, 3, 4, 8, 9, 10, 13)
ph_program_types <- c(3, 9, 10, 13)
housing_program_types <- c(2, ph_program_types)

spm_2_enrollments <- enrollment_data %>%
  filter(ProjectType %in% return_program_types) %>%
  mutate(
    ExitDateAdj = case_when(
      is.na(ExitDate) | ExitDate > report_end_date ~ report_end_date, 
      TRUE ~ ExitDate)) %>%
  select(PersonalID, EnrollmentID, EntryDate, ExitDate, ProjectType, 
         Destination, ExitDateAdj) 

spm_2_exits <- spm_2_enrollments %>%
  filter(ExitDate >= report_start_date %m-% days(lookback) &
           ExitDate <= report_end_date %m-% days(lookback) &
           !is.na(ExitDate) &
           Destination %in% c(400:499)) %>%
  group_by(PersonalID) %>%
  arrange(ExitDate, EnrollmentID) %>%
  slice(1L) %>%
  ungroup()

spm_2_possible_ph_returns <- spm_2_enrollments %>%
  filter(ProjectType %in% ph_program_types) %>%
  left_join(spm_2_enrollments %>%
              filter(ProjectType %in% ph_program_types) %>%
              setNames(c("PersonalID", 
                         paste0("R_", colnames(spm_2_enrollments)[2:7]))),
            by = "PersonalID",
            relationship = "many-to-many") %>%
  filter(EnrollmentID != R_EnrollmentID &
           (EntryDate <= R_EntryDate |
              EntryDate > R_ExitDateAdj %m+% days(14))) %>%
  select(colnames(spm_2_enrollments))

spm_2_possible_other_returns <- spm_2_enrollments %>%
  filter(ProjectType %nin% ph_program_types)

spm_2_all_possible_returns <- spm_2_possible_ph_returns %>%
  full_join(spm_2_possible_other_returns,
            by = colnames(spm_2_enrollments))

spm_2_dq <- spm_2_exits %>%
  left_join(spm_2_all_possible_returns %>%
              setNames(c("PersonalID", 
                         paste0("R_", colnames(spm_2_enrollments)[2:7]))),
            by = "PersonalID",
            relationship = "many-to-many") %>%
  filter(EnrollmentID != R_EnrollmentID &
           ExitDate <= R_EntryDate &
           R_EntryDate <= ExitDate %m+% days(730) &
           (R_ProjectType %nin% ph_program_types |
              ExitDate <= R_EntryDate %m+% days(14))) %>%
  group_by(EnrollmentID) %>%
  arrange(R_EntryDate) %>%
  slice(1L) %>%
  ungroup() %>%
  full_join(spm_2_exits,
            by = colnames(spm_2_enrollments)) %>%
  mutate(
    days_to_return = case_when(
      !is.na(R_EntryDate) ~ interval(ExitDate, R_EntryDate) %/% days(1)),
    return_group = case_when(
      !is.na(days_to_return) ~
        case_when(
          days_to_return >= 366 ~ "366-730",
          days_to_return >= 181 ~ "181-365",
          days_to_return >= 0 ~ "0-180")),
    spm_2_rows = factor(
      paste0("Exit.was.from.",
             case_when(
               ProjectType %in% ph_program_types ~ "PH",
               ProjectType %in% c(0, 1) ~ "ES",
               ProjectType == 2 ~ "TH",
               ProjectType == 4 ~ "SO",
               ProjectType == 8 ~ "SH"
             )),
      ordered = TRUE,
      levels = spm_2_rows))

spm_2 <- as.data.frame(spm_2_rows) %>%
  full_join(spm_2_dq %>%
              group_by(spm_2_rows) %>%
              summarise(
                total_permanent_exits =
                  n_distinct(PersonalID, na.rm = TRUE),
                returns_0_6 = 
                  n_distinct(PersonalID[return_group == "0-180"], na.rm = TRUE),
                returns_6_12 = 
                  n_distinct(PersonalID[return_group == "181-365"], na.rm = TRUE),
                returns_12_24 = 
                  n_distinct(PersonalID[return_group == "366-730"], na.rm = TRUE)
              ) %>%
              adorn_totals("row") %>%
              ungroup() %>%
              mutate(
                spm_2_rows = if_else(spm_2_rows == "Total",
                                     "Total.Returns.to.Homelessness",
                                     spm_2_rows),
                percent_returns_0_6 = round(
                  returns_0_6/ total_permanent_exits * 100, 2),
                percent_returns_6_12 = round(
                  returns_6_12 / total_permanent_exits * 100, 2),
                percent_returns_12_24 = round(
                  returns_12_24 / total_permanent_exits * 100, 2),
                total_returns = returns_0_6 + returns_6_12 + returns_12_24,
                total_percent_returns = round(
                  total_returns / total_permanent_exits * 100, 2)) %>%
              setNames(spm_2_colnames),
            by = "spm_2_rows") %>%
  ifnull(., 0)

rm(list = ls()[ls() %nin% items_to_keep]) 