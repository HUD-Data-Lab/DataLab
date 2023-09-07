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
                   "spm_3_dq", "spm_3")

# 3.2. System Performance Measure 3.2: Persons Experiencing Homelessness ----

spm_3_rows <- paste0(c("Emergency.Shelter", "Safe.Haven", "Transitional.Housing"),
                     ".Total")

spm_3_dq <- active_enrollments %>% 
  filter(
    (Method2 & ProjectType %in% c(1)) | (Method1 & ProjectType %in% c(0,2,8))) %>%
  select(PersonalID, ProjectType, EntryDate, MoveInDate, ExitDate,
         Method1, Method2, Method5) %>%
  mutate(
    spm_3_rows = paste0(case_when(
               ProjectType %in% c(0, 1) ~ "Emergency.Shelter",
               ProjectType == 8 ~ "Safe.Haven",
               ProjectType == 2 ~ "Transitional.Housing"), 
               ".Total")) 

spm_3 <- spm_3_dq %>%
  group_by(spm_3_rows = "Total.PIT.Count.of.sheltered.and.unsheltered.persons") %>%
  summarise(Current.FY = n_distinct(PersonalID, na.rm = TRUE)) %>%
  ungroup() %>%
  full_join(as.data.frame(spm_3_rows) %>%
              full_join(spm_3_dq %>%
                          group_by(spm_3_rows) %>%
                          summarise(Current.FY = n_distinct(PersonalID, na.rm = TRUE)) %>%
                          ungroup(),
                        by = c("spm_3_rows")),
            by = c("spm_3_rows", "Current.FY")) %>%
  mutate(Previous.FY = NA,
         Difference = NA) %>%
  select(spm_3_rows, Previous.FY, Current.FY, Difference)

rm(list = ls()[ls() %nin% items_to_keep]) 
