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

generate_new_kits <- TRUE
compare_to_last <- FALSE
if (compare_to_last) {
  compare_to_dir <- choose.dir()}
combining_files <- FALSE

# hold_ProjectCoC <- ProjectCoC
ProjectCoC <- ProjectCoC %>%
  mutate(CoCCode = case_when(
    ProjectID %in% c(340, 780, 1647) ~ "XX-500",
    TRUE ~ CoCCode)) %>%
  union(hold_ProjectCoC %>%
          filter(ProjectID == 1647))

relevant_CoC <- "XX-501"

source("DataLab.R")

multi_CoC_projects <- ProjectCoC %>%
  group_by(ProjectID) %>%
  summarise(CoCs = uniqueN(CoCCode)) %>%
  filter(CoCs > 1)


CE_element_projects <- Project %>%
  inner_join(Enrollment %>%
               filter(EnrollmentID %in%
                        union(Assessment$EnrollmentID, Event$EnrollmentID)) %>%
               select(ProjectID) %>%
               distinct(),
             by = "ProjectID")
