funding_sources <- data.frame(matrix(
  c(
    # 1, "HUD: CoC – Homelessness Prevention (High Performing Comm. Only)",
    2, "HUD: CoC – Permanent Supportive Housing",
    3, "HUD: CoC – Rapid Re-Housing",
    4, "HUD: CoC – Supportive Services Only",
    5, "HUD: CoC – Transitional Housing",
    # 6, "HUD: CoC – Safe Haven",
    # 7, "HUD: CoC – Single Room Occupancy (SRO)",
    8, "HUD: ESG – Emergency Shelter (operating and/or essential services)",
    9, "HUD: ESG – Homelessness Prevention",
    10, "HUD: ESG – Rapid Rehousing",
    11, "HUD: ESG – Street Outreach",
    # 12, "HUD: Rural Housing Stability Assistance Program",
    # 13, "HUD: HOPWA – Hotel/Motel Vouchers",
    # 14, "HUD: HOPWA – Housing Information",
    # 15, "HUD: HOPWA – Permanent Housing (facility based or TBRA)",
    # 16, "HUD: HOPWA – Permanent Housing Placement",
    # 17, "HUD: HOPWA – Short-Term Rent, Mortgage, Utility assistance",
    # 18, "HUD: HOPWA – Short-Term Supportive Facility",
    # 19, "HUD: HOPWA – Transitional Housing (facility based or TBRA)",
    # 20, "HUD: HUD/VASH",
    21, "HHS: PATH – Street Outreach & Supportive Services Only"
    # ,
    # 22, "HHS: RHY – Basic Center Program (prevention and shelter)",
    # 23, "HHS: RHY – Maternity Group Home for Pregnant and Parenting Youth",
    # 24, "HHS: RHY – Transitional Living Program",
    # 25, "HHS: RHY – Street Outreach Project",
    # 26, "HHS: RHY – Demonstration Project",
    # 27, "VA: CRS Contract Residential Services",
    # 30, "VA: Community Contract Safe Haven Program",
    # 33, "VA: Supportive Services for Veteran Families",
    # 34, "N/A",
    # 35, "HUD: Pay for Success",
    # 36, "HUD: Public and Indian Housing (PIH) Programs",
    # 37, "VA: Grant Per Diem – Bridge Housing",
    # 38, "VA: Grant Per Diem – Low Demand",
    # 39, "VA: Grant Per Diem – Hospital to Housing",
    # 40, "VA: Grant Per Diem – Clinical Treatment",
    # 41, "VA: Grant Per Diem – Service Intensive Transitional Housing",
    # 42, "VA: Grant Per Diem – Transition in Place",
    # 43, "HUD: CoC – Youth Homeless Demonstration Program (YHDP)",
    # 44, "HUD: CoC – Joint Component TH/RRH",
    # 45, "VA: Grant Per Diem – Case Management/Housing Retention",
    # 46, "Local or Other Funding Source (Please Specify)",
    # 47, "HUD: ESG – CV",
    # 48, "HUD: HOPWA – CV",
    # 49, "HUD: CoC – Joint Component RRH/PSH",
    # 50, "HUD: HOME",
    # 51, "HUD: HOME (ARP)",
    # 52, "HUD: PIH (Emergency Housing Voucher)"
    ), ncol = 2, byrow = TRUE)) %>%
  'colnames<-'(c("Funder", "FunderName")) %>%
  mutate(Funder = as.integer(Funder))

project_options <- Enrollment %>%
  filter(EntryDate >= report_start_date & 
           EntryDate <= report_end_date) %>%
  group_by(ProjectID) %>%
  summarise(households = uniqueN(HouseholdID)) %>%
  inner_join(Project %>%
               select(ProjectID, ProjectType, RRHSubType),
             by = "ProjectID") %>%
  inner_join(Funder %>%
               filter(is.na(EndDate)) %>%
               select(ProjectID, Funder),
             by = "ProjectID") %>%
  inner_join(funding_sources,
             by = "Funder")
