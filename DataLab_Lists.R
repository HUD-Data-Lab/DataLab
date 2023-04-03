# ------------------------------------------------------------------------------
# ----------------------- HMIS CSV File List With Types ------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER

hmis_csvs <- list(Affiliation = "cccTTcTc", 
                  Assessment = "cccDciiiTTcTc", 
                  AssessmentQuestions = "ccccciccTTcTc", 
                  AssessmentResults = "ccccccTTcTc",
                  Client = "ccccciciDiiiiiiiiiiiiiciiiiiiiiiiiiiTTcTc", 
                  CurrentLivingSituation = "cccDiciiiiicTTcTc", 
                  Disabilities = "cccDiiiiiiiiiiiTTcTc", 
                  EmploymentEducation = "cccDiiiiiiTTcTc",
                  Enrollment = "cccDciiiiiDiiiDDDiiiicccciiiDiiiiciiiiiiiiiiiiciiiiiiiiiiiiiiiiiiiiTTcTc", 
                  EnrollmentCoC = "cccccDciTTcTc", 
                  Event = "cccDiiiciDTTcTc",
                  Exit = "cccDiciiiiiiiiiiiiiiiiiiiiiiiiiDiiiiiiTTcTc",
                  Export = "ciccccciiTTTccciii", 
                  Funder = "cciccDDTTcTc", 
                  HealthAndDV = "cccDiiiiiiiDiiiiiTTcTc", 
                  IncomeBenefits = "cccDididididididididididididididididciiiiiiiciiiiiiiiiiiiiiiiiiiiciiiiiiiiTTcTc", 
                  Inventory = "ccciiiiiiiiiiiiDDTTcTc", 
                  Organization = "ccncTTcTn", 
                  Project = "ccccDDnnnnnnnnnTTcTc", 
                  ProjectCoC = "ccccccccciTTcTc",
                  Services = "cccDiicciciTTcTc", 
                  User = "ccccccTTTc", 
                  YouthEducationStatus = "cccDiiiiTTcTc")


# ------------------------------------------------------------------------------
# ----------------------- Question List for APR --------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR

APR_files <- c("Q4a", "Q5a", "Q6a", "Q6b", "Q6c", "Q6d", "Q6e", "Q6f", "Q7a", "Q7b",
               "Q8a", "Q8b", "Q9a", "Q9b", "Q10a", "Q10b", "Q10c", "Q11",
               "Q12a", "Q12b", "Q13a1", "Q13a2", "Q13b1", "Q13b2", "Q13c1",
               "Q13c2", "Q14a", "Q14b", "Q15", "Q16", "Q17", "Q18", "Q19a1",
               "Q19a2", "Q19b", "Q20a", "Q20b", "Q21", "Q22a1", "Q22b", "Q22c",
               "Q22e", "Q23c", "Q25a", "Q25b", "Q25c", "Q25d", "Q25e", "Q25f",
               "Q25g", "Q25h", "Q25i", "Q26a", "Q26b", "Q26c", "Q26d", "Q26e",
               "Q26f", "Q26g", "Q26h", "Q26i", "Q27a", "Q27b", "Q27c", "Q27d",
               "Q27e", "Q27f", "Q27g", "Q27h", "Q27i", "Q27j", "Q27k", "Q27l")


# ------------------------------------------------------------------------------
# ---------------------- Question List for CAPER -------------------------------
# ------------------------------------------------------------------------------
# used in:
#   CAPER

CAPER_files <- c("Q4a", "Q5a", "Q6a", "Q6b", "Q6c", "Q6d", "Q6e", "Q6f", 
                 "Q7a", "Q7b", "Q8a", "Q8b", "Q9a", "Q9b", "Q10a", "Q10b", 
                 "Q10c", "Q10d", "Q11", "Q12a", "Q12b", "Q13a1", "Q13b1", 
                 "Q13c1", "Q14a", "Q14b", "Q15", "Q16", "Q17", "Q19b", "Q20a", 
                 "Q21", "Q22a2", "Q22c", "Q22d", "Q22e", "Q23c", "Q24", 
                 "Q25a", "Q26b")


# ------------------------------------------------------------------------------
# ---------------------- Question List for CE APR ------------------------------
# ------------------------------------------------------------------------------
# used in:
#   CE APR

CE_APR_files <- c("Q4a", "Q5a", "Q6a", "Q7a", "Q8a", "Q9a", "Q9b", "Q9c",
                  "Q9d", "Q10")


# ------------------------------------------------------------------------------
# ------------------ Empty File List for APR/CAPER -----------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER file generation

missing_files <- c()


# ------------------------------------------------------------------------------
# -------------------------- Valid CoC List ------------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER file generation

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


# ------------------------------------------------------------------------------
# -------------------------- Residence List ------------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

ResidenceUses <- read_excel("SupplementalTables.xlsx",
                            sheet = "ResidenceUses",
                            col_types = c("numeric", "text", "text", "text",
                                          "logical", "logical", "logical",
                                          "numeric", "numeric", "text"))


# ------------------------------------------------------------------------------
# -------------------------- Destination List ----------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

DestinationClassification <- read_excel("SupplementalTables.xlsx",
                                        sheet = "DestinationClassificationRead",
                                        col_types = c("numeric", "text",
                                                      "numeric", "text"))


# ------------------------------------------------------------------------------
# -------------------------- Income Type List ----------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

IncomeTypes <- read_excel("SupplementalTables.xlsx",
                          sheet = "IncomeTypes",
                          col_types = c("text", "text"))


# ------------------------------------------------------------------------------
# -------------------------- Benefit Type List ---------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

BenefitTypes <- read_excel("SupplementalTables.xlsx",
                           sheet = "BenefitTypes",
                           col_types = c("text", "text"))


# ------------------------------------------------------------------------------
# ------------------------- Insurance Type List --------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

InsuranceTypes <- read_excel("SupplementalTables.xlsx",
                             sheet = "InsuranceTypes",
                             col_types = c("text", "text"))


# ------------------------------------------------------------------------------
# --------------------------- Event Type List ----------------------------------
# ------------------------------------------------------------------------------
# used in:
#   CE APR

EventTypes <- read_excel("SupplementalTables.xlsx",
                             sheet = "EventTypes",
                             col_types = c("numeric", "text"))

# ------------------------------------------------------------------------------
# ----------------------------- Race List --------------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q12a

race_list <- c("White", "Black, African American, or African", 
               "Asian or Asian American", 
               "American Indian, Alaska Native, or Indigenous", 
               "Native Hawaiian or Pacific Islander", "Multiple Races", 
               "Client.Does.Not.Know.or.Refused", "Data.Not.Collected")


# ------------------------------------------------------------------------------
# -------------------------- Ethnicity List ------------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q12b

ethnicity_list <- c("Non-Hispanic/Non-Latin(a)(o)(x)", "Hispanic/Latin(a)(o)(x)",
                    "Client.Does.Not.Know.or.Refused", "Data.Not.Collected")


# ------------------------------------------------------------------------------
# ------------------------ Simple Age Group List -------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q7a

age_groups <- c("Adults", "Children", "Client.Does.Not.Know.or.Refused", "Data.Not.Collected")


# ------------------------------------------------------------------------------
# ---------------------- Detailed Age Group List -------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

detailed_age_group_list = c("Under 5", "5-12", "13-17", "18-24", "25-34",
                            "35-44", "45-54", "55-61", "62+", 
                            "Client.Does.Not.Know.or.Refused", "Data.Not.Collected")


# ------------------------------------------------------------------------------
# -------------------------- Disability List -----------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q13

disability_list <- c("Mental Health Disorder", "Alcohol Use Disorder", 
                     "Drug Use Disorder", "Both Alcohol and Drug Use Disorders", 
                     "Chronic Health Condition", "HIV/AIDS",
                     "Developmental Disability", "Physical Disability")


# ------------------------------------------------------------------------------
# ----------------------- Disability Count List --------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q13a2

disability_count_group_list <- c("None", "One Condition", "Two Conditions",
                                 "Three Or More Conditions", "Unknown",
                                 "Client.Does.Not.Know.or.Refused", "Data.Not.Collected")

# ------------------------------------------------------------------------------
# ---------------------------- HUD List 1.8 ------------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q14a

y_n_dkr_dnc_list <- c("Yes", "No", "Client.Does.Not.Know.or.Refused", "Data.Not.Collected")


# ------------------------------------------------------------------------------
# -------------------- Length of Participation List ----------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q22d

length_of_participation <- length_of_time_groups("CAPER", "enrollment_length_group")


# ------------------------------------------------------------------------------
# ------------------------ Possible Outcome List -------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q23c

possible_outcomes <- c("P", "N", "X")


# ------------------------------------------------------------------------------
# ----------------------- Prevention Outcome List ------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q24      

assessment_outcomes = c("Able to maintain the housing they had at project start--Without a subsidy",
                                 "Able to maintain the housing they had at project start--With the subsidy they had at project start",
                                 "Able to maintain the housing they had at project start--With an on-going subsidy acquired since project start",
                                 "Able to maintain the housing they had at project start--Only with financial assistance other than a subsidy",
                                 "Moved to new housing unit--With on-going subsidy",
                                 "Moved to new housing unit--Without an on-going subsidy",
                                 "Moved in with family/friends on a temporary basis",
                                 "Moved in with family/friends on a permanent basis",
                                 "Moved to a transitional or temporary housing facility or program",
                                 "Client became homeless – moving to a shelter or other place unfit for human habitation",
                                 "Client went to jail/prison",
                                 "Client died",
                                 "Client doesn’t know/Client refused",
                                 "Data not collected (no exit interview completed)")


# ------------------------------------------------------------------------------
# ----------------- Classification List: Veteran and Chronic -------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q25a

vet_chronic_categories <- c("Chronically Homeless Veteran",
                       "Non-Chronically Homeless Veteran",
                       "Not a Veteran",
                       "Client doesn’t know/Client refused",
                       "Data not collected")


# ------------------------------------------------------------------------------
# ----------------------- Classification List: Chronic -------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q26a

chronic_categories <- c("Chronically Homeless",
                        "Not Chronically Homeless",
                        "Client doesn’t know/Client refused",
                        "Data not collected")


# ------------------------------------------------------------------------------
# ---------------------- Income Amount Categories ------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q16

income_amount_categories <- c("No Income", "$1 - $150", "$151 - $250", 
                       "$251 - $500", "$501 - $1,000", 
                       "$1,001 - $1,500", "$1,501 - $2,000", 
                       "$2,001+", "Client.Does.Not.Know.or.Refused", "Data.Not.Collected")


# ------------------------------------------------------------------------------
# -------------------- Annual Income Amount Categories-------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q16

annual_income_amount_categories <- c(income_amount_categories, "No Annual Required", "Required Annual Missing")


# ------------------------------------------------------------------------------
# ------------------------ Income Type Categories ------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q18

income_type_categories <- c("Adults with Only Earned Income (i.e., Employment Income)", 
                       "Adults with Only Other Income", 
                       "Adults with Both Earned and Other Income",
                       "Adults with No Income", "Client.Does.Not.Know.or.Refused", 
                       "Missing Income Information")


# ------------------------------------------------------------------------------
# ------------------- Annual Income Type Categories ----------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q18

annual_income_type_categories <- c(income_type_categories, 
                                   "No Annual Required", "Required Annual Missing")


# ------------------------------------------------------------------------------
# ----------------------------- Gender List ------------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q10

gender_list <- c("Male", "Female", "No Single Gender", "Questioning", 
                 "Transgender", "Client.Does.Not.Know.or.Refused", "Data.Not.Collected")


# ------------------------------------------------------------------------------
# ------------------------ CSV Benefit Categories -----------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q20a

benefit_list <- c("SNAP", "WIC", "TANFChildCare", "TANFTransportation", 
                  "OtherTANF", "OtherBenefitsSource")


# ------------------------------------------------------------------------------
# ------------------------ CSV Income Categories -----------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q19b

income_rows_to_show <- c("Earned", "SSI", "SSDI", "VADisabilityService", 
                         "PrivateDisability", "WorkersComp", "TANF", 
                         "SocSecRetirement", "Pension", "ChildSupport",
                         "Other.Source")


# ------------------------------------------------------------------------------
# ------------------------ Outreach Contact Groups -----------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q9

contact_groups <- c("Once", "2-5 times", "6-9 times", "10+ times")


# ------------------------------------------------------------------------------
# ----------------------------- Household Type Groups --------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

household_group_list <- c(Total = 0, Without.Children = 0, 
                          With.Children.And.Adults = 0, With.Only.Children = 0, 
                          Unknown.Household.Type = 0)


# ------------------------------------------------------------------------------
# ------------------------- Split Household Type Groups ------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

split_household_group_list <- c(Total = 0, Without.Children = 0, 
                          Adults.in.HH.with.Children.and.Adults = 0,
                          Children.in.HH.with.Children.and.Adults = 0,
                          With.Only.Children = 0, Unknown.Household.Type = 0)


# ------------------------------------------------------------------------------
# --------------------------- Standard Detail Columns --------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER detail exports

standard_detail_columns <- c("ProjectName", "HouseholdID", "PersonalID",
                             "EnrollmentID", "RelationshipToHoH",
                             "EntryDate", "ExitDate", "household_type")

# ------------------------------------------------------------------------------
# ----------------------- Housing Program Detail Columns -----------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER detail exports

housing_program_detail_columns <- c("ProjectName", "HouseholdID", 
                                    "PersonalID", "EnrollmentID",
                                    "RelationshipToHoH", "EntryDate", 
                                    "HoH_HMID", "ExitDate", "household_type")


# ------------------------------------------------------------------------------
# ------------------------- Demographic Detail Columns -------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER detail exports

demographic_detail_columns <- c("age", "age_group", "VeteranStatus", 
                                "household_type", "DisablingCondition",
                                "chronic")



# ------------------------------------------------------------------------------
# ------------------------ LOT Homeless Detail Columns -------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER detail exports

lot_homeless_detail_columns <- c("LivingSituation", "LengthOfStay", 
                                "LOSUnderThreshold","PreviousStreetESSH", 
                                "DateToStreetESSH", "TimesHomelessPastThreeYears", 
                                "MonthsHomelessPastThreeYears")


# ------------------------------------------------------------------------------
# ------------------------------ CE Detail Columns -----------------------------
# ------------------------------------------------------------------------------
# used in:
#   CE APR detail exports

ce_detail_columns <- c(standard_detail_columns, "AssessmentDate")


# ------------------------------------------------------------------------------
# ------------------------------ Referral Results  -----------------------------
# ------------------------------------------------------------------------------
# used in:
#   CE APR 

referral_results <- c("Successful referral: client accepted",
                      "Unsuccessful referral: client rejected",
                      "Unsuccessful referral: provider rejected",
                      "No result recorded")

# ------------------------------------------------------------------------------
# ------------------------------- CSV Columns ----------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - 

CSV_columns <- read_excel("SupplementalTables.xlsx",
                            sheet = "CSV_Columns") %>%
  mutate(RDataType = case_when(
    str_detect(DataType, "S") ~ "c",
    DataType == "I" ~ "i",
    DataType %in% c("T", "D") ~ DataType,
    str_detect(DataType, "M") ~ "d"
  )) %>%
  select(File, ColumnName, RDataType) 

