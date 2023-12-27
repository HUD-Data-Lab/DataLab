# Copyright (C) 2023 ICF
#
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


function(input, output, session) {

  valid_file <- reactiveVal(0)
  # file_list <- reactiveValues()
  # file_list$file <- list()
  # rv <- reactiveValues()
  
  importFile <- function(csvFile, guess_max = 1000) {
    filename = str_glue("{csvFile}.csv")
    data <- read_csv(utils::unzip(zipfile = input$imported$datapath, files = filename)
                     ,col_types = get_col_types(csvFile)
                     ,na = ""
    )
    file.remove(filename)
    return(data)
  }
  
  observeEvent(input$timeOut, {
    reset("imported")
    session$reload()
  })
  
  csv_files <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
  # observeEvent(input$imported, {
    valid_file(0)
    csv_files <- list()
    source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/local_initially_valid_import.R", 
           local = TRUE)
    # extract file names from their uploaded zip
    if(tolower(tools::file_ext(input$imported$datapath)) != "zip") {
      show_invalid_popup(127)
      print("Unsuccessful upload - zip file not .zip")
    } else {
      
      zipContents <- utils::unzip(zipfile = input$imported$datapath, list=TRUE)
      
      zipFiles <- zipContents$Name %>% str_replace(".csv", "")
      
      # expected files
      expected_files <- unique(cols_and_data_types$File)
      
      # get missing files by comparing what we expect with what we got
      missing_files <- expected_files[!(expected_files %in% zipFiles)]
      
      ### Now check whether the file is hashed, has the expected structure, and contains
      # the expected csv files
      if(grepl("/", zipContents$Name[1])) {
        show_invalid_popup(122)
        # logMetadata("Unsuccessful upload - zip file was misstructured")
      } else if("Export" %in% missing_files) {
        show_invalid_popup(123)
        # logMetadata("Unsuccessful upload - not an HMIS CSV Export")
      } else if(!isFY2024Export()) {
        show_invalid_popup(124)
        # logMetadata("Unsuccessful upload - out of date HMIS CSV Export")
      } else if(length(missing_files)) {
        evachecks <- evachecks %>% filter(ID == 125) %>% 
          mutate(Guidance = HTML(str_glue(
            "Your zip file appears to be missing the following files:<br/><br/>
      
        {paste(missing_files, collapse = ', ')}<br/><br/>
        
        You either uploaded something other than an HMIS CSV export or your export 
        does not contain all the files outlined in the HMIS CSV Export specifications.
        If you are not sure how to run the hashed HMIS CSV Export in your HMIS,
        please contact your HMIS vendor."))
          )
        show_invalid_popup(125)
        # logMetadata("Unsuccessful upload - incomplete dataset")
      } else if(!is_hashed()) {
        show_invalid_popup(126)
        # logMetadata("Unsuccessful upload - not hashed")
      } 
    }
    
    if(initially_valid_import == 1) {
      
      withProgress({
        setProgress(message = "Processing...", value = .15)
        setProgress(detail = "Reading your files..", value = .2)})
      
      for (file in unique(cols_and_data_types$File)) {
        # print(file)
        #import the csv and save it as a data frame
        assign(file, importFile(file))
        # csv_files <- c(csv_files, list(get(file)))
      }
    }
    default_report_start_date <- Export %>% pull(ExportStartDate)
    default_report_end_date <- Export %>% pull(ExportEndDate)
    
    # rv$Enrollment <- Enrollment
    # valid_file(1)
    # choices <<- Organization$OrganizationName
    # list(Organization = Organization, Client = Client)
    # file_list$file$Organization <- Organization
    list(Client = Client, 
         CurrentLivingSituation = CurrentLivingSituation,
         Enrollment = Enrollment,
         Event = Event,
         Exit = Exit,
         Funder = Funder,
         Organization = Organization,
         Project = Project,
         Services = Services,
         IncomeBenefits = IncomeBenefits,
         Disabilities = Disabilities,
         HealthAndDV = HealthAndDV,
         default_report_start_date = default_report_start_date,
         default_report_end_date = default_report_end_date)
    # Organization
    # names(csv_files) <- unique(cols_and_data_types$File)
    # csv_files
  })
  
  output$org_selector <- renderUI({
    if (is.null(input$imported)) {
      return ()
    }
    orgs_with_PATH <- csv_files()$Organization %>%
      inner_join(csv_files()$Project %>%
                   filter(ProjectType %in% c(4, 6)),
                 by = "OrganizationID") %>%
      inner_join(csv_files()$Funder %>%
                   filter(Funder == 21),
                 by = "ProjectID")
    
    choices <- unique(orgs_with_PATH$OrganizationName)
    # choices <- ls()
    radioButtons("org_selector",
                 "What organization would you like to generate the report for?",
                 choices = choices,
                 selected = choices[1])
  })
  
  output$report_start_date <- renderUI({
    if (is.null(input$imported)) {
      return ()
    }
    default_report_start_date <- csv_files()$default_report_start_date
    dateInput("report_start_date",
                 "Report Start Date",
                 value = default_report_start_date)
  })
  
  output$report_end_date <- renderUI({
    if (is.null(input$imported)) {
      return ()
    }
    default_report_end_date <- csv_files()$default_report_end_date
    dateInput("report_end_date",
              "Report End Date",
              value = default_report_end_date)
  })
  
  relevant_projects <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    
    csv_files()$Funder %>%
      filter(Funder == 21 &
               StartDate <= input$report_end_date &
               (is.na(EndDate) |
                  EndDate >= input$report_start_date)) %>%
      inner_join(csv_files()$Project %>% 
                   filter(ProjectType %in% c(4, 6)),
                 by = "ProjectID") %>%
      inner_join(csv_files()$Organization %>% 
                   filter(OrganizationName == input$org_selector),
                 by = "OrganizationID") %>%
      .$ProjectID %>%
      unique()
  })
  
  PATH_activity_dates <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    
    PATH_activity_date_columns <- c("EnrollmentID", "active_date", "type")
    
    PATH_activity_dates <- csv_files()$CurrentLivingSituation %>%
      select(EnrollmentID, InformationDate) %>%
      mutate(type = "CLS") %>%
      `colnames<-`(PATH_activity_date_columns) %>%
      union(csv_files()$Enrollment %>%
              mutate(DateOfPATHStatus = if_else(ClientEnrolledInPATH == 1,
                                                DateOfPATHStatus, NA)) %>%
              select(EnrollmentID, DateOfEngagement, DateOfPATHStatus) %>%
              pivot_longer(contains("Date"), names_to = "type",
                           values_to = "active_date", values_drop_na = TRUE)) %>%
      union(csv_files()$Services %>%
              filter(RecordType == 141) %>%
              mutate(type = "Service") %>%
              select(EnrollmentID, DateProvided, type) %>%
              `colnames<-`(PATH_activity_date_columns)) %>%
      inner_join(csv_files()$Enrollment %>%
                   select(EnrollmentID, EntryDate),
                 by = "EnrollmentID") %>%
      left_join(csv_files()$Exit %>%
                  select(EnrollmentID, ExitDate),
                by = "EnrollmentID") %>%
      filter(active_date >= EntryDate &
               (active_date <= ExitDate |
                  is.na(ExitDate))) %>%
      select(-c(EntryDate, ExitDate))
    
    if (nrow(PATH_activity_dates) > 0) {
      PATH_activity_dates <- PATH_activity_dates %>%
        group_by(EnrollmentID, active_date) %>%      
        mutate(
          number_in_day = seq(n()),
          in_report_period = active_date >= input$report_start_date &
            active_date <= input$report_end_date) %>%
        ungroup()
    } else {
      PATH_activity_dates$number_in_day <- NA
      PATH_activity_dates$in_report_period <- NA
    }
    
    PATH_activity_dates
  })
  
  general_detail <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    data_prep <- csv_files()$Enrollment %>%
      select(all_of(colnames(csv_files()$Enrollment)[1:21])) %>%
      left_join(csv_files()$Exit %>%
                  select(EnrollmentID, ExitDate, Destination),
                by = "EnrollmentID") %>%
      left_join(csv_files()$Project %>%
                  select(ProjectID, ProjectType),
                by = "ProjectID") %>%
      filter(
        ProjectID %in% relevant_projects() &
          ((ExitDate >= input$report_start_date &
              ExitDate <= input$report_end_date) |
             (EntryDate <= input$report_end_date &
                (is.na(ExitDate) |
                   ExitDate > input$report_end_date) &
                EnrollmentID %in% PATH_activity_dates()$EnrollmentID[PATH_activity_dates()$in_report_period])))
    
    data_prep %>%
      left_join(csv_files()$Client %>%
                  select(PersonalID, DOB),
                by = "PersonalID") %>%
      mutate(
        ExitDateAdj = if_else(is.na(ExitDate), input$report_end_date,
                              ExitDate),
        new_and_active = EntryDate >= input$report_start_date &
          EntryDate <= input$report_end_date &
          PersonalID %nin% data_prep$PersonalID[data_prep$EntryDate < input$report_start_date],
        active_and_enrolled = !is.na(ClientEnrolledInPATH) &
          !is.na(DateOfPATHStatus) &
          ClientEnrolledInPATH == 1 &
          DateOfPATHStatus <= input$report_end_date &
          DateOfPATHStatus >= EntryDate &
          DateOfPATHStatus <= ExitDateAdj,
        status_during_period = DateOfPATHStatus >= input$report_start_date &
          DateOfPATHStatus <= input$report_end_date,
        enrolled_during_period = status_during_period &
          ClientEnrolledInPATH == 1,
        leaver = !is.na(ExitDate) &
          ExitDate >= input$report_start_date &
          ExitDate <= input$report_end_date,
        stayer = is.na(ExitDate),
        date_for_age = (if_else(
          EntryDate <= input$report_start_date,
          input$report_start_date, # Return Report Start date if true
          EntryDate)),
        age = trunc((DOB %--% date_for_age) / years(1))) %>%
      select(-date_for_age) %>%
      group_by(PersonalID) %>%
      arrange(desc(EntryDate)) %>%
      slice(1L) %>%
      ungroup()
  })
  
  Q8_16 <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    Q12a_detail <- PATH_activity_dates() %>%
      inner_join(general_detail() %>%
                   filter(enrolled_during_period),
                 join_by(EnrollmentID,
                         active_date <= DateOfPATHStatus))
    
    Q12b_detail <- PATH_activity_dates() %>%
      filter(EnrollmentID %in% general_detail()$EnrollmentID[general_detail()$enrolled_during_period] &
               in_report_period)
    
    Q16_detail <- csv_files()$Services %>%
      filter(EnrollmentID %in% general_detail()$EnrollmentID[general_detail()$active_and_enrolled] &
               DateProvided >= input$report_start_date &
               DateProvided <= input$report_end_date &
               ((RecordType == 141 & #  141 is PATH service |
                   TypeProvided == 4) |
                  (RecordType == 161 & #  161 is PATH referral
                     TypeProvided == 1 &
                     ReferralOutcome == 1)))
    
    Q8_16 <- as.data.frame(matrix(c(
      "8. Number of persons contacted by PATH-funded staff this reporting period",
      n_distinct(
        general_detail()$PersonalID,
        na.rm = TRUE),
      "9. Number of new persons contacted this reporting period in a PATH Street Outreach project",
      n_distinct(
        general_detail()$PersonalID[general_detail()$new_and_active &
                                    general_detail()$ProjectType == 4],
        na.rm = TRUE),
      "10. Number of new persons contacted this reporting period in a PATH Services Only project",
      n_distinct(
        general_detail()$PersonalID[general_detail()$new_and_active &
                                    general_detail()$ProjectType == 6],
        na.rm = TRUE),
      "11. Total number of new persons contacted this reporting period (#9 + #10 = total new clients contacted)",
      # question about using the most recent enrollment? what's a universe?
      n_distinct(
        general_detail()$PersonalID[general_detail()$new_and_active],
        na.rm = TRUE),
      "12a. Instances of contact this reporting period prior to date of enrollment",
      # reporting specifications don't say that the date must be within the
      # report period, even though the specifications for the next row do
      nrow(Q12a_detail %>%
             filter(type == "CLS" |
                      number_in_day == 1)),
      "12b. Total instances of contact during the reporting period",
      nrow(Q12b_detail %>%
             filter(type == "CLS" |
                      number_in_day == 1)),
      "13a. Number of new persons contacted this reporting period who could not be enrolled because of ineligibility for PATH",
      nrow(general_detail() %>%
             filter(new_and_active &
                      status_during_period &
                      ClientEnrolledInPATH == 0 &
                      ReasonNotEnrolled == 1) %>%
             distinct(PersonalID)),
      "13b. Number of new persons contacted this reporting period who could not be enrolled because provider was unable to locate the client",
      nrow(general_detail() %>%
             filter(new_and_active &
                      status_during_period &
                      ClientEnrolledInPATH == 0 &
                      ReasonNotEnrolled == 3) %>%
             distinct(PersonalID)),
      "14. Number of new persons contacted this reporting period who became enrolled in PATH",
      n_distinct(
        general_detail()$PersonalID[general_detail()$new_and_active &
                                    general_detail()$enrolled_during_period],
        na.rm = TRUE),
      "15. Number with active, enrolled PATH status at any point during the reporting period",
      n_distinct(
        general_detail()$PersonalID[general_detail()$active_and_enrolled],
        na.rm = TRUE),
      "16. Number of active, enrolled PATH clients receiving community mental health services through any funding source at any point during the reporting period",
      n_distinct(
        Q16_detail$PersonalID,
        na.rm = TRUE
      )
    ), ncol = 2, byrow = TRUE)) %>%
      setNames(c("Persons served during this reporting period", "Count")) %>%
      set_hud_format(., ignore_row_names = TRUE) %>%
      ifnull(., 0)
    
    Q8_16[is.na(Q8_16)] <- ""
    Q8_16
  })
  
  Q17 <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    service_list <- as.data.frame(matrix(c(
      "17a. Re-engagement", 1,
      "17b. Screening", 2,
      "17c. Clinical assessment", 14,
      "17d. Habilitation/rehabilitation", 3,
      "17e. Community Mental Health", 4,
      "17f. Substance use treatment", 5,
      "17g. Case management", 6,
      "17h. Residential suport services", 7,
      "17i. Housing minor renovation", 8,
      "17j. Housing moving assistance", 9,
      "17k. Housing eligibility determination", 10,
      "17l. Security deposits", 11,
      "17m. One-time rent for eviction prevention", 12
    ), ncol = 2, byrow = TRUE)) %>%
      setNames(c("Type.of.Service", "TypeProvided")) %>%
      mutate(TypeProvided = as.numeric(TypeProvided))
    
    Q17_detail <- service_list %>%
      left_join(csv_files()$Services %>%
                  select(EnrollmentID, PersonalID, DateProvided, RecordType,
                         TypeProvided) %>%
                  filter(EnrollmentID %in% general_detail()$EnrollmentID[general_detail()$active_and_enrolled] &
                           DateProvided >= input$report_start_date &
                           DateProvided <= input$report_end_date &
                           RecordType == 141),
                by = "TypeProvided")
    
    Q17 <- Q17_detail %>%
      group_by(Type.of.Service) %>%
      summarise(Number.of.people.receiving.service = n_distinct(PersonalID,
                                                                na.rm = TRUE)) %>%
      arrange(Type.of.Service) %>%
      set_hud_format(., ignore_row_names = TRUE) %>%
      ifnull(., 0)
    
    Q17[is.na(Q17)] <- ""
    Q17
  })
  
  Q18 <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    referral_list <- as.data.frame(matrix(c(
      "Community Mental Health", 1,
      "Substance Use Treatment", 2,
      "Primary Health/ Dental Care", 3,
      "Job Training", 4,
      "Educational Services", 5,
      "Housing Services", 6,
      "Temporary Housing", 11,
      "Permanent Housing", 7,
      "Income Assistance", 8,
      "Employment Assistance", 9,
      "Medical Insurance", 10
    ), ncol = 2, byrow = TRUE)) %>%
      setNames(c("Type.of.Referral", "TypeProvided")) %>%
      mutate(TypeProvided = as.numeric(TypeProvided),
             Type.of.Referral = factor(Type.of.Referral,
                                       levels=Type.of.Referral,
                                       ordered=TRUE))
    
    Q18_detail <- referral_list %>%
      left_join(csv_files()$Services %>%
                  select(EnrollmentID, PersonalID, DateProvided, RecordType,
                         TypeProvided, ReferralOutcome) %>%
                  filter(EnrollmentID %in% general_detail()$EnrollmentID[general_detail()$active_and_enrolled] &
                           DateProvided >= input$report_start_date &
                           DateProvided <= input$report_end_date &
                           RecordType == 161),
                by = "TypeProvided")
    
    Q18 <- Q18_detail %>%
      group_by(Type.of.Referral) %>%
      summarise(Number.receiving.each.referral = n_distinct(PersonalID,
                                                            na.rm = TRUE),
                Number.who.attained.the.service.from.the.referral = n_distinct(PersonalID[ReferralOutcome == 1],
                                                                               na.rm = TRUE)) %>%
      arrange(Type.of.Referral) %>%
      set_hud_format(., ignore_row_names = TRUE) %>%
      ifnull(., 0)
    
    Q18[is.na(Q18)] <- ""
    Q18
  })
  
  Q19_24 <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    
    # names_for_1.8 <- c("Yes", "No", "Client doesn't know",
    #                    "Client prefers not to answer", "Data not collected")
    # values_for_1.8 <- c(1, 0, 8, 9, 99)
    
    # convert_list_1.8 <- function(column_name) {
    #   # replace_na(column_name, 99)
    #   names_for_1.8[match(column_name %>%
    #                         replace_na(99), values_for_1.8)]
    # }
    
    columns_for_calculation <- c(IncomeTypes$IncomeGroup,
                                 InsuranceTypes$InsuranceGroup)
    columns_for_Q19_24 <- c("IncomeFromAnySource", "ssi_or_ssdi",
                            "BenefitsFromAnySource", "InsuranceFromAnySource",
                            "medicaid_or_medicare", "other_health_insurance")
    
    Q19_detail <- csv_files()$IncomeBenefits %>%
      select(-PersonalID) %>%
      inner_join(general_detail() %>%
                   filter(active_and_enrolled),
                 join_by(EnrollmentID,
                         InformationDate >= EntryDate,
                         InformationDate <= ExitDateAdj)) %>%
      select(c(PersonalID, EnrollmentID, InformationDate, EntryDate, ExitDate,
               DataCollectionStage, IncomeFromAnySource, BenefitsFromAnySource,
               InsuranceFromAnySource, all_of(columns_for_calculation), leaver, stayer))
    
    Q19_processing <- Q19_detail %>%
      mutate(at_project_start = InformationDate == EntryDate &
               DataCollectionStage == 1,
             at_exit = leaver &
               InformationDate == ExitDate &
               DataCollectionStage == 3,
             at_report_end = stayer &
               DataCollectionStage %in% c(1, 2),
             across(all_of(columns_for_calculation),
                    ~ if_else(. == 1 & !is.na(.), 1, 0)),
             ssi_or_ssdi = pmax(SSI, SSDI),
             medicaid_or_medicare = pmax(Medicaid, Medicare),
             other_health_insurance = pmax(SCHIP, VHAServices, EmployerProvided,
                                           COBRA, PrivatePay, StateHealthIns,
                                           IndianHealthServices, OtherInsurance),
             across(all_of(columns_for_Q19_24),
                    ~ convert_list_1.8(.))) %>%
      group_by(EnrollmentID, at_project_start, at_exit, at_report_end) %>%
      arrange(desc(InformationDate)) %>%
      slice(1L) %>%
      ungroup()
    
    add_19_24_result_columns <- function(dataframe, target) {
      dataframe %>%
        mutate(Outcomes = {{target}}) %>%
        group_by(Outcomes) %>%
        summarize(at_project_start = n_distinct(PersonalID[at_project_start],
                                                na.rm = TRUE),
                  at_exit = n_distinct(PersonalID[at_exit],
                                       na.rm = TRUE),
                  at_report_end = n_distinct(PersonalID[at_report_end],
                                             na.rm = TRUE))
    }
    
    Q19_24 <- as.data.frame(c("19. Income from Any Source",
                              names_for_1.8))  %>%
      `colnames<-`(c("Outcomes")) %>%
      full_join(Q19_processing %>%
                  add_19_24_result_columns(., IncomeFromAnySource),
                by = "Outcomes") %>%
      adorn_totals() %>%
      rbind(as.data.frame(c("20. Supplemental Security Income (SSI)/ Social",
                            names_for_1.8[1:2]))  %>%
              `colnames<-`(c("Outcomes")) %>%
              full_join(Q19_processing %>%
                          add_19_24_result_columns(., ssi_or_ssdi),
                        by = "Outcomes")) %>%
      rbind(as.data.frame(c("21. Non-Cash Benefits from Any Source",
                            names_for_1.8))  %>%
              `colnames<-`(c("Outcomes")) %>%
              full_join(Q19_processing %>%
                          add_19_24_result_columns(., BenefitsFromAnySource),
                        by = "Outcomes") %>%
              adorn_totals()) %>%
      rbind(as.data.frame(c("22. Covered by Health Insurance",
                            names_for_1.8))  %>%
              `colnames<-`(c("Outcomes")) %>%
              full_join(Q19_processing %>%
                          add_19_24_result_columns(., InsuranceFromAnySource),
                        by = "Outcomes") %>%
              adorn_totals()) %>%
      rbind(as.data.frame(c("23. MEDICAID/MEDICARE",
                            names_for_1.8[1:2]))  %>%
              `colnames<-`(c("Outcomes")) %>%
              full_join(Q19_processing %>%
                          add_19_24_result_columns(., medicaid_or_medicare),
                        by = "Outcomes")) %>%
      rbind(as.data.frame(c("24. All other health insurance",
                            names_for_1.8[1:2]))  %>%
              `colnames<-`(c("Outcomes")) %>%
              full_join(Q19_processing %>%
                          add_19_24_result_columns(., other_health_insurance),
                        by = "Outcomes"))%>%
      `colnames<-`(c("Outcomes",	"At PATH Project Start",
                     "At PATH Project Exit (for clients who were exited from PATH in the reporting period - leavers)",
                     "At Report End Date (for clients who were still active in PATH as of Report End Date - stayers")) %>%
      set_hud_format(., ignore_row_names = TRUE) %>%
      ifnull(., 0)
    
    Q19_24[is.na(Q19_24)] <- ""
    Q19_24
  })
  
  Q25 <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    Q25_detail <- general_detail() %>%
      filter(active_and_enrolled) %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDate, Destination,
             stayer, leaver)
    
    destination_counts <- Q25_detail %>%
      group_by(Destination) %>%
      summarise(count = n_distinct(PersonalID, na.rm = TRUE)) %>%
      ungroup()
    
    for(residence_type in c("Homeless", "Institutional","Temporary", "Permanent",
                            "Other")) {
      
      residences_to_include <- ResidenceUses %>%
        filter(APR_LocationGroup == residence_type &
                 !is.na(LocationDescription) &
                 Destination) %>%
        mutate(
          LocationDescription = case_when(
            Location == 9 ~ "Client prefers not to answer",
            TRUE ~ LocationDescription),
          APR_LocationOrder = case_when(
            Location == 99 ~ 39,
            Location == 9 ~ 38,
            TRUE ~ APR_LocationOrder))
      
      group_of_residences <- residences_to_include %>%
        left_join(destination_counts,
                  by = c("Location" = "Destination")) %>%
        arrange(APR_LocationOrder) %>%
        select(LocationDescription, count) %>%
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
    
    Q25 <- destination_group %>%
      rbind(c("PATH-enrolled clients still active as of report end date (stayers)",
              n_distinct(Q25_detail$PersonalID[Q25_detail$stayer],
                         na.rm = TRUE))) %>%
      rbind(c("Total",
              n_distinct(Q25_detail$PersonalID,
                         na.rm = TRUE))) %>%
      set_hud_format(., ignore_row_names = TRUE) %>%
      ifnull(., 0)

    Q25[is.na(Q25)] <- ""
    Q25
  })
  
  Q26 <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    Q26a_e_detail <- general_detail() %>%
      filter(active_and_enrolled) %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDate, age, Destination,
             stayer, leaver) %>%
      left_join(csv_files()$Client %>%
                  select(PersonalID, DOB, DOBDataQuality, VeteranStatus,
                         all_of(names(gender_columns)), GenderNone,
                         all_of(unname(race_columns)), RaceNone),
                by = "PersonalID")
    
    # Q26a
    gender_table <- as.data.frame(c(unname(gender_columns),
                                    names_for_1.8[3:5]))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(
        Q26a_e_detail %>%
          select(PersonalID, all_of(names(gender_columns)), GenderNone) %>%
          ##  this bit sets GenderNone to 99 if there are no responses at all
          mutate(GenderNone = if_else(
            rowSums(across(c(all_of(names(gender_columns)), GenderNone)),
                    na.rm = TRUE) == 0,
            99, GenderNone)) %>%
          gather(ind, value, -PersonalID) %>%
          filter(value > 0) %>%
          full_join(stack(gender_columns),
                    by = "ind") %>%
          mutate(col_b = case_when(
            value == 1 |
              is.na(value) ~ values,
            TRUE ~ convert_list_1.8(value))) %>%
          group_by(col_b) %>%
          summarise(individuals = n_distinct(PersonalID,
                                             na.rm = TRUE)),
        by = "col_b") %>%
      rbind(c("Total",
              n_distinct(Q26a_e_detail$PersonalID,
                         na.rm = TRUE))) %>%
      mutate(col_a = case_when(
        col_b == unname(gender_columns)[1] ~ "26a. Gender"), .before = col_b)
    
    # Q26b
    age_groups <- c("17 and under", "18 – 23", "24 – 30", "31 – 40",
                    "41 – 50", "51 – 61", "62 and over")
    
    age_table <- as.data.frame(c(age_groups, names_for_1.8[3:5])) %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26a_e_detail %>%
                  mutate(
                    col_b = case_when(
                      is.na(DOB) ~ convert_list_1.8(DOBDataQuality),
                      age <= 17 ~ age_groups[1],
                      age <= 23 ~ age_groups[2],
                      age <= 30 ~ age_groups[3],
                      age <= 40 ~ age_groups[4],
                      age <= 50 ~ age_groups[5],
                      age <= 61 ~ age_groups[6],
                      TRUE ~ age_groups[7])) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>%
      mutate(col_a = case_when(
        col_b == age_groups[1] ~ "26b. Age"), .before = col_b)
    
    # Q26c
    race_table <- as.data.frame(c(names(race_columns),
                                  names_for_1.8[3:5]))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(
        Q26a_e_detail %>%
          select(PersonalID, all_of(unname(race_columns)), RaceNone) %>%
          ##  this bit sets RaceNone to 99 if there are no responses at all
          mutate(RaceNone = if_else(
            rowSums(across(c(all_of(unname(race_columns)), RaceNone)),
                    na.rm = TRUE) == 0,
            99, RaceNone)) %>%
          gather(values, value, -PersonalID) %>%
          filter(value > 0) %>%
          full_join(stack(race_columns),
                    by = "values") %>%
          mutate(col_b = case_when(
            value == 1 |
              is.na(value) ~ ind,
            TRUE ~ convert_list_1.8(value))) %>%
          group_by(col_b) %>%
          summarise(individuals = n_distinct(PersonalID,
                                             na.rm = TRUE)),
        by = "col_b") %>%
      rbind(c("Total",
              n_distinct(Q26a_e_detail$PersonalID,
                         na.rm = TRUE))) %>%
      mutate(col_a = case_when(
        col_b == names(race_columns)[1] ~ "26c. Race and Ethnicity"),
        .before = col_b)
    
    # Q26d was ethnicity, removed this year
    
    # Q26e
    veteran_table <- as.data.frame(c("Veteran", "Non-veteran",
                                     names_for_1.8[3:5]))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(
        Q26a_e_detail %>%
          filter(!is.na(age) &
                   age >= 18) %>%
          mutate(
            col_b = case_when(
              is.na(VeteranStatus) |
                VeteranStatus > 1 ~ convert_list_1.8(VeteranStatus),
              VeteranStatus == 1 ~ "Veteran",
              TRUE ~ "Non-veteran")) %>%
          group_by(col_b) %>%
          summarise(individuals = n_distinct(PersonalID,
                                             na.rm = TRUE)),
        by = "col_b") %>%
      adorn_totals() %>%
      mutate(col_a = case_when(
        col_b == "Veteran" ~ "26e. Veteran Status (adults only)"),
        .before = col_b)
    
    # Q26f
    Q26f_detail <- general_detail() %>%
      filter(active_and_enrolled) %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDateAdj) %>%
      left_join(csv_files()$Disabilities %>%
                  select(-PersonalID) %>%
                  filter(InformationDate <= input$report_end_date &
                           DisabilityType == 10),
                join_by(EnrollmentID,
                        EntryDate <= InformationDate,
                        ExitDateAdj >= InformationDate)) %>%
      group_by(EnrollmentID) %>%
      arrange(desc(InformationDate)) %>%
      slice(1L) %>%
      ungroup()
    
    cooccurring_disorder_table <- as.data.frame(c("Co-occurring substance use disorder",
                                                  "No co-occurring substance use disorder",
                                                  "Unknown"))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26f_detail %>%
                  mutate(col_b = case_when(
                    is.na(DisabilityResponse) |
                      DisabilityResponse > 3 ~ "Unknown",
                    DisabilityResponse == 0 ~ "No co-occurring substance use disorder",
                    TRUE ~ "Co-occurring substance use disorder")) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>%
      mutate(col_a = case_when(
        col_b == "Co-occurring substance use disorder" ~ "26f. Co-occurring disorder"),
        .before = col_b)
    
    # Q26g
    Q26g_detail <- general_detail() %>%
      filter(active_and_enrolled) %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDateAdj) %>%
      left_join(csv_files()$IncomeBenefits %>%
                  select(EnrollmentID, InformationDate, ConnectionWithSOAR) %>%
                  filter(InformationDate <= input$report_end_date),
                join_by(EnrollmentID,
                        EntryDate <= InformationDate,
                        ExitDateAdj >= InformationDate)) %>%
      group_by(EnrollmentID) %>%
      arrange(desc(InformationDate)) %>%
      slice(1L) %>%
      ungroup()
    
    soar_table <- as.data.frame(names_for_1.8)  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26g_detail %>%
                  mutate(
                    col_b = convert_list_1.8(ConnectionWithSOAR)) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>%
      mutate(col_a = case_when(
        col_b == "Yes" ~ "26g. Connection with SOAR"),
        .before = col_b)
    
    # Q26h
    disability_table <<- csv_files()$Disabilities %>% 
      filter(DisabilityResponse %in% c(0, 1) |
               (DisabilityType == 10 &
                  DisabilityResponse %in% c(2, 3))) %>%
      mutate(disability_name = case_when(DisabilityType == 5 ~ "Physical Disability",
                                         DisabilityType == 6 ~ "Developmental Disability",
                                         DisabilityType == 7 ~ "Chronic Health Condition",
                                         DisabilityType == 8 ~ "HIV/AIDS",
                                         DisabilityType == 9 ~ "Mental Health Disorder",
                                         DisabilityResponse == 1 ~ "Alcohol Use Disorder",
                                         DisabilityResponse == 2 ~ "Drug Use Disorder",
                                         DisabilityResponse == 3 ~ "Both Alcohol and Drug Use Disorders"),
             disabilities = case_when(
               DisabilityResponse %in% 1:3 ~
                 if_else(disability_name == "Both Alcohol and Drug Use Disorders", 2, 1)),
             indefinite_and_impairs = ((DisabilityResponse == 1 &
                                          DisabilityType %in% c(6, 8)) |
                                         (DisabilityResponse %in% c(2, 3) &
                                            DisabilityType %in% c(5, 7, 9, 10) &
                                            IndefiniteAndImpairs == 1))) %>%
      select(EnrollmentID, DataCollectionStage, InformationDate, disability_name, 
             DisabilityResponse, indefinite_and_impairs, disabilities)
    
    Q26h_j_detail <- general_detail() %>%
      filter(active_and_enrolled) %>%
      select(HouseholdID, EnrollmentID, PersonalID, EntryDate, ExitDate,
             DisablingCondition, LivingSituation, LengthOfStay,
             LOSUnderThreshold, PreviousStreetESSH, DateToStreetESSH,
             TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears) %>%
      add_chronicity_data(., Enrollment = csv_files()$Enrollment,
                          Project = csv_files()$Project,
                          Client = csv_files()$Client,
                          report_start_date = input$report_start_date)
    
    living_situation_counts <- Q26h_j_detail %>%
      mutate(LivingSituation = if_else(
        is.na(LivingSituation), 99, LivingSituation)) %>%
      group_by(LivingSituation) %>%
      summarise(individuals = n_distinct(PersonalID, na.rm = TRUE)) %>%
      ungroup()
    
    for(residence_type in c("Homeless", "Institutional", "Temporary",
                            "Permanent", "Other")) {
      residences_to_include <- ResidenceUses %>%
        filter(APR_LocationGroup == residence_type &
                 !is.na(LocationDescription) &
                 PriorLivingSituation) %>%
        # arrange(APR_LocationOrder) %>%
        mutate(
          LocationDescription = case_when(
            Location == 9 ~ "Client prefers not to answer",
            TRUE ~ LocationDescription))
      
      group_of_residences <- residences_to_include %>%
        left_join(living_situation_counts,
                  by = c("Location" = "LivingSituation")) %>%
        mutate(APR_LocationOrder = case_when(
          Location == 99 ~ 39,
          Location == 9 ~ 38,
          Location == 205 ~ 12,
          Location == 204 ~ 13,
          TRUE ~ APR_LocationOrder)) %>%
        arrange(APR_LocationOrder) %>%
        select(LocationDescription, individuals) %>%
        ifnull(., 0) %>%
        mutate(LocationDescription = case_when(
          LocationDescription == "Total" ~ paste(str_to_title(residence_type), "Subtotal"),
          TRUE ~ LocationDescription))
      
      group_title_row <- group_of_residences[0,]
      group_title_row[1,1] <- paste(residence_type, "Situations")
      
      group_of_residences <- rbind(group_title_row, group_of_residences)
      
      if (residence_type != "Homeless") {
        living_situation_table <- living_situation_table %>%
          union(group_of_residences)
      } else {
        living_situation_table <- group_of_residences
      }
    }
    
    living_situation_table <- living_situation_table %>%
      adorn_totals() %>%
      rename(col_b = LocationDescription) %>%
      mutate(col_a = case_when(
        col_b == "Homeless Situations" ~ "26h. Prior Living Situation"),
        .before = col_b)
    
    # Q26i
    lengths_of_stay <- c("One night or less", "Two to six nights",
                         "One week or more, but less than one month",
                         "One month or more, but less than 90 days",
                         "90 days or more, but less than one year",
                         "One year or longer")
    
    length_of_stay_table <- as.data.frame(c(lengths_of_stay,
                                            names_for_1.8[3:5]))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(
        Q26h_j_detail %>%
          filter(LivingSituation %in% 100:199) %>%
          mutate(
            col_b = case_when(
              is.na(LengthOfStay) |
                LengthOfStay %in% c(8, 9, 99) ~ convert_list_1.8(LengthOfStay),
              LengthOfStay == 10 ~ lengths_of_stay[1],
              LengthOfStay == 11 ~ lengths_of_stay[2],
              LengthOfStay == 2 ~ lengths_of_stay[3],
              LengthOfStay == 3 ~ lengths_of_stay[4],
              LengthOfStay == 4 ~ lengths_of_stay[5],
              TRUE ~ lengths_of_stay[6])) %>%
          group_by(col_b) %>%
          summarise(individuals = n_distinct(PersonalID,
                                             na.rm = TRUE)),
        by = "col_b") %>%
      adorn_totals() %>%
      mutate(col_a = case_when(
        col_b == "One night or less" ~ "26i. Length of stay in prior living situation (emergency shelter or place not meant for human habitation only)"),
        .before = col_b)
    
    # Q26j
    
    chronicity_table <- as.data.frame(c("Yes", "No", "Unknown"))  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26h_j_detail %>%
                  mutate(col_b = case_when(
                    chronic == "Y" ~ "Yes",
                    chronic == "N" ~ "No",
                    TRUE ~ "Unknown")) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>%
      mutate(col_a = case_when(
        col_b == "Yes" ~ "26j. Chronically homeless (at project start)"),
        .before = col_b)
    
    # Q26k
    Q26k_detail <- general_detail() %>%
      filter(!is.na(age) &
               age >= 18 &
               active_and_enrolled) %>%
      select(EnrollmentID, PersonalID, EntryDate, ExitDateAdj) %>%
      left_join(csv_files()$HealthAndDV %>%
                  select(EnrollmentID, InformationDate, DomesticViolenceSurvivor) %>%
                  filter(InformationDate <= input$report_end_date),
                join_by(EnrollmentID,
                        EntryDate <= InformationDate,
                        ExitDateAdj >= InformationDate)) %>%
      group_by(EnrollmentID) %>%
      arrange(desc(InformationDate)) %>%
      slice(1L) %>%
      ungroup()
    
    dv_table <- as.data.frame(names_for_1.8)  %>%
      `colnames<-`(c("col_b")) %>%
      full_join(Q26k_detail %>%
                  mutate(
                    col_b = convert_list_1.8(DomesticViolenceSurvivor)) %>%
                  group_by(col_b) %>%
                  summarise(individuals = n_distinct(PersonalID,
                                                     na.rm = TRUE)),
                by = "col_b") %>%
      adorn_totals() %>%
      mutate(col_a = case_when(
        col_b == "Yes" ~ "26k. Survivor of Domestic Violence (adults only)"),
        .before = col_b)
    
    # combine all
    Q26 <- gender_table %>%
      rbind(age_table) %>%
      rbind(race_table) %>%
      rbind(veteran_table) %>%
      rbind(cooccurring_disorder_table) %>%
      rbind(soar_table) %>%
      rbind(living_situation_table) %>%
      rbind(length_of_stay_table) %>%
      rbind(chronicity_table) %>%
      rbind(dv_table) %>%
      mutate(
        individuals = ifnull(individuals, 0))  %>%
      `colnames<-`(c(" ", "  ",
                     "Of those with an active, enrolled PATH status during this reporting period, how many individuals are in each of the following categories?")) %>%
      set_hud_format(., ignore_row_names = TRUE) %>%
      ifnull(., 0)
    
    Q26[is.na(Q26)] <- ""
    Q26
  })
  

  output$downloadall <- downloadHandler(
    filename = paste0("PATH_Report ", format(today(), "%m.%d.%y"), ".zip"),
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())


      PATH_files <- paste0(PATH_questions, ".csv")
      
      write.csv(Q8_16(), "Q8_16.csv", row.names=FALSE)
      write.csv(Q17(), "Q17.csv", row.names=FALSE)
      write.csv(Q18(), "Q18.csv", row.names=FALSE)
      write.csv(Q19_24(), "Q19_24.csv", row.names=FALSE)
      write.csv(Q25(), "Q25.csv", row.names=FALSE)
      write.csv(Q26(), "Q26.csv", row.names=FALSE)

      zip(zipfile = fname, files = PATH_files)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    }
    ,
    contentType = "application/zip"
  )
  
  output$preview_selector <- renderUI({
    if (is.null(input$imported)) {
      return ()
    }
    
    # choices <- unique(orgs_with_PATH$OrganizationName)
    # choices <- ls()
    radioButtons("preview_selector",
                 "What table would you like to preview?",
                 choices = c(PATH_questions, "Included Projects"),
                 selected = PATH_questions[1])
  })
  
  included_projects <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    csv_files()$Organization %>% 
      filter(OrganizationName == input$org_selector) %>%
      inner_join(csv_files()$Project %>%
                   mutate(`SO or SSO Project` = ProjectType %in% c(4, 6),
                          `Included Project` = ProjectID %in% general_detail()$ProjectID),
                 by = "OrganizationID") %>%
      inner_join(csv_files()$Funder %>%
                   group_by(ProjectID) %>%
                   summarise(`Has PATH Fund Source` = as.logical(max(Funder == 21))) %>%
                   ungroup(),
                 by = "ProjectID") %>%
      left_join(general_detail() %>%
                  group_by(ProjectID) %>%
                  summarise(`People With Activity` = n_distinct(PersonalID,
                                                         na.rm = TRUE)) %>%
                  ungroup(),
                by = "ProjectID") %>%
      select(ProjectName, `SO or SSO Project`, `Has PATH Fund Source`,
             `People With Activity`, `Included Project`)
  })
  
  
  output$debug_table <- DT::renderDataTable({
    if (is.null(input$imported)) {
      return ()
    }
    req(input$preview_selector)
    if (input$preview_selector == "Q8_16") {df <- Q8_16()}
    else if (input$preview_selector == "Q17") {df <- Q17()}
    else if (input$preview_selector == "Q18") {df <- Q18()}
    else if (input$preview_selector == "Q19_24") {df <- Q19_24()}
    else if (input$preview_selector == "Q25") {df <- Q25()}
    else if (input$preview_selector == "Q26") {df <- Q26()}
    else if (input$preview_selector == "Included Projects") {df <- included_projects()}
    
    datatable(
      # csv_files()$Organization,
      # run_questions(),
      df,
      rownames = FALSE,
      options = list(dom = 't',
                     "pageLength" = -1)
    )
  })
  
  }
