######################
# PURPOSE: This program will check whether the uploaded file is hashed and at
# least looks like a valid HMIS upload (i.e. contains the expected set of csv 
# files)
# if the file is a hashed HMIS, we will proceed with processing
# if it is not, we will show them a pop-up indicating the problem
######################

initially_valid_import <<- TRUE

show_invalid_popup <- function(issueID) {
  initially_valid_df <- evachecks %>% filter(ID == issueID)
  
  initially_valid_import <<- FALSE
  
  showModal(
    modalDialog(
      initially_valid_df$Guidance,
      title = initially_valid_df$Issue,
      easyClose = TRUE
    )
  )
  # reset("imported")
}

# function to check if the file is hashed
is_hashed <- function() {
  # read Export file
  Export <<- importFile("Export")
  
  # this is the soonest we can log the session data, with 
  # the export info, since this is the first time we import the Export.csv file
  # logSessionData() 
  
  # read Client file
  Client <- importFile("Client")
  
  # decide if the export is hashed
  return(  
    # TRUE
    Export$HashStatus == 4 &
      min(nchar(Client$FirstName), na.rm = TRUE) ==
      max(nchar(Client$FirstName), na.rm = TRUE)
  )
}

isFY2024Export <- function() {
  return(
    grepl("2024", as.character(importFile("Export")$CSVVersion))
  )
}

