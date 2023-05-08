library(openxlsx)

filename = "APR RRH I & II FY24"

wb = createWorkbook()

for (question in APR_files) {
  if (exists(question)) {
    
    to_write <- get(question)
    
    if(question != "Q4a") {
      to_write <- to_write %>%
        set_hud_format()
    }
    
    to_write <- to_write %>% 
      ifnull(., 0) 
    
    to_write[is.na(to_write)] <- ""
    
    writeData(wb, to_write, sheet = addWorksheet(wb, question), rowNames=FALSE)
    
  }
}

saveWorkbook(wb, paste0(filename, ".xlsx"))
