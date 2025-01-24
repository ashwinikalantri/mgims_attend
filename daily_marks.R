withProgress(message = "Updating NMC attendance data", value = 0, {
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(DBI)
  library(stringr)
  
  #rm(list = ls())
  
  httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "PostgreSQL",
    Server = Sys.getenv("db_server"),
    Database = "postgres",
    UID = Sys.getenv("db_uid"),
    PWD = Sys.getenv("db_pwd"),
    Port = 6543
  )
  
  data_date <- as.Date(
    dbGetQuery(
      con,
      "SELECT MAX(log_date) AS \"last_date\" FROM nmcatt_api_attend;"
    )$last_date
  ) + 1
  
  nmc_api <- Sys.getenv("nmcapi")
  
  #Attendance####
  url <-
    paste0(
      "https://basreports.attendance.gov.in/api/unibasglobal/api/attendance/offset/0/count/1000/reportdate/",
      data_date,
      "/apikey/",
      nmc_api
    )
  data <- GET(url = url)
  incProgress(20/100, detail = paste("Downloading Data till",data_date))
  data <- fromJSON(rawToChar(data$content))
  data <- data[["successattendance"]]
  
  if (length(data) != 0) {
    devid <- unique(c(data$in_device_id, data$out_device_id))[!unique(c(data$in_device_id, data$out_device_id)) %in% c("")]
    dev_details <- data.frame()
    for (i in devid) {
      url_dev <- paste0(
        "https://basreports.attendance.gov.in/api/unibasglobal/api/detailsbydeviceid/deviceid/",
        i,
        "/apikey/",
        nmc_api
      )
      dev_data <- GET(url = url_dev)
      dev_data <- fromJSON(rawToChar(dev_data$content))
      dev_data <- dev_data[["detailsbydeviceid"]]
      
      dev_details <- dev_details %>% bind_rows(dev_data)
    }
    
    dev_details <- dev_details %>%
      mutate(entry_name = if_else(org_id == "000046", entry_name, str_to_title(org_name)))
    
    data <- data %>%
      left_join(
        dev_details %>% select(device_id, entry_name) %>% rename("in_location" = "entry_name"),
        by = c("in_device_id" = "device_id")
      ) %>%
      left_join(
        dev_details %>% select(device_id, entry_name) %>% rename("out_location" = "entry_name"),
        by = c("out_device_id" = "device_id")
      )
    
    incProgress(10/100, detail = paste("Attendance Data updated till ",data_date))
    dbWriteTable(con,
                 'nmcatt_api_attend',
                 data,
                 row.names = FALSE,
                 append = TRUE)
    
    
    print(paste0("Attendance Updated for ", format(data_date, "%d %b %Y")))
  } else {
    print("No New Attendance Data")
    incProgress(30/100, detail = paste("No New Attendance Data"))
  }
  
  #Leaves####
  
  dates <- as.character(seq(as.Date("2024-01-01"), data_date, by = "days"))
  
  df_l <- data.frame()
  df_t <- data.frame()
  
  for (i in dates) {
    incProgress((20/100)/length(dates), detail = paste0("Getting leave data for ", format(as.Date(i), "%d %b %Y")))
    url_l <-
      paste0(
        "https://basreports.attendance.gov.in/api/unibasglobal/api/orgleavebydate/offset/0/count/200/reportdate/",
        i,
        "/apikey/",
        nmc_api
      )
    
    ldata <- GET(url = url_l)
    ldata <- fromJSON(rawToChar(ldata$content))
    ldata <- ldata[["leavedetails"]]
    ldata <- ldata %>% mutate(date = i)
    df_l <- df_l %>% bind_rows(ldata) %>% unique() %>% filter(active_status == "Y")
    
    url_t <-
      paste0(
        "https://basreports.attendance.gov.in/api/unibasglobal/api/orgtourbydate/offset/0/count/200/reportdate/",
        i,
        "/apikey/",
        nmc_api
      )
    
    tdata <- GET(url = url_t)
    tdata <- fromJSON(rawToChar(tdata$content))
    tdata <- tdata[["tourdetails"]]
    tdata <- tdata %>% mutate(date = i)
    df_t <- df_t %>% bind_rows(tdata) %>% unique() %>% filter(active_status == "Y")
  }
  
  leaves <- df_l %>% bind_rows(df_t) %>%
    mutate(date = as.Date(date)) %>%
    select(date, emp_id, leave_type, reason, created_by, creation_date) %>%
    unique()
  
  dbWriteTable(con,
               'nmcatt_api_leave',
               leaves,
               row.names = FALSE,
               overwrite = TRUE)
  #incProgress(20/100, detail = paste0("Leaves Updated till ", format(data_date, "%d %b %Y")))
  print(paste0("Leaves Updated on ", format(data_date, "%d %b %Y")))
  
  #Employee####
  url_emp <- paste0(
    "https://basreports.attendance.gov.in/api/unibasglobal/api/employee/offset/0/count/1000/apikey/",
    nmc_api
  )
  data_emp <- GET(url = url_emp)
  data_emp <- jsonlite::fromJSON(rawToChar(data_emp$content))
  data_emp <- data_emp[["employee"]]
  data_emp <- data_emp %>%
    filter(!is.na(emp_name)) %>%
    mutate(designation = str_to_title(designation)) %>%
    mutate(designation = str_remove(designation, pattern = " Ng")) %>%
    mutate(division = str_to_title(division)) %>%
    mutate(emp_name = str_to_title(emp_name)) %>%
    mutate(
      cat = case_when(
        designation %in% c(
          "Assistant Professor",
          "Statistician",
          "Associate Professor",
          "Professor"
        ) ~ "Faculty",
        designation %in% c("Junior Resident", "Tutor") ~ "Junior Resident",
        designation %in% c("Senior Resident") ~ "Senior Resident",
        .default = "Other"
      )
    )
  old_emp <- dbGetQuery(con, "SELECT * FROM nmcatt_api_emp;")
  
  emp <- data_emp %>% bind_rows(old_emp) %>%
    mutate(dup = duplicated(emp_id)) %>%
    group_by(emp_id) %>%
    filter(dup == "FALSE") %>%
    select(-dup)
  
  dbWriteTable(con,
               'nmcatt_api_emp',
               emp,
               row.names = FALSE,
               overwrite = TRUE)
  
  incProgress(25/100, detail = paste0("Employee data updated till ", format(data_date, "%d %b %Y")))
  print(paste0("Employee data updated on ", format(data_date, "%d %b %Y")))
  
  #Holidays
  
  h_url <- paste0("https://basreports.attendance.gov.in/api/unibasglobal/api/holiday/",
                  nmc_api)
  hol <- GET(url = h_url)
  hol <- fromJSON(rawToChar(hol$content))
  hol <- hol[["holiday"]]
  
  dbWriteTable(con,
               'nmcatt_api_holidays',
               hol,
               row.names = FALSE,
               overwrite = TRUE)
  incProgress(25/100, detail = paste0("Holiday data updated till ", format(data_date, "%d %b %Y")))
  
  timestamp()
})