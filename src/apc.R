apc_links <- GetLinks(apc_url,'rovisional-monthly-hospital-episode-statistics-for-admitted-patient-care-outpatient-and-accident-and-emergency-data')
apc_links2 <- GetLinks(paste0('https://digital.nhs.uk/',apc_links),'.csv')
apc_links3 <- apc_links2[grepl(pattern='OPEN_DATA.csv|MDBP.csv',apc_links2)]

apc_data <- lapply(apc_links3,
               function(x){
                 data <- data.table::fread(x)
                 return(data)
               }) %>%
  data.table::rbindlist()


FINAL_apc_data <- apc_data %>%
  mutate(date = 
           lubridate::my(CALENDAR_MONTH_END_DATE)) %>%
  select(!CALENDAR_MONTH_END_DATE) %>%
  janitor::clean_names()

