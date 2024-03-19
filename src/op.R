op_links <- GetLinks(op_url,'hospital-outpatient-activity-|hospital-outpatient-activity/2')
op_links2 <- GetLinks(paste0('https://digital.nhs.uk/',op_links),'-pla-')
op_links_csv <- op_links2[grepl(pattern='.csv',op_links2)]
op_links_xls <- op_links2[grepl(pattern='xls',op_links2)]

op_type_data <- sapply(op_links_xls,
                  function(x){ 
                    data <- as.data.frame(ReadExcel(x,'Table 2'))
                    data_start <- min(which(data[1] == 'All'))
                    names(data) <- (data[data_start - 1,])
                    names(data)[1:2] <- c('org_code','description')
                    data <- data[data_start:nrow(data),] %>%
                      janitor::clean_names() %>%
                      dplyr::mutate(date = str_extract(pattern='(200[0-9]|20[12][0-9]|2030)',x))
                    return(data)
                  })

op_type_data_csv <- lapply(op_links_csv,
                           function(x){
                             data <- data.table::fread(x) %>%
                               filter(MEASURE_TYPE == 'Attendance Type') %>%
                               select(!MEASURE_TYPE) %>%
                               janitor::clean_names()
                             names(data) <- c('date','geography','org_code','description','metric','values')
                             return(data)
                             }) %>%
  data.table::rbindlist(fill=T)

op_type_data_xls <- op_type_data %>% 
  data.table::rbindlist(fill=T) %>%
  select(!contains('na')) %>%
  select(!contains('total')) %>%
  filter(description != 'UNKNOWN') %>%
  mutate(geography='Unavailable') %>%
  pivot_longer(cols=!c(org_code,description,date,geography),names_to='metric',values_to='values')

FINAL_op_data <-
  rbind(op_type_data_xls,op_type_data_csv)
