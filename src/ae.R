ae_links_post17 <- GetLinks(ae_url,'/ae-attendances-and-emergency-admissions-')
ae_links_pre15 <- GetLinks(ae_url,'areas/ae-waiting-times-and-activity/weekly-ae-sitreps')
ae_links_15to17 <- GetLinks(ae_url,'2015-16/|-and-emergency-admissions-2016-17/')

ae_files_post17 <- GetLinks(ae_links_post17,'.csv')
ae_files_15to17 <- GetLinks(ae_links_15to17,'.xls')
ae_files_15to17 <- ae_files_15to17[!grepl(pattern='Q1|Q2|Q3|Q4|Quarterly|Quarter|Timeseries',ae_files_15to17)]
ae_files_pre15 <- GetLinks(ae_links_pre15,'.xls')
ae_files_pre15 <- ae_files_pre15[!grepl(pattern='Q1|Q2|Q3|Q4|Quarterly|Quarter|Timeseries',ae_files_pre15)]

#Use this for the post-17 data
ae_data_post17 <- lapply(ae_files_post17,
                  function(x){
                    data <- data.table::fread(x) %>%
                      dplyr::mutate(name = x) %>%
                      janitor::clean_names()
                  }) %>%
                      data.table::rbindlist(fill=T)

ae_data_post17a <- ae_data_post17 %>%
  dplyr::mutate(
    remergency_type_1 = rowSums(across(c(number_of_a_e_attendances_type_1,a_e_attendances_type_1)),na.rm=T),
    remergency_type_2 = rowSums(across(c(number_of_a_e_attendances_type_2,a_e_attendances_type_2)),na.rm=T),
    remergency_other = rowSums(across(c(number_of_a_e_attendances_other_a_e_department,a_e_attendances_other_a_e_department)),na.rm=T),
    remergency_admission_type_1 = emergency_admissions_via_a_e_type_1,
    remergency_admission_type_2 = emergency_admissions_via_a_e_type_2,
    remergency_admission_other = rowSums(across(c(other_emergency_admissions,emergency_admissions_via_a_e_other_a_e_department)),na.rm=T),
    remergency_breaches_type_1 = rowSums(across(c(attendances_over_4hrs_type_1,number_of_attendances_over_4hrs_type_1)),na.rm=T),
    remergency_breaches_type_2 = rowSums(across(c(attendances_over_4hrs_type_2,number_of_attendances_over_4hrs_type_2)),na.rm=T),
    remergency_breaches_other = rowSums(across(c(attendances_over_4hrs_other_department,number_of_attendances_over_4hrs_other_a_e_department)),na.rm=T),
    remergency_admission_breaches = rowSums(across(c(patients_who_have_waited_12_hrs_from_dta_to_admission,patients_who_have_waited_4_12_hs_from_dta_to_admission)),na.rm=T)) %>%
  dplyr::select(period,org_code,name,starts_with('remergency_'))
  

#Use this for the pre-2015 data
ae_data_pre15 <- lapply(ae_files_pre15,
                  function(x){
                    data <- ReadExcel(files = x, sheets = 1)
                  }) %>% 
  flatten()

ae_data_pre15a <- lapply(ae_data_pre15,
                        function(x){
                          names(x) <- x[14,]
                          x['date'] <- x[4,2]
                          data <- x[-c(1:16),]
                          data <- data %>% janitor::clean_names()
                          return(data)
                        }) %>%
  data.table::rbindlist(fill=T)
  

final_ae_data_pre15 <- ae_data_pre15a %>%
  dplyr::mutate_at(names(ae_data_pre15a)[!names(ae_data_pre15a) %in% c('na','area_team','date','sha','code','name')], ~as.numeric(.)) %>%
  dplyr::mutate(
    remergency_type_1 = rowSums(across(c(contains('type_1_departments_major'))),na.rm=T),
    remergency_type_2 = rowSums(across(c(contains('type_2_departments_single'))),na.rm=T)
  ) %>%
  select(code,name,date,contains(c('remergency')))

#Use this for the 2015 to 2017 data
ae_data_15to17a <- lapply(ae_files_15to17,
                          function(x){
                            data <- ReadExcel(files=x,sheets=1)
                          }) %>%
  flatten()

ae_data_15to17 <- lapply(ae_data_15to17a,
                         function(x){
                           names(x) <- x[14,]
                           x['date'] <- x[4,2]
                           data <- x[-c(1:16),]
                           data <- data %>% janitor::clean_names()
                           return(data)
                         }) %>%
  data.table::rbindlist(fill=T)

FINAL_ae_data <- ae_data %>%
  dplyr::filter(period != 'TOTAL' & period != '') %>%
  dplyr::mutate(period = lubridate::my(substr(period,8,nchar(period)))) %>%
  rename(trust_code='org_code')