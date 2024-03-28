ae_links_post17 <- GetLinks(ae_url,'/ae-attendances-and-emergency-admissions-')
ae_links_pre15 <- GetLinks(ae_url,'areas/ae-waiting-times-and-activity/weekly-ae-sitreps')
ae_links_15to17 <- GetLinks(ae_url,'admissions-2015-16-monthly-3|-and-emergency-admissions-2016-17/')

ae_files_post17 <- GetLinks(ae_links_post17,'.csv')
#note: we have to add the odd xls from 17. ugh
ae_files_15to17 <- c(GetLinks(ae_links_15to17,'.xls'),GetLinks(ae_links_post17,'2017-AE-by|2018-AE-by-provider-revised'))
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

FINAL_ae_data_post17 <- ae_data_post17 %>%
  dplyr::mutate(
    remergency_type_1 = rowSums(across(c(number_of_a_e_attendances_type_1,a_e_attendances_type_1)),na.rm=T),
    remergency_type_2 = rowSums(across(c(number_of_a_e_attendances_type_2,a_e_attendances_type_2)),na.rm=T),
    remergency_type_3 = rowSums(across(c(number_of_a_e_attendances_other_a_e_department,a_e_attendances_other_a_e_department)),na.rm=T),
    remergency_admissions_type_1 = emergency_admissions_via_a_e_type_1,
    remergency_admissions_type_2 = emergency_admissions_via_a_e_type_2,
    remergency_admissions_type_3 = rowSums(across(c(other_emergency_admissions,emergency_admissions_via_a_e_other_a_e_department)),na.rm=T),
    remergency_breaches_type_1 = rowSums(across(c(attendances_over_4hrs_type_1,number_of_attendances_over_4hrs_type_1)),na.rm=T),
    remergency_breaches_type_2 = rowSums(across(c(attendances_over_4hrs_type_2,number_of_attendances_over_4hrs_type_2)),na.rm=T),
    remergency_breaches_type_3 = rowSums(across(c(attendances_over_4hrs_other_department,number_of_attendances_over_4hrs_other_a_e_department)),na.rm=T),
    date = lubridate::my(substr(x=period,start=8,stop=nchar(period))),
    code = org_code) %>%
  dplyr::select(date,code,starts_with('remergency_'))
  
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
  

FINAL_ae_data_pre15 <- ae_data_pre15a %>%
  dplyr::mutate_at(names(ae_data_pre15a)[!names(ae_data_pre15a) %in% c('na','area_team','date','sha','code','name')], ~as.numeric(.)) %>%
  dplyr::mutate(
    remergency_type_1 = type_1_departments_major_a_e,
    remergency_type_2 = type_2_departments_single_specialty,
    remergency_type_3 = type_3_departments_other_a_e_minor_injury_unit,
    remergency_breaches_type_1 = type_1_departments_major_a_e_2,
    remergency_breaches_type_2 = type_2_departments_single_specialty_2,
    remergency_breaches_type_3 = type_3_departments_other_a_e_minor_injury_unit_2,
    remergency_admissions_type_1 = emergency_admissions_via_type_1_a_e,
    remergency_admissions_type_2 = emergency_admissions_via_type_2_a_e,
    remergency_admissions_type_3 = emergency_admissions_via_type_3_and_4_a_e,
    date = lubridate::dmy(substr(date,nchar(date)-11,nchar(date)))
  ) %>%
  select(date,code,contains(c('remergency'))) %>%
  mutate(date = lubridate::make_date(year=lubridate::year(date),month=lubridate::month(date),day=1L)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(date,code) %>%
  summarise(across(starts_with('remergency'), sum, .names = "{.col}"))

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

FINAL_ae_data_15to17 <- ae_data_15to17 %>%
  dplyr::mutate_at(names(ae_data_15to17)[!names(ae_data_15to17) %in% c('na','area_team','date','region','code','name')], ~as.numeric(.)) %>%
  dplyr::mutate(
    remergency_type_1 = type_1_departments_major_a_e,
    remergency_type_2 = type_2_departments_single_specialty,
    remergency_type_3 = type_3_departments_other_a_e_minor_injury_unit,
    remergency_breaches_type_1 = type_1_departments_major_a_e_2,
    remergency_breaches_type_2 = type_2_departments_single_specialty_2,
    remergency_breaches_type_3 = type_3_departments_other_a_e_minor_injury_unit_2,
    remergency_admissions_type_1 = emergency_admissions_via_type_1_a_e,
    remergency_admissions_type_2 = emergency_admissions_via_type_2_a_e,
    remergency_admissions_type_3 = emergency_admissions_via_type_3_and_4_a_e,
    date = case_when(
      substr(date,1,4) == 'Week' ~ lubridate::dmy(substr(date,nchar(date)-11,nchar(date))),
      TRUE ~ lubridate::my(date)
    )
  ) %>%
  select(date,code,contains(c('remergency'))) %>%
  mutate(date = lubridate::make_date(year=lubridate::year(date),month=lubridate::month(date),day=1L)) %>%
  group_by(date,code) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  summarise(across(starts_with('remergency'), sum, .names = "{.col}"))

FINAL_ae_data <- rbind(FINAL_ae_data_15to17,FINAL_ae_data_pre15,FINAL_ae_data_post17) %>%
  drop_na()%>%
  mutate(period_year=year(date),
         period_month = month(date)) %>%
  select(!date)%>%
  ungroup()%>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(period_year,period_month,code) %>%
  summarise(across(starts_with('remergency'), sum, .names = "{.col}")) %>%
  mutate(date = make_date(year=period_year,month=period_month,day=1L),
         org_code = code)
       