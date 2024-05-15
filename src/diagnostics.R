
#Get the full extracts
diag_links <-GetLinks(diag_url,'monthly-diagnostics-waiting-times-')
diag_links <-GetLinks(diag_links,'full-extract')
#Remove CDCs (should these be included?)
diag_links <- diag_links[!grepl("CDC", diag_links)]

#Download everything
raw_diagnostics_data <- lapply(diag_links,
                function(x){
                  UnzipCSV(x)
                    }) %>%
  purrr::flatten()


#Fix up data to be ready for use
FINAL_diagnostic_data <- lapply(raw_diagnostics_data,
                          function(x){
                            x %>%
                              #Extract data
                              dplyr::mutate(date = zoo::as.yearqtr(zoo::as.yearmon(substr(period,6,99),format='%B-%Y')),
                                            diagnostic_tests = case_when(
                                              diagnostic_tests == 'TOTAL' ~ 'all_diagnostics_tests',
                                              TRUE ~ diagnostic_tests)) %>%
                              #filter(diagnostic_tests == 'TOTAL') %>%
                              dplyr::select(diagnostic_tests,commissioner_parent_org_code,date,total_activity,provider_org_code) %>%
                              dplyr::rename('org' = commissioner_parent_org_code,
                                            'trust_code' = `provider_org_code`)
                          }) %>%
  #Bind everything together and group and summarise
  data.table::rbindlist() %>%
  dplyr::group_by(trust_code,date,diagnostic_tests) %>%
  dplyr::summarise(tests = sum(total_activity,na.rm=T),.groups='keep') %>%
  tidyr::pivot_wider(.,names_from = diagnostic_tests,values_from=tests) %>%
  janitor::clean_names()
