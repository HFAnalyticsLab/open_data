ae_links <- GetLinks(ae_url,'/ae-attendances-and-emergency-admissions')
ae_files <- GetLinks(ae_links,'.csv')

ae_data <- lapply(ae_files,
                  function(x){
                    data <- data.table::fread(x) %>%
                      dplyr::mutate(name = x) %>%
                      janitor::clean_names() %>%
                      dplyr::rowwise() %>%
                      dplyr::group_by(period,org_code) %>%
                      dplyr::summarise(ae_attendances = sum(c_across(contains('a_e_attendances'))),
                                       ae_breaches = sum(c_across(contains('over_4hrs'))),
                                       ae_admissions = sum(c_across(contains('emergency_admissions'))),
                                       admission_breaches = sum(c_across(contains('patients_who'))))
                  }) %>%
                      data.table::rbindlist()

FINAL_ae_data <- ae_data %>%
  dplyr::filter(period != 'TOTAL' & period != '') %>%
  dplyr::mutate(period = lubridate::my(substr(period,8,nchar(period)))) %>%
  rename(trust_code='org_code')