#Read in all gp links
gp_url <- c(gp_url1,gp_url2,gp_url3)
gp_files <- GetLinks(gp_url,'Daily|CCG_CSV')

#read in data
gp_data <- sapply(gp_files,
                   function(x){
                     data <- UnzipCSV(x)
                   },
                   USE.NAMES = T) %>%
  #like a pancake
  flatten()

#Filter out coverage files
gp_data2 <-purrr::keep(gp_data, !str_detect(names(gp_data), 'COVERAGE'))

#apply and bind together
FINAL_gp_data <- lapply(gp_data2,
                   function(x){
                     x %>%
                       dplyr::rename(dplyr::any_of(c(
                         ccg_code='sub_icb_location_code',
                         appt_mode='appointment_mode',
                         count = 'count_of_appointments',
                         appt_date='appointment_date',
                         appt_status = 'appointment_status',
                         ccg_ons_code='sub_icb_location_ons_code'))) %>%
                       dplyr::select(ccg_code,ccg_ons_code,appt_status,appt_date,hcp_type,appt_mode,count,time_between_book_and_appt) %>%
                       #different date formats, coerce into consistent one
                       dplyr::mutate(appt_date = zoo::as.yearmon(lubridate::parse_date_time(appt_date,orders=c('dmy'))),
                                     count = as.numeric(count))
                   }) %>%
  data.table::rbindlist() %>%
  #Group up to month/yr and summarise
  dplyr::group_by(across(c(-count))) %>%
  dplyr::summarise(count = sum(count,na.rm=T)) %>%
  #Remove oddities: there will be roughly 1300 obs that will be taken out, but this is
  #nothing in the grand scheme of things. Less than 0.001% or something.
  dplyr::filter(count != 0 |
                  is.na(appt_date) == F) %>%
  dplyr::rename(period = 'appt_date')

