night_bed_links <- GetLinks(overnight_beds_url,'.xls')

night_bed_data <- lapply(night_bed_links,
                       function(x){
                         data <- ReadExcel(x,1)
                       }) %>%
  flatten()

night_bed_names <- c(
  'fyear',
  'month',
  'DROP_region_code',
  'org_code',
  'org_name',
  'all_beds',
  'general_acute_beds',
  'learning_disability_beds',
  'maternity_beds',
  'mental_illness_beds',
  'DROP',
  'all_occupied_beds',
  'occupied_general_acute_beds',
  'occupied_learning_disability_beds',
  'occupied_maternity_beds',
  'occupied_mental_illness_beds',
  'DROP2',
  'DROP3',
  'DROP4',
  'DROP5',
  'DROP6',
  'DROP7',
  'DROP8',
  'DROP9',
  'DROP10'
)

night_bed_data2 <- lapply(night_bed_data,
                        function(x){
                          data_start <- min(which(x[1] == 'Year'))
                          names(x) <- x[data_start,]
                          x <- x[(data_start+1):nrow(x),] %>%
                            janitor::clean_names()
                        })

FINAL_night_bed_data <- purrr::keep(night_bed_data2,
                                  function(x) ncol(x)==22) %>%
  data.table::rbindlist(fill=T) %>%
  mutate(period_end = case_when(
    is.na(period_end) == T ~ period,
    TRUE ~ period_end
  ))

names(FINAL_night_bed_data) <- night_bed_names

FINAL_night_bed_data<-FINAL_night_bed_data %>% 
  select(!starts_with('DROP')) %>%
  dplyr::mutate_at(names(.)[!names(.) %in% c('fyear','month','org_code','org_name')], ~as.numeric(.)) %>%
  drop_na() %>%
  mutate(year = case_when(as.numeric(month)<=3 ~ as.numeric(substr(fyear,1,4))-1,
                     TRUE ~ as.numeric(substr(fyear,1,4))),
         date = lubridate::my(paste0(month,'-',year)),
         quarter_date = zoo::as.yearqtr(date))
