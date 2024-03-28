day_bed_links <- GetLinks(day_bed_url,'.xls')

day_bed_data <- lapply(day_bed_links,
                       function(x){
                         data <- ReadExcel(x,1)
                       }) %>%
  flatten()

day_bed_names <- c(
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
  'DROP7'
)

day_bed_data2 <- lapply(day_bed_data,
                        function(x){
                          data_start <- min(which(x[1] == 'Year'))
                          names(x) <- x[data_start,]
                          x <- x[(data_start+1):nrow(x),] %>%
                            janitor::clean_names()
                        })

FINAL_day_bed_data <- purrr::keep(day_bed_data2,
                          function(x) ncol(x)>7) %>%
  data.table::rbindlist()

names(FINAL_day_bed_data) <- day_bed_names

FINAL_day_bed_data<-FINAL_day_bed_data %>% 
  select(!starts_with('DROP')) %>%
  dplyr::mutate_at(names(FINAL_day_bed_data)[!names(FINAL_day_bed_data) %in% c('fyear','month','org_code','org_name')], ~as.numeric(.)) %>%
  drop_na()

