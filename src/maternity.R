msds_links <- GetLinks(msds_url,'/data-and-information/publications/statistical/maternity-services-monthly-statistics')
msds_links2 <- GetLinks(paste0('https://digital.nhs.uk',msds_links),'.csv')
msds_links3 <- msds_links2[grepl(pattern='data',msds_links2)]
msds_links4 <- msds_links3[!grepl(pattern='RESTRICTED|prov|provisional|3Provisional|4Provisional',msds_links3)]

msds_names <- c(
  'period',
  'dimension',
  'org_level',
  'org_code',
  'org_name',
  'measure',
  'count_of',
  'value'
)

msds_data <- lapply(
  msds_links4,
  function(x){
    data <- data.table::fread(x) %>%
      janitor::clean_names()
    return(data)
  }
)

msds_data2 <- lapply(
  msds_data,
  function(x){
    if(ncol(x) == 10){
      x %>%
        mutate(measure = paste0(measure,count_of)) %>%
        select(-any_of('reporting_period_end_date'),-count_of,-org_geog_code)
    } else {
      x %>%
        select(-any_of(c('reporting_period_end_date','geog_code')))
    }
  }
)

msds_data3 <- purrr::keep(msds_data2,
                               function(x) ncol(x)!=9)


FINAL_msds_data <- 
  lapply(msds_data3,
         function(x){
           x %>%
             setNames(.,nm=msds_names) %>%
             mutate(period = as.character(period))
         }) %>%
  data.table::rbindlist()
