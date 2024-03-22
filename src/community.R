#read links
community_links <- GetLinks(community_url,'community-services')
community_links2 <- GetLinks(paste0('https://digital.nhs.uk',community_links),'')

#different structures: prejan20 the data was all in one links directly from links2
#afterwards it was in an ADDITIONAL link, half of it is zip the other csv
community_links_prejan20 <- community_links2[grepl(pattern='exp-data.csv',community_links2)]
#looks confusing but just removes provisional datasets and extracts the links
community_links_postjan20 <- GetLinks(
  paste0('https://digital.nhs.uk',community_links2[grepl(pattern='/data-sets$|/datasets$',community_links2)]),'exp-data|exp-core-data')
#remove anything with prov in it: i.e provisional datasets
community_links_postjan20 <- community_links_postjan20[!grepl(pattern='-prov',community_links_postjan20)]
#get all community files together
community_files <- c(community_links_prejan20,community_links_postjan20)
#insane, but needed
community_zip <- community_files[grepl('.zip',community_files)]
community_csv <- community_files[grepl('.csv',community_files)]

#read the files appropriately
community_zip_files<- lapply(community_zip,
                         function(x){
                           UnzipCSV(x)
                         }) %>%
  purrr::flatten()

community_csv_files <- lapply(community_csv,
                           function(x){
                             data.table::fread(x,fill=T)
                           })

#Merge the files together
community_data <- c(community_zip_files,community_csv_files)
#comm names change annoyingly, so this just unifies them into prexisting system
comm_names <- c('period',
                'trust_code',
                'org_code',
                'geo_code',
                'org_name',
                'dimension',
                'measure',
                'measure_desc',
                'measure2',
                'measure2_desc',
                'count',
                'value',
                'value_0_18',
                'value_19')

#flatten the data and apply logic then rbind; takes longer than should?
FINAL_community_data <- lapply(community_data,
               function(x){
                 names(x)[1] <- 'period'
                 data <- x %>% 
                   janitor::clean_names() %>%
                   select(!any_of('reporting_period_end')) %>%
                   #filter(dimension %in% c('CareContacts','Referrals','UniqueCareContacts')) %>%
                   #filter(geo_code == 'Provider') %>%
                   dplyr::mutate(period = lubridate::parse_date_time(period,order=c('%d%m%y','%y%m%d'))) %>%
                   dplyr::select(any_of(c('period','org_code','organisation_code','measure_value','value','dimension')))
                 names(data) <- c('period','org_code','count','dimension')
                 return(data)
                 }) %>%
  data.table::rbindlist()


