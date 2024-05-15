therapy_links <- GetLinks(therapy_url,'annual-reports/')
therapy_links2 <- GetLinks(paste0('https://digital.nhs.uk/',therapy_links),'psych-ther-ann-rep-csvs')

iapt_links <- GetLinks(iapt_url,'annual-report-2')
iapt_links2 <- GetLinks(paste0('https://digital.nhs.uk/',iapt_links),'.zip')
iapt_links3 <- iapt_links2[grepl(pattern='psych-ther-',iapt_links2)]

all_iapt_links <- c(iapt_links3,therapy_links2)

therapy_data <- lapply(all_iapt_links,
               function(x){
                 UnzipCSV(x,'main|8a')
               }) %>%
  purrr::flatten()

therapy_data_post_16 <- therapy_data[c(1:4,7)] %>%
  data.table::rbindlist(fill=T,idcol=T)

therapy_data_pre_16 <- therapy_data[c(5:6)] %>%
  data.table::rbindlist(fill=T,idcol=T)
