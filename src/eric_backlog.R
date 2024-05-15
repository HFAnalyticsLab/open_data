eric_links <- GetLinks(eric_url,'estates-returns-information-collection/')

site_links <- lapply(eric_links,
                     function(x){
                       if(x == "/data-and-information/publications/statistical/estates-returns-information-collection/england-historical-data-files-1999-2000-to-2013-14"){
                         filter_dat <- '.xls|.XLS'
                       } else if(x == '/data-and-information/publications/statistical/estates-returns-information-collection/estates-returns-information-collection-eric-england-2014-15'){
                         filter_dat <- 'est-ret-info-col-2014-2015-dat.csv'
                       } else {
                         filter_dat <- 'site'
                       }
                       test<-GetLinks(paste0('https://digital.nhs.uk',x),
                                      filter_dat)
                       test<-test[grepl('.csv|xls|XLS',test)]
                       #remove double counting
                       test<-test[!grepl('eric-201617',test)]
                       return(test)
                     }) %>%
  unlist()

site_data <- lapply(site_links[1:19],
                    function(x){
                      if(grepl('xls|XLS',x) == TRUE){
                        temp = tempfile(fileext = ".xls")
                        download.file(x, destfile=temp, mode='wb',skip=2)
                        output <- readxl::read_xls(temp, sheet ='Site Data',skip=1)
                      } else if(x %in% c('https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18726/est-ret-info-col-2014-2015-dat.csv',
                                         "https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21992/est-ret-info-col-2015-2016-site-data.csv")){
                        output <- data.table::fread(x,skip=1)
                      } else{
                        output <- data.table::fread(x)
                      }
                      output <- output %>% 
                        janitor::clean_names() %>%
                        dplyr::mutate(date=2023-match(x,site_links)) %>%
                        dplyr::rename(.,dplyr::any_of(c(trust_code='organisation_code'))) %>%
                        dplyr::select(date,
                               trust_code,
                               site_code,
                               site_type,
                               contains(c('low','high','significant','moderate')) & contains('cost')& contains('backlog')) %>%
                        tidyr::pivot_longer(cols=!c(date,trust_code,site_code,site_type),names_to='risk',values_to='cost') %>%
                        dplyr::mutate(risk = stringr::str_extract(pattern='high|low|significant|moderate',risk))
                      return(output)
                    }) %>%
  data.table::rbindlist(fill=T) %>%
  dplyr::filter(trust_code != 'Trust Code') %>%
  dplyr::mutate(cost = as.numeric(gsub(",","",cost))) %>%
  tidyr::drop_na() %>%
  dplyr::filter(cost!=0)
