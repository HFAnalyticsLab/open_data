eric_links <- GetLinks(eric_url,'estates-returns-information-collection/')

trust_links <- lapply(eric_links,
                      function(x){
                        if(x == "/data-and-information/publications/statistical/estates-returns-information-collection/england-historical-data-files-1999-2000-to-2013-14"){
                          filter_dat <- '.xls|.XLS'
                          test<-GetLinks(paste0('https://digital.nhs.uk',x),
                                         filter_dat)
                          test<-test[grepl('.csv|xls|XLS',test)]
                          #remove double counting
                          test<-test[!grepl('eric-201617',test)]
                        } else if(x == '/data-and-information/publications/statistical/estates-returns-information-collection/estates-returns-information-collection-eric-england-2014-15'){
                          filter_dat <- 'est-ret-info-col-2014-2015-tru-lev'
                          test<-GetLinks(paste0('https://digital.nhs.uk',x),
                                         filter_dat)
                          test<-test[grepl('.csv',test)]
                          #remove double counting
                          test<-test[!grepl('eric-201617',test)]
                        } else {
                          filter_dat <- 'trust'
                          test<-GetLinks(paste0('https://digital.nhs.uk',x),
                                         filter_dat)
                          test<-test[grepl('.csv',test)]
                          #remove double counting
                          test<-test[!grepl('eric-201617',test)]
                        }
                        return(test)
                      })  %>%
  unlist()

trust_data <- lapply(trust_links[1:19],
                     function(x){
                       if(grepl('xls|XLS',x) == TRUE){
                         temp = tempfile(fileext = ".xls")
                         download.file(x, destfile=temp, mode='wb',skip=2)
                         output <- readxl::read_xls(temp, sheet ='Trust Data',skip=1)
                       } else if(x %in% c( "https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18726/est-ret-info-col-2014-2015-tru-lev-dat-v2.csv",
                                           "https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21992/est-ret-info-col-2015-2016-trust-data.csv")){
                         output <- data.table::fread(x,skip=1)
                       } else{
                         output <- data.table::fread(x)
                       }
                       output <- output %>% 
                         janitor::clean_names() %>%
                         dplyr::mutate(date=2023-match(x,trust_links)) %>%
                         dplyr::rename(.,dplyr::any_of(c(trust_code='organisation_code'))) %>%
                         dplyr::select(date,
                                trust_code,
                                dplyr::starts_with('investment'),
                                dplyr::any_of(dplyr::starts_with('capital_investment_for_maintaining'))) %>%
                         rename(.,dplyr::any_of(c(capital_investment_for_maintaining_lifecycle_existing_buildings='investment')))
                       return(output)
                     }) %>%
  data.table::rbindlist(fill=T) %>%
  filter(trust_code != 'Trust Code') %>%
  dplyr::mutate(investment1 = as.numeric(gsub(",","",investment_to_reduce_backlog_maintenance)),
                investment2 = as.numeric(gsub(",","",capital_investment_for_maintaining_lifecycle_existing_buildings))) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(investment = investment1+investment2)%>%
  dplyr::select(!c(investment1,investment2,starts_with('investment_to'),capital_investment_for_maintaining_lifecycle_existing_buildings)) %>%
  tidyr::drop_na()
