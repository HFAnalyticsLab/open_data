covid_links <- GetLinks(covid_url,'Covid-Publication')
covid_links <- covid_links[!grepl("Supplementary", covid_links)]

FINAL_covid <- lapply(covid_links,
                      function(x){
                        da <- openxlsx::read.xlsx(x,sheet='Total Beds Occupied Covid')
                        names(da) <- da[9,]
                        da <- da[-c(1:17),]
                        da <- da %>%
                          dplyr::select(!c(Name,`NHS England Region`)) %>%
                          tidyr::pivot_longer(cols=!c(Code),names_to='date',values_to='covid_beds') %>%
                          dplyr::mutate(date = zoo::as.yearqtr(as.Date(as.numeric(date), origin = "1899-12-30"))) %>%
                          dplyr::rename('trust_code' = Code)
                        return(da)
                      }) %>%
  data.table::rbindlist() %>%
  #Note the summarise is outside the loop as there will be overlap in the quarterly date aggregations 
  #(wasted 2 hours debugging before I realised this...)
  dplyr::group_by(date,trust_code) %>%
  dplyr::summarise(covid_beds = sum(covid_beds))
