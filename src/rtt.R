#Urls for datasets (1:3 means it only gets the past 3 years)
rtt_link <- GetLinks(rtt_url,'statistical-work-areas/rtt-waiting-times/rtt-data-')
rtt_files <- GetLinks(rtt_link,'Full-CSV-data')

#Read all the data (currently from 2016, if you want last 4 years change the GETLINKS URL)
rtt_data <- sapply(rtt_files,
                   function(x){
                     UnzipCSV(x)
                   })

#Apply function to all .zip links
rtt_data2 <- lapply(rtt_data,
                   function(x){
                     if(ncol(x) == 71){
                       names(x) <- as.character(x[3,])
                       x <- x[-(1:3),]
                       return(x)
                     } else {
                       return(x)
                     }
                   })

FINAL_rtt <- lapply(rtt_data2,
                    function(x){
                      x %>%
                        dplyr::mutate(all_rtt = total_all,
                                      date = zoo::as.yearqtr(zoo::as.yearmon(substr(period,5,99),'%B-%Y'))) %>%
                        #Remove all the columns that are 'waiting 18 to 20 weeks' or whaever,
                        #We aren't using them and there is mismatch after 2019/20
                        dplyr::select(-starts_with('gt')) %>%
                        #Select ALL specialties
                        #dplyr::filter(treatment_function_name == 'Total') %>%
                        dplyr::rename( 'trust_code' = provider_org_code)
                    }) %>%
  #collapse the list
  data.table::rbindlist(fill=T) %>%
  #remove treatment_function_name if needed?
  dplyr::group_by(date,treatment_function_code,treatment_function_name,rtt_part_description,trust_code) %>%
  dplyr::summarise(all_rtt = sum(all_rtt,na.rm=T)) %>%
  #Useful for left_joins
  tidyr::pivot_wider(.,names_from=rtt_part_description,values_from=all_rtt)
