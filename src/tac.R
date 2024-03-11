#Tac urls
tac_urls_post20 <- GetLinks(tac_url,'data-published-in')
tac_urls_pre19 <- GetLinks(GetLinks(tac_url,'2019-20-accounts'),'.xlsx')
tac_urls <- c(tac_urls_post20,tac_urls_pre19)

#Read all
tac_data <- sapply(tac_urls,
                   function(x){
                     data <- ReadExcel(x, 3) 
                   })

FINAL_tac_data <- tac_data %>%
  data.table::rbindlist(fill=T,idcol=TRUE) %>%
  dplyr::mutate(date = str_extract(pattern='(200[0-9]|20[12][0-9]|2030)',.id),
         type = str_extract(pattern='NHS-trust-accounts|NHS-foundation-trust-accounts',.id)) %>%
  dplyr::select(!.id)
  
