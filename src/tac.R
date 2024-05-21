#Tac urls
tac_urls_post20 <- GetLinks(tac_url,'data-published-in')
tac_urls_pre19 <- GetLinks(GetLinks(tac_url,'2019-20-accounts'),'.xlsx')
tac_urls <- c(tac_urls_post20,tac_urls_pre19,tac_17_url,tac_18_url)

tac_old_links <- GetLinks(tac_old_url,'files')
tac_old_links2 <- GetLinks(paste0('https://www.gov.uk',tac_old_links),'.xlsx')
tac_old_links3 <- tac_old_links2[!grepl(pattern='illustrative|Illustrative',tac_old_links2)]

#Read all
tac_data <- lapply(tac_urls,
                   function(x){
                     data <- ReadExcel(x, 3)  %>%
                       data.table::rbindlist()%>%
                       mutate(id = x)
                     names(data) <- c('org_name','worksheet','table','maincode','row','subcode','total','id')
                     return(data)
                   })

old_tac_data <- lapply(tac_old_links3,
                   function(x){
                     data <- ReadExcel(x, 'All data') %>%
                       data.table::rbindlist() %>%
                       mutate(date = x)
                     names(data) <- c('org_name','worksheet','table','maincode','row','subcode','total','id')
                     return(data)
                   })

tac_data_old <- old_tac_data %>% 
  data.table::rbindlist(fill=T) %>%
  rowwise() %>%
  dplyr::mutate(date = 2017 - which(tac_old_links3 == id))

tac_data_new <- tac_data %>%
  data.table::rbindlist(fill=T) %>%
  dplyr::mutate(date = stringr::str_extract(pattern='(200[0-9]|20[12][0-9]|2030)',id),
                type = stringr::str_extract(pattern='NHS-trust-accounts|NHS-foundation-trust-accounts',id))

# tac_data_new1 <- tac_data_new %>%
#   filter(maincode=='A14CY01' & subcode == 'PPE0420' | maincode=='A09CY01' & subcode == 'STA0490') %>%
#   group_by(id,subcode) %>%
#   summarise(total=sum(total,na.rm=T)) %>%
#   tidyr::pivot_wider(names_from=subcode,values_from=total)
# 
# tac_data_old1 <- tac_data_old %>%
#   filter(maincode=='08K' & subcode == '150' | maincode=='14J' & subcode == '125') %>%
#   mutate(type = 'NHS-foundation-trust-accounts') %>%
#   group_by(date,type,subcode) %>%
#   summarise(total = sum(total))
# 
# write.csv(tac_data_old1,'old_tac.csv')
# write.csv(tac_data_new1,'new_tac.csv')
