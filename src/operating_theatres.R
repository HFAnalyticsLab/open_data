#operating table links. We have to get rid of 2013 to 2015 as:
#1) they're not needed
#2) change in formatting messes up the read. 
#This is a hackish approach i am aware :(
op_th_links <- GetLinks(op_th_url,'Operating-Theatres')
op_th_links <- op_th_links[!grepl("transparency", op_th_links)]
op_th_links<-op_th_links[!grepl("-2013|-2012|-2014|-2015", op_th_links)]


op_th_raw_data <- lapply(op_th_links,
                      function(x){
                        data <- openxlsx::read.xlsx(x)
                        data <- data %>%
                          #Extracts year and date from string
                          mutate(date = paste0(stringr::str_sub(data[2,2],nchar(data[2,2])-4,nchar(data[2,2])-1),
                                               ' Q',
                                               stringr::str_sub(data[2,2],9,9)
                          )
                          )
                        #fix rows; names are row 12 everything prior is useless
                        names(data) <- data[12,]
                        data <- data[-c(1:14),]
                        #new row called date that we mutated.
                        names(data)[7] <- 'date'
                        #fix names
                        names(data) <- names(data) %>% 
                          janitor::make_clean_names()
                        data})

#Final output file
FINAL_op_th_data <- op_th_raw_data %>%
  #merge together
  data.table::rbindlist() %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  rename( 'trust_code' = organisation_code) %>%
  group_by(date,trust_code) %>%
  summarise(operating_theatres = sum(as.numeric(number_of_operating_theatres),na.rm=T))
