#Get URLS
pesa_url_2 <- GetLinks(pesa_url,'public-expenditure-statistical-analyses-')
#filter only for readable files
pesa_url_3 <- GetLinks(paste0('https://www.gov.uk/',pesa_url_2),'csv|xls|xlsx|ods')
#filter for the capter we want
pesa_url_4 <- pesa_url_3[grepl(pattern='Chapter_1_|Chapter_1.xlsx|chapter1.xls|chapter1.1.xlsx',pesa_url_3)]
#remove 2013 oddity
pesa_url_5 <- pesa_url_4[!grepl(pattern='PESA_2013_Cm_Chapter_1_Tables',pesa_url_4)]

#Read urls conditional on type of file
all_pesa_data <- sapply(pesa_url_5,
               function(x){
                 if(str_extract(x,'.xlsx|.xls|.ods') %in%  c('.xls','.xlsx')){
                   data <- ReadExcel(x)
                   data <- lapply(data,
                                  function(z){
                                    z %>% mutate(date = x)
                                  })
                 } else {
                  data <- ReadODS(x)
                  data <- lapply(data,
                                 function(z){
                                   z %>% mutate(date = x)
                                 })}
                 data
               },
               USE.NAMES = TRUE)


all_pesa_data2 <- lapply(all_pesa_data,
                         function(x){
                           #fix names
                           names(x) <- sapply(x,
                                              function(y){
                                                names(y)[1]
                                                })
                           x <- lapply(x,
                                       function(y){
                                         names(y) <- y[3,]
                                         names(y)[ncol(y)] <- 'date'
                                         names(y)[1] <- 'type'
                                         remove_cols <- unique(c(which(is.na(y[1] == TRUE)),which(is.na(y[2] == TRUE))))
                                         y <- y[-c(remove_cols),]
                                         y <- y %>%
                                           as.data.frame()%>%
                                           janitor::clean_names() %>%
                                           select(type,date,contains('20'))

                                         if(ncol(y) > 2){
                                         y<-y %>%
                                           pivot_longer(cols=!c(date,type),names_to='period',values_to='values') 
                                         return(y)
                                         }else{
                                           return(y)}
                                         y
                                         
                                       })
                           return(x)
                         })

FINAL_pesa_capital <- all_pesa_data2 %>% 
  flatten() %>%
  data.table::rbindlist(fill=T,idcol=TRUE) %>%
  mutate(date = str_extract(pattern='(200[0-9]|20[12][0-9]|2030)',date)) %>%
  filter(grepl('Capital budget',.id) == TRUE | 
           grepl('Resource DEL',.id) == TRUE) %>%
  filter(grepl('real terms',.id) == FALSE) %>%
  mutate(period = substr(period,2,5),
         planned_outrun = case_when(
           period >= date ~ 'Planned',
           TRUE ~ 'Outrun'
         )) %>%
  filter(grepl('Health',type) == TRUE |
           grepl('NHS',type) == TRUE) %>%
  mutate(
    measure = case_when(
      substr(.id,7,9) %in% c('1.3','1.5') ~ 'RDEL',
      TRUE ~ 'CDEL'),
    values = as.numeric(values)) %>%
  group_by(date,period,planned_outrun,measure) %>%
  summarise(values=sum(values,na.rm=T)) %>%
  ungroup()%>%
  group_by(period,planned_outrun,measure) %>%
  summarise(values = mean(values,na.rm=T))
