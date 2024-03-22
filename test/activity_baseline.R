source('src/functions.R')
source('const/global_var.R')

#op not needed - in apc
#source('src/op.R')
source('src/community.R')
source('src/gp.R')
source('src/apc.R')
source('src/ae.R')
#source('src/mh.R')
#source('src/maternity.R')

rm(list=ls()[!grepl("FINAL_", ls())])

ae_group <- FINAL_ae_data %>%
  group_by(period) %>%
  summarise(
    ae_attendances = sum(ae_attendances,na.rm=T),
    ae_admissions = sum(ae_admissions,na.rm=T)
  )

apc_group <- FINAL_apc_data %>%
  select(period,
         apc_ordinary_episodes,
         apc_day_case_episodes,
         apc_emergency,
         outpatient_total_appointments,
         outpatient_attended_appointments) %>%
  group_by(period) %>%
  summarise_all(.,sum)

gp_group <- FINAL_gp_data %>%
  mutate(period = appt_date) %>%
  filter(appt_status!='DNA') %>%
  group_by(period) %>%
  summarise(appointments = sum(count,na.rm=T))

community_group <- FINAL_community_data %>%
  filter(org_code == 'ALL' | org_code == 'All') %>%
  filter(dimension %in% c('TotalReferrals','ReferralSource','TotalCareContacts',"Age_CareContactDate-Group")) %>%
  mutate(metric_type = case_when(
    dimension %in% c('TotalReferrals','ReferralSource') ~ 'community_referrals',
    dimension %in% c('TotalCareContacts',"Age_CareContactDate-Group") ~ 'community_contacts',
    TRUE ~ 'community_other')) %>%
  mutate(count = as.numeric(count)) %>%
  group_by(period,metric_type) %>%
  summarise(count = sum(count,na.rm=T)) %>%
  pivot_wider(.,names_from=metric_type,values_from=count)

maternity_group <- FINAL_msds_data %>%
  #filter(dimension == 'TotalDeliveries') %>%
  mutate(value = as.numeric(value),
         period = parse_date_time(period,orders=c('%m%Y','%b%y','%Y%M','%d%m%Y','%Y%m%d'))) %>%
  filter(dimension %in% c('TotalMothers','TotalBookings')) %>%
  group_by(org_level,period,org_code) %>%
  #note, something wrong here and double counting, so averaged to remove. check source
  summarise(value=mean(value,na.rm=T)) %>%
  group_by(org_level,period) %>%
  summarise(value = sum(value,na.rm=T)) %>%
  group_by(period) %>%
  summarise(mothers=mean(value,na.rm=T))

baseline_data <- 
  purrr::reduce(list(
    ae_group,
    apc_group,
    community_group,
    maternity_group), 
    dplyr::full_join, by = c('period'))

write.csv(baseline_data,'test/baseline_data.csv')
