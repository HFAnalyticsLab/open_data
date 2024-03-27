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

baseline_budget <- read.csv('test/baseline/baseline_budget.csv') %>%
  rename('type'=Year)
  
cwa <- read.csv('test/baseline/cwa.csv') %>%
  pivot_wider(names_from=type,values_from=constant_prices) %>%
  rename(
    'ae_attendances' = ae_cost,
    'gp_appointments' = total_gp_cost,
    'apc_emergency' = apc_spell_cost_emergency,
    'outpatient_attended_appointments' = op_cost,
    'apc_spell_count_elective' = apc_spell_cost_elective
  ) %>%
  pivot_longer(!c(fyear),names_to='type',values_to='cwa')

baselines <- read.csv('test/baseline/data.csv') %>%
  select(!changing_rate)%>%
  pivot_wider(names_from=outcome,values_from=constant_2018) %>%
  rename(
    'ae_attendances' = ae_count,
    'gp_appointments' = gp_referrals_count,
    'apc_emergency' = apc_spell_count_emergency,
    'outpatient_attended_appointments' = op_count
  )

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
  filter(appt_status!='DNA') %>%
  group_by(period) %>%
  summarise(gp_appointments = sum(count,na.rm=T)) %>%
  mutate(period = lubridate::my(period))

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
    gp_group),
    #community_group,
    #maternity_group), 
    dplyr::full_join, by = c('period')) %>%
  dplyr::mutate(apc_spell_count_elective = apc_ordinary_episodes + apc_day_case_episodes) 

write.csv(baseline_data,'test/baseline_data.csv') 

### test -------

final_data <- baseline_data %>%
  ungroup()%>%
  filter(period < lubridate::make_date(year=2019,month=4,day=1) & period >= lubridate::make_date(year=2018,month=4,day=1)) %>%
  select(!period) %>%
  mutate(fyear = 2018) %>%
  group_by(fyear)%>%
  summarise(across(where(is.numeric), sum, .names = "{.col}")) %>%
  select(fyear,apc_emergency,ae_attendances,apc_spell_count_elective,outpatient_attended_appointments,gp_appointments) %>%
  pivot_longer(!c(fyear),names_to='type',values_to='baseline_2018')

all_data <- baseline_data %>%
  ungroup()%>%
  mutate(fyear = case_when(
    lubridate::month(period) <= 3 ~ lubridate::year(period) -1,
    TRUE ~ lubridate::year(period)
  )) %>%
  select(!period) %>%
  group_by(fyear)%>%
  summarise(across(where(is.numeric), sum, .names = "{.col}")) %>%
  select(fyear,apc_emergency,ae_attendances,apc_spell_count_elective,outpatient_attended_appointments,gp_appointments) %>%
  pivot_longer(!c(fyear),names_to='type',values_to='actual_values')

baselines2 <- baselines %>%
  pivot_longer(!c(year),names_to='type',values_to='activity_cum_growth') %>%
  filter(year >= 2018) %>%
  rename(fyear='year') %>%
  left_join(.,final_data %>% select(!fyear),by=c('type')) %>%
  mutate(predicted_activity = (activity_cum_growth/100)*baseline_2018) %>%
  left_join(.,all_data,by=c('type','fyear')) %>%
  left_join(.,cwa,by=c('type','fyear')) %>%
  left_join(.,baseline_budget,by='type') %>%
  mutate(predicted_budget = (cwa/100)*baseline_budget*(activity_cum_growth/100))

write.csv(baselines2,'test/baseline/predicted_cwa_budget.csv')
