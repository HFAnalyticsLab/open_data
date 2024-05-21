#urls
pesa_url <- 'https://www.gov.uk/government/collections/public-expenditure-statistical-analyses-pesa'
tac_url <- 'https://www.england.nhs.uk/financial-accounting-and-reporting/nhs-providers-tac-data-publications/'

tac_17_url <- c('https://webarchive.nationalarchives.gov.uk/ukgwa/20200327163023mp_/https://improvement.nhs.uk/documents/3302/All_TAC_data_published_in_NHS_trusts_accounts_for_2017-18.xlsx',
                'https://webarchive.nationalarchives.gov.uk/ukgwa/20200327163023mp_/https://improvement.nhs.uk/documents/3142/All_TAC_data_published_in_NHS_foundation_trusts_accounts_for_2017-18.xlsx')
tac_18_url <- c('https://webarchive.nationalarchives.gov.uk/ukgwa/20200327162132mp_/https://improvement.nhs.uk/documents/5905/TAC_data_published_in_NHS_trusts_accounts_for_2018-19.xlsx',
                'https://webarchive.nationalarchives.gov.uk/ukgwa/20200327162132mp_/https://improvement.nhs.uk/documents/5906/TAC_data_published_in_NHS_foundation_trusts_accounts_for_2018-19.xlsx')

tac_old_url <- 'https://www.gov.uk/government/collections/foundation-trust-consolidation-ftc-accounts-data'

rtt_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/'
#For whatever reason, they decided to bundle together multiple months in the same sheets,
#But unhelpfully, they did not do this in a predictable manner. So each .zip would have
#A different amount of months. Reading all of these and filtering would be a nightmare, so for now
#I've just decided to do it by hand.
gp_all_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice'
gp_url1 <- 'https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice/january-2019'
gp_url2 <- 'https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice/july-2021'
gp_url3 <- 'https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice/january-2024'
workforce_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics'
ae_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/'
community_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/community-services-statistics-for-children-young-people-and-adults'
op_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/hospital-outpatient-activity'
apc_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/provisional-monthly-hospital-episode-statistics-for-admitted-patient-care-outpatient-and-accident-and-emergency-data'
mh_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics'
msds_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/maternity-services-monthly-statistics'
overnight_beds_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-overnight/'
day_bed_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-day-only/'
op_th_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/cancelled-elective-operations/supporting-facilities-data/'
covid_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/'
diag_url <- ('https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/')
therapy_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/nhs-talking-therapies-for-anxiety-and-depression-annual-reports'
iapt_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/psychological-therapies-annual-reports-on-the-use-of-iapt-services'
eric_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/estates-returns-information-collection'
