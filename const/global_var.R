#urls
pesa_url <- 'https://www.gov.uk/government/collections/public-expenditure-statistical-analyses-pesa'
tac_url <- 'https://www.england.nhs.uk/financial-accounting-and-reporting/nhs-providers-tac-data-publications/'
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
