source('src/functions.R')
source('const/global_var.R')
source('src/ae.R')
source('src/overnight_beds.R')

rm(list=ls()[!grepl("FINAL_", ls())])

#output data
FINAL_regression_data <- FINAL_ae_data %>%
  dplyr::mutate(quarter_date = zoo::as.yearqtr(date)) %>%
  dplyr::ungroup()%>%
  dplyr::select(-c(period_year,period_month,code,date)) %>%
  dplyr::group_by(quarter_date,org_code)%>%
  dplyr::summarise(across(starts_with('remergency'), sum, .names = "{.col}"))  %>%
  dplyr::left_join(.,FINAL_night_bed_data,by=c('quarter_date','org_code')) %>%
  dplyr::mutate(time_since_covid = quarter_date - zoo::as.yearqtr(make_date(year=2020,month=3,day=1)),
                covid_flag = case_when(
                  time_since_covid == 0 ~ 'covid',
                  time_since_covid < 0 ~ 'pre_covid',
                  time_since_covid > 0 ~ paste0(as.integer(time_since_covid),"_yrs_since_covid")
                ),
                type_1_breaches = remergency_breaches_type_1/remergency_type_1,
                type_1_admit_ratio = remergency_admissions_type_1/remergency_type_1,
                occupied_ratio = occupied_general_acute_beds/general_acute_beds) %>%
  tidyr::drop_na()

#formula for ae
ae_formula <- bf(
  # mu (mean) part
  type_1_breaches ~  type_1_admit_ratio + occupied_ratio + covid_flag + (1 | org_code) ,
  # phi (precision) part
  phi ~  covid_flag + (1 | org_code) ,
  # alpha (zero-inflation) part
  zoi ~ type_1_admit_ratio + occupied_ratio,
  coi ~ type_1_admit_ratio + occupied_ratio
)

#priors for brms?
brms::get_prior(
  ae_formula,
  data = FINAL_regression_data,
  family = zero_one_inflated_beta()
)

priors <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"))

#actual underlying model
ae_model <- brms::brm(
  ae_formula,
  data = FINAL_regression_data,
  family = zero_one_inflated_beta(),
  init = 0,
  prior = priors,
  control = list(adapt_delta = 0.97,
                 max_treedepth = 12),
  chains = 4, 
  iter = 2000, 
  warmup = 1000,
  cores = 4, 
  seed = 1234, 
  threads = threading(2),
  file = "ae_model"
)

ae_beta_1 <- ae_model %>%
  marginaleffects::avg_comparisons(variables = "occupied_ratio") 

ame_zi_1 <- ae_model %>% 
  marginaleffects::predictions(newdata = marginaleffects::datagrid(covid_flag = unique,
                                 occupied_beds = seq(0, 1, by = 0.1))) %>% 
  marginaleffects::posterior_draws() %>% 
  # Scale occupied_beds
  mutate(occupied_beds = occupied_beds * 100)

## OLD MODELLING -----

#model
ae_model_1 <- brm(
  #set up parameters: coi and zoi represents one and zero models respectively
  bf(type_1_breaches ~  type_1_admit_ratio + occupied_ratio,
     phi ~  type_1_admit_ratio + occupied_ratio,
     zoi ~ type_1_admit_ratio + occupied_ratio,
     coi ~ type_1_admit_ratio + occupied_ratio),
  data = ae_regression_data,
  #we have full occupancy and no occupancies, so zoib needed
  family = zero_one_inflated_beta(),
  init = 0,
  chains = 4, 
  iter = 1000, 
  warmup = 500,
  cores = 4, 
  seed = 1234, 
  #save because this takes ages to run
  file = "ae_model"
)

ae_beta_1 <- ae_model_1 %>%
  marginaleffects::avg_comparisons(variables = "occupied_ratio") 

beta_bayes_pred_1 <- model_beta_bayes_1 %>% 
  epred_draws(newdata = expand_grid(quota = FALSE,
                                    occupied = seq(0, 100, by = 1)))