#Read in the data
source('src/functions.R')
source('const/global_var.R')
source('src/ae.R')
source('src/overnight_beds.R')

#remove anything unnecessary
rm(list=ls()[!grepl("FINAL_", ls())])

#useful functions for plots
label_pp <- scales::label_number(accuracy = 1, scale = 100, 
                         suffix = " pp.", style_negative = "minus")
label_pp_tiny <- scales::label_number(accuracy = 0.01, scale = 100, 
                              suffix = " pp.", style_negative = "minus")
#Create regression data ----

#output data
FINAL_regression_data <- FINAL_ae_data %>%
  #need it quarterly to be able to join
  dplyr::mutate(quarter_date = zoo::as.yearqtr(date)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(period_year,period_month,code,date)) %>%
  dplyr::group_by(quarter_date,org_code)%>%
  #group by summarise
  dplyr::summarise(across(starts_with('remergency'), sum, .names = "{.col}"))  %>%
  dplyr::left_join(.,FINAL_night_bed_data,by=c('quarter_date','org_code')) %>%
  #create variables, covid_flag, time since covid, breaches, admit ratio and occupied
  dplyr::mutate(time_since_covid = quarter_date - zoo::as.yearqtr(lubridate::make_date(year=2020,month=3,day=1)),
                covid_flag = dplyr::case_when(
                  as.integer(time_since_covid) == 0 ~ 'covid',
                  as.integer(time_since_covid) < 0 ~ 'pre_covid',
                  as.integer(time_since_covid) > 0 ~ paste0(as.integer(time_since_covid),"_yrs_since_covid")
                ),
                type_1_breaches = remergency_breaches_type_1/remergency_type_1,
                type_1_admit_ratio = remergency_admissions_type_1/remergency_type_1,
                #note, multiply by 100 to enable easier interpretation. this is so confusing!!!
                occupied_ratio = 100*(occupied_general_acute_beds/general_acute_beds)) %>%
  #remove nas: should i constrain this dataset more?
  tidyr::drop_na() %>%
  #remove small superflous places (500 seems a good limit?)
  dplyr::filter(general_acute_beds >= 250)

#Model specification ----

#formula for ae
ae_formula <- brms::bf(
  # mu (mean) part
  type_1_breaches ~  type_1_admit_ratio + occupied_ratio + covid_flag + (1|org_code),
  # phi (precision) part
  phi ~  covid_flag + (1|org_code) ,
  # alpha (zero-inflation) part
  zoi ~ type_1_admit_ratio + occupied_ratio,
  coi ~ type_1_admit_ratio + occupied_ratio
)

#priors for brms?
brms::get_prior(
  ae_formula,
  data = FINAL_regression_data,
  family = brms::zero_one_inflated_beta()
)

#save priors
priors <- c(brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            brms::set_prior("normal(0, 1)", class = "b"))

#actual underlying model
ae_model <- brms::brm(
  ae_formula,
  data = FINAL_regression_data,
  #thinking this should be one-inflated / constrainted to > 50% occupied
  family = brms::zero_one_inflated_beta(),
  #initialise at 0 to speed up process
  init = 0,
  prior = priors,
  #q: is this depth enough? should i increase? no warning about convergence
  #so am sort of happy so far
  control = list(adapt_delta = 0.97,
                 max_treedepth = 12),
  chains = 4, 
  iter = 2000, 
  warmup = 1000,
  cores = 4, 
  seed = 1926, 
  #this is up to system limitation. my device is an M1, so it can really
  #only handle 4 threads max? 4 chains x 1 threads is enough. someone buy me
  #a better computer and I can do this more effectively
  threads = brms::threading(1),
  file = "ae_model2"
)

# Model outputs -----

#get average slopes through vars;
#how does this work exactly?
ae_beta_1 <- marginaleffects::avg_slopes(ae_model,
                                         variables = 'occupied_ratio',
                                         by = 'covid_flag')

ame_zi_1 <- marginaleffects::predictions(ae_model, 
                               newdata = marginaleffects::datagrid(
                                 covid_flag = unique,
                                 occupied_ratio = seq(0.5, 1, by = 0.01))) %>%
  marginaleffects::posterior_draws()

#average predictions
ame_zi_2 <- marginaleffects::avg_predictions(ae_model, 
                                             variables = 'occupied_ratio')

ggplot2::ggplot(ame_zi_1,aes(x = occupied_ratio, y = draw, color = covid_flag, fill = covid_flag)) +
  ggdist::stat_lineribbon(aes(fill_ramp = after_stat(level))) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  ggdist::scale_fill_ramp_discrete(range = c(0.2, 0.7)) +
  facet_wrap(vars(covid_flag), ncol = 3) +
  labs(x = "A&G Occupied beds ratio",
       y = "Predicted proportion of type 1 AE breaches",
       fill = "Covid Flag", color = "Covid Flag",
       fill_ramp = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom")

#model
ame_beta_bayes_1 <- ae_model %>% 
  marginaleffects::slopes(variables = "occupied_ratio",
         newdata = marginaleffects::datagrid(occupied_ratio = c(1,0.9,0.8,0.7))) %>% 
  marginaleffects::posterior_draws()

#plot of different tests
ggplot(ame_beta_bayes_1, aes(x = draw, fill = factor(occupied_ratio))) +
  geom_vline(xintercept = 0) +
  ggdist::stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_hdi",
               slab_alpha = 0.75) +
  scale_x_continuous(labels = label_pp_tiny) +
  scale_fill_viridis_d(option = "viridis", end = 0.6) +
  labs(x = "Average marginal effect of occupied beds", 
       y = "Density", fill = "Occupied bed rates",
       caption = "80% and 95% credible intervals shown in black") +
  theme(legend.position = "bottom")

#plotting occupied ratio preds
marginaleffects::plot_predictions(model = ae_model,
                                  condition = c('occupied_ratio'),
                                  type = 'average')

# Testing with effects:: package. I hate this.
marginaleffects::avg_slopes(ae_model)
