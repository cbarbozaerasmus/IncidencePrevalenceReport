library(IncidencePrevalence)
library(IncidencePrevalenceReport)
library(dplyr)
library(tidyr)
library(ggplot2)

# Example with mock data

cdm <- generate_mock_incidence_prevalence_db(sample_size = 50000,
                                             out_pre = 0.5)


cdm$person %>%
  glimpse()

cdm$observation_period %>%
  glimpse()

cdm$strata %>%
  glimpse()

cdm$outcome %>%
  glimpse()


#NORMAL

# Denominator populations

dpop <- collect_denominator_pops(cdm = cdm,
                                 study_start_date = as.Date("2008-01-01"),
                                 study_end_date = as.Date("2012-01-01"),
                                 study_age_stratas = list(c(18, 40)),
                                 study_sex_stratas = "Female",
                                 study_days_prior_history = 365,
                                 combine_subgroups = TRUE)


dpop <- dpop$denominator_population %>%
  collect() %>%
  left_join(dpop$denominator_settings)
#
dpop %>%
  glimpse()
#
# dpop %>%
#   group_by(cohort_definition_id, age_strata) %>% tally()

# Adding denominator population to cdm object

cdm$denominator <- dpop$denominator_population

# Prevalence calculation

prevalence <- collect_pop_prevalence(
  cdm = cdm,
  table_name_denominator = "denominator",
  cohort_ids_denominator_pops = "1",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  time_intervals="Years",
  type = "point",
  minimum_cell_count = 0
)

?collect_pop_prevalence
# Incidence calculation

incidence <- collect_pop_incidence(
  cdm = cdm,
  table_name_denominator = "denominator",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  cohort_ids_denominator_pops = "1",
  time_interval = c("Years"),
  outcome_washout_windows = 180
)

##### STUDY REPORT LAUNCH

title <- "Incidence Prevalence Report"
author <- "CESAR BARBOZA GUTIERREZ"
prevalence <- prevalence$prevalence_estimates
incidence <- incidence$incidence_estimates



### SEVERAL RANGES

cdm <- generate_mock_incidence_prevalence_db(sample_size = 50000,
                                             out_pre = 0.5)


dpop <- collect_denominator_pops(cdm = cdm,
                                  study_start_date = as.Date("2008-01-01"),
                                  study_end_date = as.Date("2016-01-01"),
                                  study_age_stratas = list(c(0,39),
                                                           c(40,59),
                                                           c(60,150)),
                                  study_sex_stratas = c("Male", "Female"),
                                  study_days_prior_history = 0)

cdm$denominator <- dpop$denominator_populations

prev <- collect_pop_prevalence(
  cdm = cdm,
  table_name_denominator = "denominator",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  cohort_ids_denominator_pops = c("1", "2", "3","4","5","6"),
  time_intervals="months",
  type = "period",
  minimum_cell_count = 0
)

prev$prevalence_estimates%>%
  glimpse()

prev$analysis_settings%>%
  glimpse()

prev$prevalence_estimates <- prev$prevalence_estimates %>%
  left_join(prev$analysis_settings,
            by = "prevalence_analysis_id")


dpop$denominator_settings%>%
  glimpse()


prev$prevalence_estimates <- prev$prevalence_estimates %>%
  left_join(dpop$denominator_settings,
          by=c("cohort_id_denominator_pop" = "cohort_definition_id"))

prev$prevalence_estimates%>%
  glimpse()

prev$prevalence_estimates %>%
  ggplot(aes(start_time, prev))+
  facet_grid(age_strata ~ sex_strata)+
  geom_point()+
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,NA)) +
  theme_bw()


prev$prevalence_estimates %>%
  left_join(prev$analysis_settings,
            by = "prevalence_analysis_id") %>%
  left_join(dpop$denominator_settings,
            by=c("cohort_id_denominator_pop" = "cohort_definition_id")) %>%
  ggplot(aes(start_time, prev))+
  facet_grid(age_strata ~ sex_strata)+
  geom_point()+
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,NA)) +
  theme_bw()


## RUN

reportIncidencePrevalence(title,
                          author,
                          prevalence,
                          incidence,
                          word = FALSE)
