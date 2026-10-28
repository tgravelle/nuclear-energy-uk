rm(list=ls())
options(scipen=999)
select <- dplyr::select
vars <- dplyr::vars
coalesce <- dplyr::coalesce

getwd()

library(tidyverse)
library(haven)
library(survey)
library(brms)
library(cmdstanr)
library(bayesplot)
library(expss)
library(lavaan)
library(weights)
library(semTools)
library(modelsummary)
library(DescTools)
library(scales)
library(ggh4x)
library(openxlsx)

data.url <- "https://raw.githubusercontent.com/tgravelle/nuclear-energy-uk/refs/heads/main/UK_SMNR.csv"
data.0 <- read_csv(data.url)

data.0a <- readRDS("C:/Users/timgr/OneDrive - Wilfrid Laurier University/Academic/UK Nuclear Energy/Data/UK_SMNR.rds")

summary(data.0$weight)

tabs <- data.0 %>%
  tab_weight(weight = weight) %>%
  tab_total_row_position("above") %>%
  ## Column variables go below
  tab_cols(total(), condition) %>%
  ## Row variables go below
  tab_cells(CONSENT %to% BORN) %>% tab_stat_cpct() %>%  # column %
  tab_pivot() %>%
  as_tibble() %>%
  mutate_if(is.numeric, replace_na, replace = 0) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  mutate(Overall = `#Total`,
         row_labels = str_replace_all(row_labels, "#Total cases", "Total"),
         split_labels = str_split_fixed(row_labels, "\\|", n = 2),
         Question = split_labels[,1],
         Choice = split_labels[,2]
  ) %>%
  select(Question, Choice, Overall, starts_with("condition"))

print(tabs, n = Inf)

summary(data.0$nuclear_dist_km)
summary(data.0$ln_nuclear_dist_km)

data.1 <- data.0 %>%
  mutate(support_smr = case_when(
    SMNR_UK %in% 1:5 | SMNP_UK %in% 1:5 | SMNR_LOCAL %in% 1:5 | SMNP_LOCAL %in% 1:5 ~ 6 - coalesce(SMNR_UK, SMNP_UK, SMNR_LOCAL, SMNP_LOCAL),
    SMNR_UK == 6 | SMNP_UK == 6 | SMNR_LOCAL == 6 | SMNP_LOCAL == 6 ~ 3,
    TRUE ~ 3
  )) %>%
  mutate(support_smr_nr = case_when(
    SMNR_UK %in% 1:5 | SMNP_UK %in% 1:5 | SMNR_LOCAL %in% 1:5 | SMNP_LOCAL %in% 1:5 ~ 6 - coalesce(SMNR_UK, SMNP_UK, SMNR_LOCAL, SMNP_LOCAL),
    SMNR_UK == 6 | SMNP_UK == 6 | SMNR_LOCAL == 6 | SMNP_LOCAL == 6 ~ -9,
    TRUE ~ -9
  )) %>%
  mutate(RPS = fct_relevel(as_factor(RPS), "Power Station", "Reactor")) %>%
  mutate(LOC = fct_relevel(as_factor(LOC), "UK", "Local")) %>%
  mutate(Female = case_when(
    GENDER == 1 ~ 0,
    GENDER == 2 ~ 1,
    SEX_panel == "Male" ~ 0,
    SEX_panel == "Female" ~ 1,
    TRUE ~ 0.5
  )) %>%
  mutate(ln_age_years = log(AgeYears)) %>%
  mutate(ln_age_years = ln_age_years - weighted.mean(x = ln_age_years, w = weight)) %>%
  mutate(ln_nuclear_dist_km = ln_nuclear_dist_km - weighted.mean(x = ln_nuclear_dist_km, w = weight)) %>%
  mutate(ed = as_factor(case_when(
    EDUCATION %in% c(10, 11, 12, 17, 19, -99) ~ "O-level/GCSE/None",
    EDUCATION %in% c(7, 8) ~ "A-level",
    EDUCATION %in% c(3, 4, 5, 6, 9, 13, 14, 15, 16, 18) ~ "HND/HNC/trade",
    EDUCATION == 2 ~ "Bachelor degree",
    EDUCATION == 1 ~ "Postgraduate degree",
    TRUE ~ EDU_panel
  ))) %>%
  mutate(Educ = case_when(
    ed == "O-level/GCSE/None" ~ 0,
    ed == "A-level" ~ 0.25,
    ed == "HND/HNC/trade" ~ 0.5,
    ed == "Bachelor degree" ~ 0.75,
    ed == "Postgraduate degree" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(Educ = Educ - weighted.mean(x = Educ, w = weight)) %>%
  select(-ed) %>%
  mutate(Country = fct_relevel(COUNTRY, "England", "Wales", "Scotland", "Northern Ireland")) %>%
  mutate(CountryWales = case_when(
    Country == "Wales" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(CountryNorthernIreland = case_when(
    Country == "Northern Ireland" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(Region = fct_relevel(REGION, "London")) %>%
  mutate(mii_col = case_when(
    MII == 9 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(left_right = LEFT_RIGHT) %>%
  mutate(local_econ = 6 - LOCAL_ECON) %>%
  mutate(local_envt = 6 - LOCAL_ENVT) %>%
  mutate(elec_envt_friendly = 6 - ELECTRICITY_1) %>%
  mutate(need_more_elec = 6 - ELECTRICITY_2) %>%
  mutate(elec_self_suff = 6 - ELECTRICITY_3) %>%
  mutate(elec_price_low = 6 - ELECTRICITY_4) %>%
  mutate(cc_problem = 6 - CLIMATE_1) %>%
  mutate(envt_exagg_r = CLIMATE_2) %>%
  mutate(cc_action = 6 - CLIMATE_3) %>%
  mutate(cc_uncert_occ_r = CLIMATE_4) %>%
  mutate(cc_uncert_eff_r = CLIMATE_5) %>%
  mutate(follow_elec = 5 - FOLLOW_ELEC) %>%
  mutate(trust_westminster = 5 - TRUST_1) %>%
  mutate(trust_pm = 5 - TRUST_2) %>%
  mutate(trust_local_mp = 5 - TRUST_3) %>%
  mutate(trust_council = 5 - TRUST_13) %>%
  mutate(trust_elec_cos = 5 - TRUST_14) %>%
  mutate(trust_elec_supp = 5 - TRUST_15) %>%
  mutate(across(c(left_right:cc_uncert_eff_r), ~ case_when(
    .x %in% 1:5 ~ .x,
    TRUE ~ NA_real_
  ))) %>%
  mutate(across(c(follow_elec:trust_elec_supp), ~ case_when(
    .x %in% 1:4 ~ .x,
    TRUE ~ NA_real_
  ))) %>%
  mutate(vote_text = str_to_lower(VOTE_2024_TEXT)) %>%
  mutate(VOTE2024 = as_factor(case_when(
    VOTE_2024 == 2 ~ "Labour",
    VOTE_2024 == 1 ~ "Conservative",
    VOTE_2024 == 3 ~ "Liberal Democrat",
    VOTE_2024 == 13 ~ "Reform UK",
    VOTE_2024 == 4 ~ "Scottish National Party",
    VOTE_2024 == 6 ~ "Plaid Cymru",
    VOTE_2024 == 10 ~ "Nationalist",
    VOTE_2024 %in% c(8, 11, 12) ~ "Unionist",
    VOTE_2024 == 14 ~ "Other",
    VOTE_2024 %in% c(16, 17, 18, 19, 20) ~ "DNV",
    str_detect(vote_text, "for labour") == TRUE ~ "Labour",
    str_detect(vote_text, "voted labour") == TRUE ~ "Labour",
    str_detect(vote_text, "independent") == TRUE ~ "Other",
    str_detect(vote_text, "sdlp") == TRUE ~ "Nationalist",
    str_detect(vote_text, "spoiled") == TRUE ~ "DNV",
    str_detect(vote_text, "didn't") == TRUE ~ "DNV",
    vote_text != "-99" ~ "Other",
    TRUE ~ "DNV"
  ))) %>%
  mutate(VOTE2024 = fct_relevel(VOTE2024, "Labour", "Conservative", "Liberal Democrat", "Reform UK",
                                "Scottish National Party", "Plaid Cymru", "Nationalist", "Unionist")) %>%
  mutate(Scotland_Local = case_when(
    Country == "Scotland" & LOC == "Local" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(Scotland_Ind_Ref = as_factor(case_when(
    Country == "Scotland" & IND_SCOTLAND == 1 ~ "Yes",
    Country == "Scotland" ~ "Not yes",
    TRUE ~ "Outside Scotland"
  ))) %>%
  mutate(Scotland_Ind_Ref = fct_relevel(Scotland_Ind_Ref, "Outside Scotland", "Yes", "Not yes")) %>%
  mutate(Scotland_Vote_2024 = as_factor(case_when(
    Country == "Scotland" & VOTE2024 == "Scottish National Party" ~ "Scotland-SNP",
    Country == "Scotland" ~ "Scotland-Other",
    TRUE ~ "Outside Scotland"
  ))) %>%
  mutate(Scotland_Vote_2024 = fct_relevel(Scotland_Vote_2024, "Outside Scotland", "Scotland-SNP", "Scotland-Other")) %>%
  mutate(Scotland_PID = as_factor(case_when(
    Country == "Scotland" & PID == 4 ~ "Scotland-SNP",
    Country == "Scotland" ~ "Scotland-Other",
    TRUE ~ "Outside Scotland"
  ))) %>%
  mutate(Scotland_PID = fct_relevel(Scotland_PID, "Outside Scotland", "Scotland-SNP", "Scotland-Other"))

xtabs(~ condition, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ condition + Country, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ RPS, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ LOC, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ support_smr, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ support_smr_nr + Female, addNA = TRUE, data = data.1)
xtabs(~ support_smr_nr + Female, addNA = TRUE, data = data.1) %>% prop.table(margin = 2)*100

xtabs(~ Female, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ Educ, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ Country, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ Country, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ CountryWales, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ CountryNorthernIreland, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ Region, addNA = TRUE, na.action = NULL, data = data.1)

xtabs(~ local_econ, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ local_envt, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ elec_envt_friendly, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ need_more_elec, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ elec_self_suff, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ elec_price_low, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ cc_problem, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ envt_exagg_r, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ cc_action, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ cc_uncert_occ_r, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ cc_uncert_eff_r, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ follow_elec, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ trust_westminster, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ trust_pm, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ trust_local_mp, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ trust_council, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ trust_elec_cos, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ trust_elec_supp, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ left_right, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ left_right, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ VOTE2024, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ Scotland_Ind_Ref, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ Scotland_Vote_2024, addNA = TRUE, na.action = NULL, data = data.1)
xtabs(~ Scotland_PID, addNA = TRUE, na.action = NULL, data = data.1)

data.1 %>% group_by(SEX) %>% summarise(mean_weight = mean(weight))
data.1 %>% group_by(AGE) %>% summarise(mean_weight = mean(weight))
data.1 %>% group_by(EDU) %>% summarise(mean_weight = mean(weight))
data.1 %>% group_by(Country) %>% summarise(mean_weight = mean(weight))
data.1 %>% group_by(REGION) %>% summarise(mean_weight = mean(weight))
data.1 %>% group_by(VOTE2024) %>% summarise(mean_weight = mean(weight))
data.1 %>% filter(Country == "Scotland") %>% group_by(VOTE2024) %>% summarise(mean_weight = mean(weight))

wtd.cor(x = (select(data.1, local_econ, local_envt)),
        weight = data.1$weight) %>%
  pluck(., "correlation")

wtd.cor(x = (select(data.1, elec_envt_friendly, need_more_elec, elec_self_suff, elec_price_low)),
        weight = data.1$weight) %>%
  pluck(., "correlation")

wtd.cor(x = (select(data.1, support_smr, need_more_elec, elec_self_suff, elec_price_low)),
        weight = data.1$weight) %>%
  pluck(., "correlation")

wtd.cor(x = (select(data.1, cc_problem, envt_exagg_r, cc_action, cc_uncert_occ_r, cc_uncert_eff_r)),
        weight = data.1$weight) %>%
  pluck(., "correlation")

wtd.cor(x = (select(data.1, cc_problem, envt_exagg_r, cc_action, cc_uncert_occ_r)),
        weight = data.1$weight) %>%
  pluck(., "correlation")

wtd.cor(x = (select(data.1, trust_westminster, trust_pm, trust_local_mp, trust_council)),
        weight = data.1$weight) %>%
  pluck(., "correlation")

wtd.cor(x = (select(data.1, trust_elec_cos, trust_elec_supp)),
        weight = data.1$weight) %>%
  pluck(., "correlation")

svycralpha(~local_econ + local_envt,
           (svydesign(id = ~1, strata = ~condition, weights = ~weight, data = data.1)), na.rm = TRUE)

svycralpha(~need_more_elec + elec_self_suff + elec_price_low,
           (svydesign(id = ~1, strata = ~condition, weights = ~weight, data = data.1)), na.rm = TRUE)

svycralpha(~cc_problem + envt_exagg_r + cc_action + cc_uncert_occ_r,
           (svydesign(id = ~1, strata = ~condition, weights = ~weight, data = data.1)), na.rm = TRUE)

svycralpha(~trust_westminster + trust_pm + trust_local_mp + trust_council,
           (svydesign(id = ~1, strata = ~condition, weights = ~weight, data = data.1)), na.rm = TRUE)

svycralpha(~trust_elec_cos + trust_elec_supp,
           (svydesign(id = ~1, strata = ~condition, weights = ~weight, data = data.1)), na.rm = TRUE)

## CFA for trust, climate concern
cfa_model <-
  '
  trust_govt =~ NA*trust_westminster + trust_pm + trust_local_mp + trust_council
  trust_elec =~ A*trust_elec_supp + B*trust_elec_cos
  climate =~ NA*cc_problem + envt_exagg_r + cc_action + cc_uncert_occ_r
  A == B
  trust_govt ~~ 1*trust_govt
  climate ~~ 1*climate
  trust_local_mp ~~ trust_council
  trust_westminster ~~ trust_pm
  cc_problem ~~ cc_action
'

cfa_fit <- cfa(cfa_model, data = data.1, ordered = TRUE, estimator = "WLSMV", sampling.weights = "weight",
               missing = "pairwise", parameterization = "theta", mimic = "Mplus")
summary(cfa_fit, fit.measures = TRUE, standardized = TRUE)
modindices(cfa_fit, standardized = TRUE, maximum.number = 20, sort = TRUE)
residuals(cfa_fit, type = "cor")
compRelSEM(cfa_fit, ord.scale = TRUE)

range_std <- function(x) {
  x <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

range_centre <- function(x, w = NULL) {
  x <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  x <- x - weighted.mean(x, w, na.rm = TRUE)
}

data.2 <- data.1 %>%
  bind_cols(., (lavPredict(object = cfa_fit, type = "lv", method = "EBM", append.data = FALSE) %>%
                  as.data.frame() %>% as_tibble())) %>%
  mutate(support_smr = range_std(x = support_smr) - 0.5) %>%
  mutate(follow_elec = case_when(follow_elec %in% 1:4 ~ follow_elec, TRUE ~ 1)) %>%
  mutate(follow_elec = range_centre(x = follow_elec, w = weight)) %>%
  mutate(need_more_elec = case_when(need_more_elec %in% 1:5 ~ need_more_elec, TRUE ~ 3)) %>%
  mutate(need_more_elec = range_centre(x = need_more_elec, w = weight)) %>%
  mutate(elec_self_suff = case_when(elec_self_suff %in% 1:5 ~ elec_self_suff, TRUE ~ 3)) %>%
  mutate(elec_self_suff = range_centre(x = elec_self_suff, w = weight)) %>%
  mutate(elec_price_low = case_when(elec_price_low %in% 1:5 ~ elec_price_low, TRUE ~ 3)) %>%
  mutate(elec_price_low = range_centre(x = elec_price_low, w = weight)) %>%
  mutate(local_econ = case_when(local_econ %in% 1:5 ~ local_econ, TRUE ~ 3)) %>%
  mutate(local_econ = range_centre(x = local_econ, w = weight)) %>%
  mutate(local_envt = case_when(local_envt %in% 1:5 ~ local_envt, TRUE ~ 3)) %>%
  mutate(local_envt = range_centre(x = local_envt, w = weight)) %>%
  mutate(trust_govt = range_centre(x = trust_govt, w = weight)) %>%
  mutate(trust_elec = range_centre(x = trust_elec, w = weight)) %>%
  mutate(climate = range_centre(x = climate, w = weight)) %>%
  mutate(left_right = case_when(left_right %in% 1:5 ~ left_right, TRUE ~ 3)) %>%
  mutate(left_right = range_centre(x = left_right, w = weight))
  
summary(data.2$support_smr)
summary(data.2$ln_age_years)
summary(data.2$follow_elec)
summary(data.2$need_more_elec)
summary(data.2$elec_self_suff)
summary(data.2$elec_price_low)
summary(data.2$local_econ)
summary(data.2$local_envt)
summary(data.2$trust_govt)
summary(data.2$trust_elec)
summary(data.2$climate)
summary(data.2$left_right)
summary(data.2$ln_nuclear_dist_km)

data.2.nr <- data.2 %>%
  filter_out(support_smr_nr == -9)

summary(data.2.nr$support_smr)

## Regression models
mean_y <- weighted.mean(x = data.2$support_smr, weights = data.2$weight, na.rm = TRUE)
sd_y <- sqrt(Hmisc::wtd.var(data.2$support_smr, weights = data.2$weight, na.rm = TRUE))
range_age <- max(data.2$ln_age_years) - min(data.2$ln_age_years)
range_dist <- max(data.2$ln_nuclear_dist_km) - min(data.2$ln_nuclear_dist_km)

priors <- c(set_prior(sprintf("normal(%f, %f)", mean_y, sd_y), class = "Intercept"),
            set_prior(sprintf("normal(0, %f)", 0.5 * sd_y), class = "b"),
            set_prior(sprintf("exponential(%f)", 1 / sd_y), class = "sigma"),
            #set_prior(sprintf("normal(0, %f)", (0.5 * sd_y) / 1.564), class = "b", coef = "ln_age_years"),
            #set_prior(sprintf("normal(0, %f)", (0.5 * sd_y) / 5.612), class = "b", coef = "ln_nuclear_dist_km"),
            set_prior("normal(-0.0625, 0.05)", class = "b", coef = "RPSReactor"),
            set_prior("normal(-0.0625, 0.05)", class = "b", coef = "LOCLocal"))

m00 <- brm(formula = support_smr | weights(weight) ~ RPS + LOC + RPS*LOC,
           data = data.2,
           family = gaussian,
           prior = priors,
           backend = "cmdstanr",
           chains = 5,
           cores = 5,
           threads = 3,
           iter = 12000,
           warmup = 2000,
           refresh = 1000,
           seed = 20261008,
           save_pars = save_pars(all = TRUE))
print(m00, digits = 3)
prior_summary(m00)
m00 <- add_criterion(m00, "loo")

priors <- c(set_prior(sprintf("normal(%f, %f)", mean_y, sd_y), class = "Intercept"),
            set_prior(sprintf("normal(0, %f)", 0.5 * sd_y), class = "b"),
            set_prior(sprintf("exponential(%f)", 1 / sd_y), class = "sigma"),
            set_prior(sprintf("normal(0, %f)", (0.5 * sd_y) / range_age), class = "b", coef = "ln_age_years"),
            set_prior(sprintf("normal(0, %f)", (0.5 * sd_y) / range_dist), class = "b", coef = "ln_nuclear_dist_km"),
            set_prior("normal(-0.0625, 0.05)", class = "b", coef = "RPSReactor"),
            set_prior("normal(-0.0625, 0.05)", class = "b", coef = "LOCLocal"))

m0 <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
          + RPS + LOC + ln_nuclear_dist_km,
          data = data.2,
          family = gaussian,
          prior = priors,
          backend = "cmdstanr",
          chains = 5,
          cores = 5,
          threads = 3,
          iter = 12000,
          warmup = 2000,
          refresh = 1000,
          seed = 20261008,
          save_pars = save_pars(all = TRUE))
print(m0, digits = 3)
prior_summary(m0)
m0 <- add_criterion(m0, "loo")

m1 <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
          + RPS + LOC + ln_nuclear_dist_km
          + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right,
          data = data.2,
          family = gaussian,
          prior = priors,
          backend = "cmdstanr",
          chains = 5,
          cores = 5,
          threads = 3,
          iter = 12000,
          warmup = 2000,
          refresh = 1000,
          seed = 20261008,
          save_pars = save_pars(all = TRUE))
print(m1, digits = 3)
prior_summary(m1)
m1 <- add_criterion(m1, "loo")
posterior_m1 <- as_draws_df(m1)

mean(posterior_m1$b_RPSReactor >= 0)
mean(posterior_m1$b_RPSReactor < 0)
mean(posterior_m1$b_LOCLocal >= 0)
mean(posterior_m1$b_LOCLocal < 0)

m2 <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
          + RPS + LOC + ln_nuclear_dist_km
          + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right
          + LOC*Country,
          data = data.2,
          family = gaussian,
          prior = priors,
          backend = "cmdstanr",
          chains = 5,
          cores = 5,
          threads = 3,
          iter = 12000,
          warmup = 2000,
          refresh = 1000,
          seed = 20261008,
          save_pars = save_pars(all = TRUE))
print(m2, digits = 3)
prior_summary(m2)
m2 <- add_criterion(m2, "loo")
bayes_factor(m2, m1)   # Estimated Bayes factor in favor of m2 over m1: 10.12517
loo_compare(m1, m2)
posterior_m2 <- as_draws_df(m2)
mean(posterior_m2$b_LOCLocal < 0)

m3 <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
          + RPS + LOC + ln_nuclear_dist_km
          + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right
          + Scotland_Local,
          data = data.2,
          family = gaussian,
          prior = priors,
          backend = "cmdstanr",
          chains = 5,
          cores = 5,
          threads = 3,
          iter = 12000,
          warmup = 2000,
          refresh = 1000,
          seed = 20261008,
          save_pars = save_pars(all = TRUE))
print(m3, digits = 3)
prior_summary(m3)
m3 <- add_criterion(m3, "loo")
bayes_factor(m3, m1)   # Estimated Bayes factor in favor of m3 over m1: 34.751
loo_compare(m1, m3)
posterior_m3 <- as_draws_df(m3)
posterior_m3$b_LOCLocal_plus_Scotland_Local <- posterior_m3$b_LOCLocal + posterior_m3$b_Scotland_Local

mean(posterior_m3$b_RPSReactor >= 0)
mean(posterior_m3$b_RPSReactor < 0)

hypothesis(m3, "RPSReactor > 0")
mcmc_areas(posterior_m3, pars = "b_RPSReactor", prob = 0.95)

mean(posterior_m3$b_LOCLocal >= 0)
mean(posterior_m3$b_LOCLocal < 0)
hypothesis(m3, "LOCLocal < 0")
mcmc_areas(posterior_m3, pars = "b_LOCLocal", prob = 0.95)

mean(posterior_m3$b_Scotland_Local >= 0)
mean(posterior_m3$b_Scotland_Local < 0)
mean(posterior_m3$b_LOCLocal + posterior_m3$b_Scotland_Local < 0)
hypothesis(m3, "Scotland_Local < 0")
mcmc_areas(posterior_m3, pars = "b_Scotland_Local", prob = 0.95)
mcmc_areas(posterior_m3, pars = "b_LOCLocal_plus_Scotland_Local", prob = 0.95)

mean(posterior_m3$b_ln_nuclear_dist_km <= 0)
mean(posterior_m3$b_ln_nuclear_dist_km > 0)
hypothesis(m3, "ln_nuclear_dist_km > 0")
mcmc_areas(posterior_m3, pars = "b_ln_nuclear_dist_km", prob = 0.95)

m4 <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
          + RPS + LOC + ln_nuclear_dist_km
          + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right
          + LOC*ln_nuclear_dist_km,
          data = data.2,
          family = gaussian,
          prior = priors,
          backend = "cmdstanr",
          chains = 5,
          cores = 5,
          threads = 3,
          iter = 12000,
          warmup = 2000,
          refresh = 1000,
          seed = 20261008,
          save_pars = save_pars(all = TRUE))
print(m4, digits = 3)
prior_summary(m4)
m4 <- add_criterion(m4, "loo")
bayes_factor(m4, m1)   # Estimated Bayes factor in favor of m4 over m1: 0.15421
loo_compare(m1, m4)
posterior_m4 <- as_draws_df(m4)

m5 <- brm(formula = support_smr | weights(weight) ~ CountryWales + CountryNorthernIreland + Female + ln_age_years + Educ
           + RPS + LOC + ln_nuclear_dist_km
           + Scotland_Ind_Ref
           + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right,
           data = data.2,
           family = gaussian,
           prior = priors,
           backend = "cmdstanr",
           chains = 5,
           cores = 5,
           threads = 3,
           iter = 12000,
           warmup = 2000,
           refresh = 1000,
           seed = 20261008,
           save_pars = save_pars(all = TRUE))
print(m5, digits = 3)
prior_summary(m5)
m5 <- add_criterion(m5, "loo")
bayes_factor(m5, m1)   # Estimated Bayes factor in favor of m5 over m1: 364618.719
loo_compare(m1, m5)
posterior_m5 <- as_draws_df(m5)
mean(posterior_m5$b_Scotland_Ind_RefYes < 0)

m6 <- brm(formula = support_smr | weights(weight) ~ CountryWales + CountryNorthernIreland + Female + ln_age_years + Educ
           + RPS + LOC + ln_nuclear_dist_km
           + Scotland_Vote_2024
           + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right,
           data = data.2,
           family = gaussian,
           prior = priors,
           backend = "cmdstanr",
           chains = 5,
           cores = 5,
           threads = 3,
           iter = 12000,
           warmup = 2000,
           refresh = 1000,
           seed = 20261008,
           save_pars = save_pars(all = TRUE))
print(m6, digits = 3)
prior_summary(m6)
m6 <- add_criterion(m6, "loo")
bayes_factor(m6, m1)   # Estimated Bayes factor in favor of m6 over m1: 145.419
loo_compare(m1, m6)
posterior_m6 <- as_draws_df(m6)
mean(posterior_m6$b_Scotland_Vote_2024ScotlandMSNP < 0)

m7 <- brm(formula = support_smr | weights(weight) ~ CountryWales + CountryNorthernIreland + Female + ln_age_years + Educ
           + RPS + LOC + ln_nuclear_dist_km
           + Scotland_PID
           + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right,
           data = data.2,
           family = gaussian,
           prior = priors,
           backend = "cmdstanr",
           chains = 5,
           cores = 5,
           threads = 3,
           iter = 12000,
           warmup = 2000,
           refresh = 1000,
           seed = 20261008,
           save_pars = save_pars(all = TRUE))
print(m7, digits = 3)
prior_summary(m7)
m7 <- add_criterion(m7, "loo")
bayes_factor(m7, m1)   # Estimated Bayes factor in favor of m7 over m1: 8.076
loo_compare(m1, m7)
posterior_m7 <- as_draws_df(m7)
mean(posterior_m7$b_Scotland_PIDScotlandMSNP < 0)


## Regression models with item non-response on DV dropped
mean_y <- weighted.mean(x = data.2.nr$support_smr, weights = data.2.nr$weight, na.rm = TRUE)
sd_y <- sqrt(Hmisc::wtd.var(data.2.nr$support_smr, weights = data.2.nr$weight, na.rm = TRUE))
range_age <- max(data.2.nr$ln_age_years) - min(data.2.nr$ln_age_years)
range_dist <- max(data.2.nr$ln_nuclear_dist_km) - min(data.2.nr$ln_nuclear_dist_km)

priors <- c(set_prior(sprintf("normal(%f, %f)", mean_y, sd_y), class = "Intercept"),
            set_prior(sprintf("normal(0, %f)", 0.5 * sd_y), class = "b"),
            set_prior(sprintf("exponential(%f)", 1 / sd_y), class = "sigma"),
            #set_prior(sprintf("normal(0, %f)", (0.5 * sd_y) / 1.564), class = "b", coef = "ln_age_years"),
            #set_prior(sprintf("normal(0, %f)", (0.5 * sd_y) / 5.612), class = "b", coef = "ln_nuclear_dist_km"),
            set_prior("normal(-0.0625, 0.05)", class = "b", coef = "RPSReactor"),
            set_prior("normal(-0.0625, 0.05)", class = "b", coef = "LOCLocal"))

m00.nr <- brm(formula = support_smr | weights(weight) ~ RPS + LOC + RPS*LOC,
                 data = data.2.nr,
                 family = gaussian,
                 prior = priors,
                 backend = "cmdstanr",
                 chains = 5,
                 cores = 5,
                 threads = 3,
                 iter = 12000,
                 warmup = 2000,
                 refresh = 1000,
                 seed = 20261008,
                 save_pars = save_pars(all = TRUE))
print(m00.nr, digits = 3)
prior_summary(m00.nr)
m00.nr <- add_criterion(m00.nr, "loo")

priors <- c(set_prior(sprintf("normal(%f, %f)", mean_y, sd_y), class = "Intercept"),
            set_prior(sprintf("normal(0, %f)", 0.5 * sd_y), class = "b"),
            set_prior(sprintf("exponential(%f)", 1 / sd_y), class = "sigma"),
            set_prior(sprintf("normal(0, %f)", (0.5 * sd_y) / range_age), class = "b", coef = "ln_age_years"),
            set_prior(sprintf("normal(0, %f)", (0.5 * sd_y) / range_dist), class = "b", coef = "ln_nuclear_dist_km"),
            set_prior("normal(-0.0625, 0.05)", class = "b", coef = "RPSReactor"),
            set_prior("normal(-0.0625, 0.05)", class = "b", coef = "LOCLocal"))

m0.nr <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
             + RPS + LOC + ln_nuclear_dist_km,
             data = data.2.nr,
             family = gaussian,
             prior = priors,
             backend = "cmdstanr",
             chains = 5,
             cores = 5,
             threads = 3,
             iter = 12000,
             warmup = 2000,
             refresh = 1000,
             seed = 20261008,
             save_pars = save_pars(all = TRUE))
print(m0.nr, digits = 3)
prior_summary(m0.nr)
m0.nr <- add_criterion(m0.nr, "loo")

m1.nr <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
             + RPS + LOC + ln_nuclear_dist_km
             + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right,
             data = data.2.nr,
             family = gaussian,
             prior = priors,
             backend = "cmdstanr",
             chains = 5,
             cores = 5,
             threads = 3,
             iter = 12000,
             warmup = 2000,
             refresh = 1000,
             seed = 20261008,
             save_pars = save_pars(all = TRUE))
print(m1.nr, digits = 3)
prior_summary(m1.nr)
m1.nr <- add_criterion(m1.nr, "loo")
posterior_m1.nr <- as_draws_df(m1.nr)

mean(posterior_m1.nr$b_RPSReactor >= 0)
mean(posterior_m1.nr$b_RPSReactor < 0)
mean(posterior_m1.nr$b_LOCLocal >= 0)
mean(posterior_m1.nr$b_LOCLocal < 0)

m2.nr <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
             + RPS + LOC + ln_nuclear_dist_km
             + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right
             + LOC*Country,
             data = data.2.nr,
             family = gaussian,
             prior = priors,
             backend = "cmdstanr",
             chains = 5,
             cores = 5,
             threads = 3,
             iter = 12000,
             warmup = 2000,
             refresh = 1000,
             seed = 20261008,
             save_pars = save_pars(all = TRUE))
print(m2.nr, digits = 3)
prior_summary(m2.nr)
m2.nr <- add_criterion(m2.nr, "loo")
bayes_factor(m2.nr, m1.nr)   # Estimated Bayes factor in favor of m2.nr over m1.nr: 9.53820
loo_compare(m1.nr, m2.nr)
posterior_m2.nr <- as_draws_df(m2.nr)
mean(posterior_m2.nr$b_LOCLocal < 0)

m3.nr <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
             + RPS + LOC + ln_nuclear_dist_km
             + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right
             + Scotland_Local,
             data = data.2.nr,
             family = gaussian,
             prior = priors,
             backend = "cmdstanr",
             chains = 5,
             cores = 5,
             threads = 3,
             iter = 12000,
             warmup = 2000,
             refresh = 1000,
             seed = 20261008,
             save_pars = save_pars(all = TRUE))
print(m3.nr, digits = 3)
prior_summary(m3.nr)
m3.nr <- add_criterion(m3.nr, "loo")
bayes_factor(m3.nr, m1.nr)   # Estimated Bayes factor in favor of m3.nr over m1.nr: 30.339
loo_compare(m1.nr, m3.nr)
posterior_m3.nr <- as_draws_df(m3.nr)
posterior_m3.nr$b_LOCLocal_plus_Scotland_Local <- posterior_m3.nr$b_LOCLocal + posterior_m3.nr$b_Scotland_Local

mean(posterior_m3.nr$b_RPSReactor >= 0)
mean(posterior_m3.nr$b_RPSReactor < 0)

hypothesis(m3.nr, "RPSReactor > 0")
mcmc_areas(posterior_m3.nr, pars = "b_RPSReactor", prob = 0.95)

mean(posterior_m3.nr$b_LOCLocal >= 0)
mean(posterior_m3.nr$b_LOCLocal < 0)
hypothesis(m3.nr, "LOCLocal < 0")
mcmc_areas(posterior_m3.nr, pars = "b_LOCLocal", prob = 0.95)

mean(posterior_m3.nr$b_Scotland_Local >= 0)
mean(posterior_m3.nr$b_Scotland_Local < 0)
mean(posterior_m3.nr$b_LOCLocal + posterior_m3.nr$b_Scotland_Local < 0)
hypothesis(m3.nr, "Scotland_Local < 0")
mcmc_areas(posterior_m3.nr, pars = "b_Scotland_Local", prob = 0.95)
mcmc_areas(posterior_m3.nr, pars = "b_LOCLocal_plus_Scotland_Local", prob = 0.95)

mean(posterior_m3.nr$b_ln_nuclear_dist_km <= 0)
mean(posterior_m3.nr$b_ln_nuclear_dist_km > 0)
hypothesis(m3.nr, "ln_nuclear_dist_km > 0")
mcmc_areas(posterior_m3.nr, pars = "b_ln_nuclear_dist_km", prob = 0.95)

m4.nr <- brm(formula = support_smr | weights(weight) ~ Country + Female + ln_age_years + Educ
             + RPS + LOC + ln_nuclear_dist_km
             + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right
             + LOC*ln_nuclear_dist_km,
             data = data.2.nr,
             family = gaussian,
             prior = priors,
             backend = "cmdstanr",
             chains = 5,
             cores = 5,
             threads = 3,
             iter = 12000,
             warmup = 2000,
             refresh = 1000,
             seed = 20261008,
             save_pars = save_pars(all = TRUE))
print(m4.nr, digits = 3)
prior_summary(m4.nr)
m4.nr <- add_criterion(m4.nr, "loo")
bayes_factor(m4.nr, m1.nr)   # Estimated Bayes factor in favor of m4.nr over m1.nr: 0.157
loo_compare(m1.nr, m4.nr)
posterior_m4.nr <- as_draws_df(m4.nr)

m5.nr <- brm(formula = support_smr | weights(weight) ~ CountryWales + CountryNorthernIreland + Female + ln_age_years + Educ
             + RPS + LOC + ln_nuclear_dist_km
             + Scotland_Ind_Ref
             + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right,
             data = data.2.nr,
             family = gaussian,
             prior = priors,
             backend = "cmdstanr",
             chains = 5,
             cores = 5,
             threads = 3,
             iter = 12000,
             warmup = 2000,
             refresh = 1000,
             seed = 20261008,
             save_pars = save_pars(all = TRUE))
print(m5.nr, digits = 3)
prior_summary(m5.nr)
m5.nr <- add_criterion(m5.nr, "loo")
bayes_factor(m5.nr, m1.nr)   # Estimated Bayes factor in favor of m5.nr over m1.nr: 748704.383
loo_compare(m1.nr, m5.nr)
posterior_m5.nr <- as_draws_df(m5.nr)
mean(posterior_m5.nr$b_Scotland_Ind_RefYes < 0)

m6.nr <- brm(formula = support_smr | weights(weight) ~ CountryWales + CountryNorthernIreland + Female + ln_age_years + Educ
             + RPS + LOC + ln_nuclear_dist_km
             + Scotland_Vote_2024
             + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right,
             data = data.2.nr,
             family = gaussian,
             prior = priors,
             backend = "cmdstanr",
             chains = 5,
             cores = 5,
             threads = 3,
             iter = 12000,
             warmup = 2000,
             refresh = 1000,
             seed = 20261008,
             save_pars = save_pars(all = TRUE))
print(m6.nr, digits = 3)
prior_summary(m6.nr)
m6.nr <- add_criterion(m6.nr, "loo")
bayes_factor(m6.nr, m1.nr)   # Estimated Bayes factor in favor of m6.nr over m1.nr: 250.056
loo_compare(m1.nr, m6.nr)
posterior_m6.nr <- as_draws_df(m6.nr)
mean(posterior_m6.nr$b_Scotland_Vote_2024ScotlandMSNP < 0)

m7.nr <- brm(formula = support_smr | weights(weight) ~ CountryWales + CountryNorthernIreland + Female + ln_age_years + Educ
             + RPS + LOC + ln_nuclear_dist_km
             + Scotland_PID
             + follow_elec + need_more_elec + elec_self_suff + elec_price_low + trust_govt + trust_elec + climate + left_right,
             data = data.2.nr,
             family = gaussian,
             prior = priors,
             backend = "cmdstanr",
             chains = 5,
             cores = 5,
             threads = 3,
             iter = 12000,
             warmup = 2000,
             refresh = 1000,
             seed = 20261008,
             save_pars = save_pars(all = TRUE))
print(m7.nr, digits = 3)
prior_summary(m7.nr)
m7.nr <- add_criterion(m7.nr, "loo")
bayes_factor(m7.nr, m1.nr)   # Estimated Bayes factor in favor of m7.nr over m1.nr: 11.30639
loo_compare(m1.nr, m7.nr)
posterior_m7.nr <- as_draws_df(m7.nr)
mean(posterior_m7.nr$b_Scotland_PIDScotlandMSNP < 0)

#save(m00, m0, m1, m2, m3, m4, m5, m6, m7,
#     m00.nr, m0.nr, m1.nr, m2.nr, m3.nr, m4.nr, m5.nr, m6.nr, m7.nr, file = "./uk_smr_models.rda")


## Model summaries; save out results to Excel and create LaTeX tables
bayes_rmse <- function(fit) {
  y_obs <- brms::get_y(fit)
  preds <- brms::posterior_epred(fit) 
  rmse_samples <- apply(preds, 1, function(pred) {
    sqrt(mean((y_obs - pred)^2))
  })
  tibble::tibble(
    term      = "RMSE",
    Estimate  = mean(rmse_samples),
    Est.Error = sd(rmse_samples)
  )
}

model.out <- function(model_obj){
  old_opts <- options(warn = -1)
  on.exit(options(old_opts), add = TRUE)
  model_name <- deparse(substitute(model_obj))
  params <- summary(model_obj) %>%
    pluck(., "fixed") %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    rename(term = rowname,
           CrI.low = `l-95% CI`,
           CrI.high = `u-95% CI`)
  nobs <- nrow(model_obj$data) %>%
    as_tibble() %>%
    mutate(Estimate = value,
           term = "n") %>%
    select(term, Estimate)
  R2 <- bayes_R2(model_obj) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    rename(term = rowname,
           CrI.low = Q2.5,
           CrI.high = Q97.5)
  loo <- model_obj %>%
    pluck(., "criteria", "loo", "estimates") %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    rename(term = rowname,
           Est.Error = SE) %>%
    filter(term %in% c("elpd_loo", "looic")) %>%
    select(term, Estimate, Est.Error)
  RMSE <- bayes_rmse(model_obj)
  sigma <- broom.mixed::tidy(model_obj) %>%
    filter(term == "sd__Observation") %>%
    mutate(term = "Sigma") %>%
    rename(Estimate = estimate,
           Est.Error = std.error,
           CrI.low = conf.low,
           CrI.high = conf.high) %>%
    select(term, Estimate, Est.Error, CrI.low, CrI.high)
  model_out <- bind_rows(params, nobs, R2, loo, sigma, RMSE) %>%
    mutate(model = as.character(model_name)) %>%
    mutate(across(c(Estimate, Est.Error, CrI.low, CrI.high, Rhat), ~sprintf("%.3f", round(.x, digits = 3)))) %>%
    mutate(across(c(Bulk_ESS, Tail_ESS), ~round(.x, digits = 0))) %>%
    mutate_if(is.character, ~str_replace_all(.x, c("NA"), NA_character_)) %>%
    mutate_if(is.character, ~str_replace_all(.x, c("(NA)"), NA_character_)) %>%
    mutate(Est.Error = case_when(
      is.na(Est.Error) == FALSE ~ str_c("(", Est.Error, ")")
    )) %>%
    mutate(CrI = case_when(
      is.na(CrI.low) == FALSE & is.na(CrI.high) == FALSE ~ str_c("[", CrI.low, ", ", CrI.high, "]")
    )) %>%
    select(model, term, Estimate, Est.Error, CrI, Rhat, Bulk_ESS, Tail_ESS)
  print(model_out, quote = FALSE, right = TRUE, n = 200)
  model_results <- model_out %>% select(model:CrI)
  assign(paste0("summ_", model_name), model_results, envir = .GlobalEnv)
  openxlsx::addWorksheet(work_book, sheetName = model_name)
  openxlsx::writeData(work_book, model_name, model_out)
}

## Export results for Table A1
work_book <- createWorkbook()
model.out(m00)
model.out(m0)
model.out(m1)
model.out(m2)
model.out(m3)
model.out(m4)
model.out(m5)
model.out(m6)
model.out(m7)
model.out(m00.nr)
model.out(m0.nr)
model.out(m1.nr)
model.out(m2.nr)
model.out(m3.nr)
model.out(m4.nr)
model.out(m5.nr)
model.out(m6.nr)
model.out(m7.nr)
saveWorkbook(work_book, "./appendix/Bayesian models-nuclear energy-2026 10 28.xlsx", overwrite = TRUE)

summ_m3 <- summ_m3 %>% mutate(term = case_when(term == "Scotland_Local" ~ "CountryScotland:LOCLocal", TRUE ~ term))
summ_m3.nr <- summ_m3.nr %>% mutate(term = case_when(term == "Scotland_Local" ~ "CountryScotland:LOCLocal", TRUE ~ term))

## Table A1
summ_models_a1 <- bind_rows(summ_m0, summ_m1, summ_m2, summ_m3, summ_m4)

model_levels <- c("m0", "m1", "m2", "m3", "m4")
model_labels <- c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4")

fit_terms  <- c("n", "R2", "elpd_loo", "looic", "Sigma", "RMSE")
fit_labels <- c(n = "n", R2 = "Bayesian R2", elpd_loo = "ELPD",
                looic = "LOOIC", Sigma = "Sigma", RMSE = "RMSE")

stat_cols <- c("Estimate", "Est.Error", "CrI")

model_groups <- list(c("m0", "m1"), c("m2", "m3", "m4"))
table_captions <- c("Results: Linear models",
                    "Results: Linear models (continued)")
table_labels   <- c("linear-models", NA)   # knitr adds "tab:"; NA = no label

term_spec <- tribble(
  ~term,                              ~label,                                                    ~indent,
  "Intercept",                        "Intercept",                                               FALSE,
  NA,                                 "Country (Ref. = England)",                                FALSE,
  "CountryWales",                     "Wales",                                                   TRUE,
  "CountryScotland",                  "Scotland",                                                TRUE,
  "Scotland_Ind_RefYes",              "Scotland--Independence referendum: Yes",                  TRUE,
  "Scotland_Ind_RefNotyes",           "Scotland--Independence referendum: Not yes",              TRUE,
  "Scotland_Vote_2024ScotlandMSNP",   "Scotland--2024 vote: SNP",                                TRUE,
  "Scotland_Vote_2024ScotlandMOther", "Scotland--2024 vote: Not SNP",                            TRUE,
  "Scotland_PIDScotlandMSNP",         "Scotland--Party ID: SNP",                                 TRUE,
  "Scotland_PIDScotlandMOther",       "Scotland--Party ID: Not SNP",                             TRUE,
  "CountryNorthernIreland",           "Northern Ireland",                                        TRUE,
  "Female",                           "Female",                                                  FALSE,
  "ln_age_years",                     "Age (Years logged)",                                      FALSE,
  "Educ",                             "Education",                                               FALSE,
  NA,                                 "Power source frame (Ref. = power station)",               FALSE,
  "RPSReactor",                       "Reactor frame",                                           TRUE,
  NA,                                 "Location frame (Ref. = United Kingdom)",                  FALSE,
  "LOCLocal",                         "Local frame",                                             TRUE,
  "ln_nuclear_dist_km",               "Distance to nearest nuclear site (km logged)",            FALSE,
  "follow_elec",                      "Follow news about electricity",                           FALSE,
  "need_more_elec",                   "UK needs to produce a lot more electricity",              FALSE,
  "elec_self_suff",                   "UK should be self-sufficient in electricity",             FALSE,
  "elec_price_low",                   "Critical to keep the price of electricity in the UK low", FALSE,
  "trust_govt",                       "Trust in government (factor score)",                      FALSE,
  "trust_elec",                       "Trust in electricity companies/suppliers (factor score)", FALSE,
  "climate",                          "Concern with climate change (factor score)",              FALSE,
  "left_right",                       "Ideology (left\u2013right)",                              FALSE,
  "CountryWales:LOCLocal",            "Wales \u00d7 Local frame",                                FALSE,
  "CountryScotland:LOCLocal",         "Scotland \u00d7 Local frame",                             FALSE,
  "CountryNorthernIreland:LOCLocal",  "Northern Ireland \u00d7 Local frame",                     FALSE,
  "LOCLocal:ln_nuclear_dist_km",      "Distance to nearest nuclear site \u00d7 Local frame",     FALSE,
  "LOCLocal:ln_nuclear_dist_km",      "Distance to nearest nuclear site \u00d7 Local frame",     FALSE
)

unmatched_in_data <- setdiff(unique(summ_models_a1$term), c(term_spec$term, fit_terms))
unmatched_in_spec <- setdiff(na.omit(term_spec$term), unique(summ_models_a1$term))
if (length(unmatched_in_data) > 0)
  warning("Terms in data but missing from term_spec (will be DROPPED): ",
          paste(unmatched_in_data, collapse = ", "))
if (length(unmatched_in_spec) > 0)
  warning("Terms in term_spec that match nothing in the data: ",
          paste(unmatched_in_spec, collapse = ", "))

build_table <- function(mods, caption, label) {
  labs  <- model_labels[match(mods, model_levels)]
  n_mod <- length(mods)
  
  col_order <- as.vector(t(outer(labs, stat_cols, paste, sep = "__")))
  
  wide_tab <- summ_models_a1 %>%
    filter(model %in% mods) %>%
    mutate(model   = factor(model, model_levels, model_labels),
           section = if_else(term %in% fit_terms, "fit", "coef")) %>%
    select(model, section, term, all_of(stat_cols)) %>%
    pivot_wider(names_from  = model,
                values_from = all_of(stat_cols),
                names_glue  = "{model}__{.value}") %>%
    select(section, term, any_of(col_order))
  
  coef_tab <- term_spec %>%
    left_join(filter(wide_tab, section == "coef") %>% select(-section),
              by = "term", na_matches = "never") %>%
    filter(is.na(term) | if_any(-c(term, label, indent), ~ !is.na(.x))) %>%
    mutate(term = label)
  
  indent_rows <- which(coef_tab$indent)
  coef_tab    <- coef_tab %>% select(-label, -indent)
  
  fit_tab <- wide_tab %>%
    filter(section == "fit") %>%
    select(-section) %>%
    arrange(match(term, fit_terms)) %>%
    mutate(across(ends_with("__Estimate"),
                  ~ if_else(term == "n" & !is.na(.x),
                            formatC(suppressWarnings(as.numeric(.x)),
                                    format = "d", big.mark = ","),
                            .x))) %>%
    mutate(term = unname(fit_labels[term]))
  
  tab_df <- bind_rows(coef_tab, fit_tab) %>%
    mutate(across(everything(), ~ replace_na(as.character(.x), ""))) %>%
    mutate(across(-term, ~ gsub("-", "$-$", .x, fixed = TRUE))) %>%
    mutate(term = term %>%
             gsub("\u00d7", "$\\times$", ., fixed = TRUE) %>%
             gsub("\u2013", "--", ., fixed = TRUE) %>%
             gsub("Bayesian R2", "Bayes \\textit{R}\\textsuperscript{2}", ., fixed = TRUE))
  
  stopifnot(ncol(tab_df) == 1 + 3 * n_mod)
  
  tab_df %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE,
        escape = FALSE, linesep = "",
        caption = caption, label = label,
        align = c("l", rep(c("r", "r", "c"), n_mod)),
        col.names = c("", rep(c("\\textbf{Estimate}", "\\textbf{Est. Err.}",
                                "\\textbf{95\\% CrI}"), n_mod))) %>%
    add_header_above(c(" " = 1,
                       setNames(rep(3, n_mod), paste0("\\textbf{", labs, "}"))),
                     escape = FALSE) %>%
    add_indent(indent_rows) %>%
    row_spec(nrow(coef_tab), extra_latex_after = "\\midrule") %>%  # rule above fit stats
    column_spec(1, width = "2.5in") %>%
    kable_styling(latex_options = "repeat_header") %>%
    as.character()
}

note <- paste(
  "Models are linear models fit by Bayesian estimation in R (version 4.6.1) using brms",
  "(version 2.23.0) with the cmdstanr backend (Stan 2.40). Each model used 5 chains",
  "with 2,000 warmup and 10,000 post-warmup iterations per chain. Priors were weakly",
  "informative (normal) priors on all regression coefficients and default weakly",
  "informative priors on the model intercept [Student-t(3, 0.2, 2.5)] and the residual",
  "standard deviation (sigma) [Student-t(3, 0, 2.5)]. Estimates are posterior means with",
  "standard deviations in parentheses (Est. Error) and 95\\% equal-tailed credible",
  "intervals in brackets. All Rhat values were $\\leq$ 1.001 and all bulk and tail",
  "effective sample sizes exceeded 37,000 with no divergent transitions."
)

tables <- pmap_chr(list(model_groups, table_captions, table_labels),
                   ~ build_table(..1, ..2, ..3))

tex <- c("\\begin{landscape}",
         "\\renewcommand{\\thetable}{A\\arabic{table}}",
         "\\scriptsize",
         "\\setlength{\\tabcolsep}{2pt}",
         "\\renewcommand{\\arraystretch}{0.96}",
         paste(tables, collapse = "\n\n"),
         "",
         "\\vspace{-6pt}",
         "\\begin{minipage}{\\linewidth}",
         "\\setlength{\\parskip}{0pt}\\scriptsize",
         note,
         "\\end{minipage}",
         "\\end{landscape}")

writeLines(tex, "./manuscript/table_a1.tex")


## Table A2
summ_models_a2 <- bind_rows(summ_m5, summ_m6, summ_m7)

model_levels <- c("m5", "m6", "m7")
model_labels <- c("Model 5", "Model 6", "Model 7")

fit_terms  <- c("n", "R2", "elpd_loo", "looic", "Sigma", "RMSE")
fit_labels <- c(n = "n", R2 = "Bayesian R2", elpd_loo = "ELPD",
                looic = "LOOIC", Sigma = "Sigma", RMSE = "RMSE")

stat_cols <- c("Estimate", "Est.Error", "CrI")

model_groups <- list(c("m5", "m6", "m7"))
table_captions <- c("Results: Linear models")
table_labels   <- c("linear-models-addl")

unmatched_in_data <- setdiff(unique(summ_models_a2$term), c(term_spec$term, fit_terms))
unmatched_in_spec <- setdiff(na.omit(term_spec$term), unique(summ_models_a2$term))
if (length(unmatched_in_data) > 0)
  warning("Terms in data but missing from term_spec (will be DROPPED): ",
          paste(unmatched_in_data, collapse = ", "))
if (length(unmatched_in_spec) > 0)
  warning("Terms in term_spec that match nothing in the data: ",
          paste(unmatched_in_spec, collapse = ", "))

build_table <- function(mods, caption, label) {
  labs  <- model_labels[match(mods, model_levels)]
  n_mod <- length(mods)
  
  col_order <- as.vector(t(outer(labs, stat_cols, paste, sep = "__")))
  
  wide_tab <- summ_models_a2 %>%
    filter(model %in% mods) %>%
    mutate(model   = factor(model, model_levels, model_labels),
           section = if_else(term %in% fit_terms, "fit", "coef")) %>%
    select(model, section, term, all_of(stat_cols)) %>%
    pivot_wider(names_from  = model,
                values_from = all_of(stat_cols),
                names_glue  = "{model}__{.value}") %>%
    select(section, term, any_of(col_order))
  
  coef_tab <- term_spec %>%
    left_join(filter(wide_tab, section == "coef") %>% select(-section),
              by = "term", na_matches = "never") %>%
    filter(is.na(term) | if_any(-c(term, label, indent), ~ !is.na(.x))) %>%
    mutate(term = label)
  
  indent_rows <- which(coef_tab$indent)
  coef_tab    <- coef_tab %>% select(-label, -indent)
  
  fit_tab <- wide_tab %>%
    filter(section == "fit") %>%
    select(-section) %>%
    arrange(match(term, fit_terms)) %>%
    mutate(across(ends_with("__Estimate"),
                  ~ if_else(term == "n" & !is.na(.x),
                            formatC(suppressWarnings(as.numeric(.x)),
                                    format = "d", big.mark = ","),
                            .x))) %>%
    mutate(term = unname(fit_labels[term]))
  
  tab_df <- bind_rows(coef_tab, fit_tab) %>%
    mutate(across(everything(), ~ replace_na(as.character(.x), ""))) %>%
    mutate(across(-term, ~ gsub("-", "$-$", .x, fixed = TRUE))) %>%
    mutate(term = term %>%
             gsub("\u00d7", "$\\times$", ., fixed = TRUE) %>%
             gsub("\u2013", "--", ., fixed = TRUE) %>%
             gsub("Bayesian R2", "Bayes \\textit{R}\\textsuperscript{2}", ., fixed = TRUE))
  
  stopifnot(ncol(tab_df) == 1 + 3 * n_mod)
  
  tab_df %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE,
        escape = FALSE, linesep = "",
        caption = caption, label = label,
        align = c("l", rep(c("r", "r", "c"), n_mod)),
        col.names = c("", rep(c("\\textbf{Estimate}", "\\textbf{Est. Err.}",
                                "\\textbf{95\\% CrI}"), n_mod))) %>%
    add_header_above(c(" " = 1,
                       setNames(rep(3, n_mod), paste0("\\textbf{", labs, "}"))),
                     escape = FALSE) %>%
    add_indent(indent_rows) %>%
    row_spec(nrow(coef_tab), extra_latex_after = "\\midrule") %>%  # rule above fit stats
    column_spec(1, width = "2.5in") %>%
    kable_styling(latex_options = "repeat_header") %>%
    as.character()
}

tables <- pmap_chr(list(model_groups, table_captions, table_labels),
                   ~ build_table(..1, ..2, ..3))

tex <- c("\\begin{landscape}",
         "\\renewcommand{\\thetable}{A\\arabic{table}}",
         "\\scriptsize",
         "\\setlength{\\tabcolsep}{2pt}",
         "\\renewcommand{\\arraystretch}{0.96}",
         paste(tables, collapse = "\n\n"),
         "",
         "\\vspace{-6pt}",
         "\\begin{minipage}{\\linewidth}",
         "\\setlength{\\parskip}{0pt}\\scriptsize",
         note,
         "\\end{minipage}",
         "\\end{landscape}")

writeLines(tex, "./manuscript/table_a2.tex")


## Table A3
summ_models_a3 <- bind_rows(summ_m0.nr, summ_m1.nr, summ_m2.nr, summ_m3.nr, summ_m4.nr)

model_levels <- c("m0.nr", "m1.nr", "m2.nr", "m3.nr", "m4.nr")
model_labels <- c("Model 0a", "Model 1a", "Model 2a", "Model 3a", "Model 4a")

fit_terms  <- c("n", "R2", "elpd_loo", "looic", "Sigma", "RMSE")
fit_labels <- c(n = "n", R2 = "Bayesian R2", elpd_loo = "ELPD",
                looic = "LOOIC", Sigma = "Sigma", RMSE = "RMSE")

stat_cols <- c("Estimate", "Est.Error", "CrI")

model_groups <- list(c("m0.nr", "m1.nr"), c("m2.nr", "m3.nr", "m4.nr"))
table_captions <- c("Results: Linear models, non-responses on dependent variable removed",
                    "Results: Linear models, non-responses on dependent variable removed (continued)")
table_labels   <- c("linear-models-wo-dks", NA)

unmatched_in_data <- setdiff(unique(summ_models_a3$term), c(term_spec$term, fit_terms))
unmatched_in_spec <- setdiff(na.omit(term_spec$term), unique(summ_models_a3$term))
if (length(unmatched_in_data) > 0)
  warning("Terms in data but missing from term_spec (will be DROPPED): ",
          paste(unmatched_in_data, collapse = ", "))
if (length(unmatched_in_spec) > 0)
  warning("Terms in term_spec that match nothing in the data: ",
          paste(unmatched_in_spec, collapse = ", "))

build_table <- function(mods, caption, label) {
  labs  <- model_labels[match(mods, model_levels)]
  n_mod <- length(mods)
  
  col_order <- as.vector(t(outer(labs, stat_cols, paste, sep = "__")))
  
  wide_tab <- summ_models_a3 %>%
    filter(model %in% mods) %>%
    mutate(model   = factor(model, model_levels, model_labels),
           section = if_else(term %in% fit_terms, "fit", "coef")) %>%
    select(model, section, term, all_of(stat_cols)) %>%
    pivot_wider(names_from  = model,
                values_from = all_of(stat_cols),
                names_glue  = "{model}__{.value}") %>%
    select(section, term, any_of(col_order))
  
  coef_tab <- term_spec %>%
    left_join(filter(wide_tab, section == "coef") %>% select(-section),
              by = "term", na_matches = "never") %>%
    filter(is.na(term) | if_any(-c(term, label, indent), ~ !is.na(.x))) %>%
    mutate(term = label)
  
  indent_rows <- which(coef_tab$indent)
  coef_tab    <- coef_tab %>% select(-label, -indent)
  
  fit_tab <- wide_tab %>%
    filter(section == "fit") %>%
    select(-section) %>%
    arrange(match(term, fit_terms)) %>%
    mutate(across(ends_with("__Estimate"),
                  ~ if_else(term == "n" & !is.na(.x),
                            formatC(suppressWarnings(as.numeric(.x)),
                                    format = "d", big.mark = ","),
                            .x))) %>%
    mutate(term = unname(fit_labels[term]))
  
  tab_df <- bind_rows(coef_tab, fit_tab) %>%
    mutate(across(everything(), ~ replace_na(as.character(.x), ""))) %>%
    mutate(across(-term, ~ gsub("-", "$-$", .x, fixed = TRUE))) %>%
    mutate(term = term %>%
             gsub("\u00d7", "$\\times$", ., fixed = TRUE) %>%
             gsub("\u2013", "--", ., fixed = TRUE) %>%
             gsub("Bayesian R2", "Bayes \\textit{R}\\textsuperscript{2}", ., fixed = TRUE))
  
  stopifnot(ncol(tab_df) == 1 + 3 * n_mod)
  
  tab_df %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE,
        escape = FALSE, linesep = "",
        caption = caption, label = label,
        align = c("l", rep(c("r", "r", "c"), n_mod)),
        col.names = c("", rep(c("\\textbf{Estimate}", "\\textbf{Est. Err.}",
                                "\\textbf{95\\% CrI}"), n_mod))) %>%
    add_header_above(c(" " = 1,
                       setNames(rep(3, n_mod), paste0("\\textbf{", labs, "}"))),
                     escape = FALSE) %>%
    add_indent(indent_rows) %>%
    row_spec(nrow(coef_tab), extra_latex_after = "\\midrule") %>%  # rule above fit stats
    column_spec(1, width = "2.5in") %>%
    kable_styling(latex_options = "repeat_header") %>%
    as.character()
}

tables <- pmap_chr(list(model_groups, table_captions, table_labels),
                   ~ build_table(..1, ..2, ..3))

tex <- c("\\begin{landscape}",
         "\\renewcommand{\\thetable}{A\\arabic{table}}",
         "\\scriptsize",
         "\\setlength{\\tabcolsep}{2pt}",
         "\\renewcommand{\\arraystretch}{0.96}",
         paste(tables, collapse = "\n\n"),
         "",
         "\\vspace{-6pt}",
         "\\begin{minipage}{\\linewidth}",
         "\\setlength{\\parskip}{0pt}\\scriptsize",
         note,
         "\\end{minipage}",
         "\\end{landscape}")

writeLines(tex, "./manuscript/table_a3.tex")


## Table A4
summ_models_a4 <- bind_rows(summ_m5.nr, summ_m6.nr, summ_m7.nr)

model_levels <- c("m5.nr", "m6.nr", "m7.nr")
model_labels <- c("Model 5a", "Model 6a", "Model 7a")

fit_terms  <- c("n", "R2", "elpd_loo", "looic", "Sigma", "RMSE")
fit_labels <- c(n = "n", R2 = "Bayesian R2", elpd_loo = "ELPD",
                looic = "LOOIC", Sigma = "Sigma", RMSE = "RMSE")

stat_cols <- c("Estimate", "Est.Error", "CrI")

model_groups <- list(c("m5.nr", "m6.nr", "m7.nr"))
table_captions <- c("Results: Linear models, non-responses on dependent variable removed")
table_labels   <- c("linear-models-wo-dks-addl")

unmatched_in_data <- setdiff(unique(summ_models_a4$term), c(term_spec$term, fit_terms))
unmatched_in_spec <- setdiff(na.omit(term_spec$term), unique(summ_models_a4$term))
if (length(unmatched_in_data) > 0)
  warning("Terms in data but missing from term_spec (will be DROPPED): ",
          paste(unmatched_in_data, collapse = ", "))
if (length(unmatched_in_spec) > 0)
  warning("Terms in term_spec that match nothing in the data: ",
          paste(unmatched_in_spec, collapse = ", "))

build_table <- function(mods, caption, label) {
  labs  <- model_labels[match(mods, model_levels)]
  n_mod <- length(mods)
  
  col_order <- as.vector(t(outer(labs, stat_cols, paste, sep = "__")))
  
  wide_tab <- summ_models_a4 %>%
    filter(model %in% mods) %>%
    mutate(model   = factor(model, model_levels, model_labels),
           section = if_else(term %in% fit_terms, "fit", "coef")) %>%
    select(model, section, term, all_of(stat_cols)) %>%
    pivot_wider(names_from  = model,
                values_from = all_of(stat_cols),
                names_glue  = "{model}__{.value}") %>%
    select(section, term, any_of(col_order))
  
  coef_tab <- term_spec %>%
    left_join(filter(wide_tab, section == "coef") %>% select(-section),
              by = "term", na_matches = "never") %>%
    filter(is.na(term) | if_any(-c(term, label, indent), ~ !is.na(.x))) %>%
    mutate(term = label)
  
  indent_rows <- which(coef_tab$indent)
  coef_tab    <- coef_tab %>% select(-label, -indent)
  
  fit_tab <- wide_tab %>%
    filter(section == "fit") %>%
    select(-section) %>%
    arrange(match(term, fit_terms)) %>%
    mutate(across(ends_with("__Estimate"),
                  ~ if_else(term == "n" & !is.na(.x),
                            formatC(suppressWarnings(as.numeric(.x)),
                                    format = "d", big.mark = ","),
                            .x))) %>%
    mutate(term = unname(fit_labels[term]))
  
  tab_df <- bind_rows(coef_tab, fit_tab) %>%
    mutate(across(everything(), ~ replace_na(as.character(.x), ""))) %>%
    mutate(across(-term, ~ gsub("-", "$-$", .x, fixed = TRUE))) %>%
    mutate(term = term %>%
             gsub("\u00d7", "$\\times$", ., fixed = TRUE) %>%
             gsub("\u2013", "--", ., fixed = TRUE) %>%
             gsub("Bayesian R2", "Bayes \\textit{R}\\textsuperscript{2}", ., fixed = TRUE))
  
  stopifnot(ncol(tab_df) == 1 + 3 * n_mod)
  
  tab_df %>%
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE,
        escape = FALSE, linesep = "",
        caption = caption, label = label,
        align = c("l", rep(c("r", "r", "c"), n_mod)),
        col.names = c("", rep(c("\\textbf{Estimate}", "\\textbf{Est. Err.}",
                                "\\textbf{95\\% CrI}"), n_mod))) %>%
    add_header_above(c(" " = 1,
                       setNames(rep(3, n_mod), paste0("\\textbf{", labs, "}"))),
                     escape = FALSE) %>%
    add_indent(indent_rows) %>%
    row_spec(nrow(coef_tab), extra_latex_after = "\\midrule") %>%  # rule above fit stats
    column_spec(1, width = "2.5in") %>%
    kable_styling(latex_options = "repeat_header") %>%
    as.character()
}

tables <- pmap_chr(list(model_groups, table_captions, table_labels),
                   ~ build_table(..1, ..2, ..3))

tex <- c("\\begin{landscape}",
         "\\renewcommand{\\thetable}{A\\arabic{table}}",
         "\\scriptsize",
         "\\setlength{\\tabcolsep}{2pt}",
         "\\renewcommand{\\arraystretch}{0.96}",
         paste(tables, collapse = "\n\n"),
         "",
         "\\vspace{-6pt}",
         "\\begin{minipage}{\\linewidth}",
         "\\setlength{\\parskip}{0pt}\\scriptsize",
         note,
         "\\end{minipage}",
         "\\end{landscape}")

writeLines(tex, "./manuscript/table_a4.tex")

## Figures
## Plot of unadjusted means by condition
score_means <- c("UK", "Local") %>%
  as_tibble() %>%
  mutate(LOC = as_factor(value)) %>%
  select(-value) %>%
  crossing(as_tibble(c("Power Station", "Reactor"))) %>%
  mutate(RPS = as_factor(value)) %>%
  select(-value)

pred.means <- fitted(m00, score_means, probs = c(0.085, 0.915), cores = 12) %>%
  as_tibble() %>%
  bind_cols(., score_means) %>%
  rename(LCL = Q8.5,
         UCL = Q91.5) %>%
  mutate(RPS = fct_relevel(RPS, "Power Station", "Reactor")) %>%
  mutate(LOC = as_factor(case_when(
    LOC == "Local" ~ "Local area",
    LOC == "UK" ~ "UK"
  ))) %>%
  mutate(LOC = fct_relevel(LOC, "UK", "Local area"))
print(pred.means, digits = 3)

fig.0 <- ggplot(data = pred.means, aes(x = RPS, y = Estimate, fill = RPS)) +
  geom_bar(aes(y = Estimate+0.5), stat = "identity", width = 0.6, position = position_dodge(0.75)) +
  scale_fill_manual(values = c("Power Station" = "grey84", "Reactor" = "grey30")) +
  geom_errorbar(aes(ymin = LCL+0.5, ymax = UCL+0.5),
                position = position_dodge(0.75), width = 0.3, colour = "black", linewidth = 0.4) +
  labs(y = "Support for modular nuclear",
       x = "") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), expand = c(0,0),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  facet_wrap(~ LOC, ncol = 2, axes = "all", scales = "fixed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.spacing = unit(1.5, "lines"),
        axis.text = element_text(size = 9, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 10), color = "black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 10, colour = "black"),
        legend.position = "none")

fig.0

ggsave(plot = fig.0, file = "./manuscript/Fig 0-Means by condition 2026 10 26.png",
       device = "png",  width = 6.5, height = 3, units = "in", dpi = 1200)

## Fig. 2: Coefficient plot
coef.ci.m1 <- m1 %>%
  broom::tidy(., conf.int = TRUE) %>%
  filter(effect == "fixed") %>%
  bind_rows(., tribble(~term, "England (ref.)", "Power station frame (ref.)", "UK frame (ref.)")) %>%
  mutate(model.name = "Model 1") %>%
  mutate(reference = case_when(
    is.na(estimate) == TRUE ~ "Baseline category",
    TRUE ~ "Non-baseline category"
  )) %>%
  mutate(estimate = case_when(
    #str_detect(term, "Intercept") == TRUE ~ estimate / 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ estimate
  )) %>%
  mutate(std.error = case_when(
    #str_detect(term, "Intercept") == TRUE ~ std.error / 10,
    TRUE ~ std.error
  )) %>%
  mutate(UCL = case_when(
    #str_detect(term, "Intercept") == TRUE ~ conf.high/ 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ conf.high
  )) %>%
  mutate(LCL = case_when(
    #str_detect(term, "Intercept") == TRUE ~ conf.low / 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ conf.low
  )) %>%
  select(model.name, term, estimate, LCL, UCL, reference)

coef.ci.m2 <- m2 %>%
  broom::tidy(., conf.int = TRUE) %>%
  filter(effect == "fixed") %>%
  bind_rows(., tribble(~term, "England (ref.)", "Power station frame (ref.)", "UK frame (ref.)")) %>%
  mutate(model.name = "Model 2") %>%
  mutate(reference = case_when(
    is.na(estimate) == TRUE ~ "Baseline category",
    TRUE ~ "Non-baseline category"
  )) %>%
  mutate(estimate = case_when(
    #tr_detect(term, "Intercept") == TRUE ~ estimate / 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ estimate
  )) %>%
  mutate(std.error = case_when(
    #str_detect(term, "Intercept") == TRUE ~ std.error / 10,
    TRUE ~ std.error
  )) %>%
  mutate(UCL = case_when(
    #str_detect(term, "Intercept") == TRUE ~ conf.high/ 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ conf.high
  )) %>%
  mutate(LCL = case_when(
    #str_detect(term, "Intercept") == TRUE ~ conf.low / 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ conf.low
  )) %>%
  select(model.name, term, estimate, LCL, UCL, reference)
  
coef.ci.m3 <- m3 %>%
  broom::tidy(., conf.int = TRUE) %>%
  filter(effect == "fixed") %>%
  bind_rows(., tribble(~term, "England (ref.)", "Power station frame (ref.)", "UK frame (ref.)")) %>%
  mutate(model.name = "Model 3") %>%
  mutate(reference = case_when(
    is.na(estimate) == TRUE ~ "Baseline category",
    TRUE ~ "Non-baseline category"
  )) %>%
  mutate(estimate = case_when(
    #str_detect(term, "Intercept") == TRUE ~ estimate / 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ estimate
  )) %>%
  mutate(std.error = case_when(
    #str_detect(term, "Intercept") == TRUE ~ std.error / 10,
    TRUE ~ std.error
  )) %>%
  mutate(UCL = case_when(
    #str_detect(term, "Intercept") == TRUE ~ conf.high/ 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ conf.high
  )) %>%
  mutate(LCL = case_when(
    #str_detect(term, "Intercept") == TRUE ~ conf.low / 10,
    reference == "Baseline category" ~ 0,
    TRUE ~ conf.low
  )) %>%
  select(model.name, term, estimate, LCL, UCL, reference)

coef.ci.support <- bind_rows(coef.ci.m1, coef.ci.m2, coef.ci.m3) %>% 
  mutate(model.name = as_factor(model.name)) %>%
  mutate(model.name = fct_relevel(model.name, "Model 3", "Model 2", "Model 1")) %>%
  mutate(coef.label = as_factor(case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "ln_age_years" ~ "Age (years logged)",
    term == "Educ" ~ "Education",
    term == "CountryWales" ~ "Wales",
    term == "CountryScotland" ~ "Scotland",
    term == "CountryNorthernIreland" ~ "N. Ireland",
    term == "RPSReactor" ~ "Reactor frame",
    term == "LOCLocal" ~ "Local area frame",
    term == "ln_nuclear_dist_km" ~ "Distance to nuclear site\n(km logged)",
    term == "follow_elec" ~ "Follow news about\nelectricity in UK",
    term == "need_more_elec" ~ "Need to produce a lot\nmore electricity",
    term == "elec_self_suff" ~ "UK should be self-sufficient\nin electricity",
    term == "elec_price_low" ~ "Critical to keep price of\nelectricity in UK low",
    term == "trust_govt" ~ "Trust in government",
    term == "trust_elec" ~ "Trust in electricity industry",
    term == "climate" ~ "Climate concern",
    term == "left_right" ~ "Ideology (left-right)",
    term == "CountryWales:LOCLocal" ~ paste0("Wales ", "\u00D7", " Local area frame"),
    term %in% c("CountryScotland:LOCLocal", "Scotland_Local") ~ paste0("Scotland ", "\u00D7", " Local area frame"),
    term == "CountryNorthernIreland:LOCLocal" ~ paste0("N. Ireland ", "\u00D7", " Local area frame"),
    TRUE ~ term
  ))) %>%
  filter(is.na(coef.label) == FALSE) %>%
  mutate(coef.label = fct_relevel(coef.label,
                                  "Intercept",
                                  "Power station frame (ref.)", "Reactor frame",
                                  "UK frame (ref.)", "Local area frame",
                                  "England (ref.)", "Scotland", "Wales", "N. Ireland",
                                  "Distance to nuclear site\n(km logged)",
                                  "Female", "Age (years logged)", "Education",
                                  "Follow news about\nelectricity in UK", "Need to produce a lot\nmore electricity",
                                  "UK should be self-sufficient\nin electricity", "Critical to keep price of\nelectricity in UK low",
                                  "Climate concern", "Trust in government", "Trust in electricity industry",
                                  "Ideology (left-right)",
                                  "Scotland × Local area frame",
                                  "Wales × Local area frame",
                                  "N. Ireland × Local area frame"
  )) %>%
  mutate(pt.shape = case_when(
    model.name == "Model 1" & reference == "Baseline category" ~ 21,
    model.name == "Model 1" ~ 16,
    model.name == "Model 2" & reference == "Baseline category" ~ 22,
    model.name == "Model 2" ~ 15,
    model.name == "Model 3" & reference == "Baseline category" ~ 23,
    model.name == "Model 3" ~ 18
  )) %>%
  mutate(pt.fill = case_when(
    reference == "Baseline category" ~ "white"
  )) %>%
  mutate(pt.size = case_when(
    model.name == "Model 3" & reference == "Baseline category" ~ 0.45,
    model.name == "Model 1" ~ 0.5,
    model.name == "Model 2" ~ 0.5,
    model.name == "Model 3" ~ 0.65
  ))

fig.2 <- ggplot(data = coef.ci.support, aes(x = estimate, y = coef.label, colour = model.name)) +
  geom_vline(xintercept = 0, colour = "grey72", linewidth = 0.5, linetype = "longdash") +
  geom_pointrange(aes(x = estimate, y = coef.label, xmin = LCL, xmax = UCL, colour = model.name),
                  shape = coef.ci.support$pt.shape, size = coef.ci.support$pt.size,
                  fill = coef.ci.support$pt.fill, stroke = 0.4,
                  position = position_dodge(0.75)) +
  scale_color_manual(values = c("grey60", "grey48", "grey32")) +
  labs(x = "Estimate", y = "") +
  scale_y_discrete(limits = rev(levels(coef.ci.support$coef.label))) +
  coord_cartesian(xlim = c(-0.4, 0.4)) +
  scale_x_continuous(breaks = seq(-0.4, 0.4, by = 0.2)) +
  guides(colour = guide_legend(override.aes = list(shape = c(16,15,18), size = c(0.5,0.5,0.6)), reverse = TRUE)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 9, colour = "black"), 
        axis.text.y = element_text(size = 8, colour = "black"),
        text = element_text(size = 9, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.14, 0.94),
        legend.text = element_text(size = 9, colour = "black"))

fig.2

ggsave(plot = fig.2, file = "./manuscript/Fig 2-Coefficient plot-2026 10 26.png",
       device = "png",  width = 6, height = 8.25, units = "in", dpi = 1200)

coef.ci.support.exp.dist <- coef.ci.support %>%
  filter(coef.label %in% c("Intercept",
                           "England (ref.)", "Wales", "Scotland", "N. Ireland",
                           "Distance to nuclear site\n(km logged)",
                           "Power station frame (ref.)", "Reactor frame",
                           "UK frame (ref.)", "Local area frame",
                           "Scotland × Local area frame",
                           "Wales × Local area frame",
                           "N. Ireland × Local area frame")) %>%
  mutate(coef.label = fct_drop(coef.label))

fig.2a <- ggplot(data = coef.ci.support.exp.dist, aes(x = estimate, y = coef.label, colour = model.name)) +
  geom_vline(xintercept = 0, colour = "grey72", linewidth = 0.5, linetype = "longdash") +
  geom_pointrange(aes(x = estimate, y = coef.label, xmin = LCL, xmax = UCL, colour = model.name),
                  shape = coef.ci.support.exp.dist$pt.shape, size = coef.ci.support.exp.dist$pt.size,
                  fill = coef.ci.support.exp.dist$pt.fill, stroke = 0.4,
                  position = position_dodge(0.75)) +
  scale_color_manual(values = c("grey60", "grey48", "grey32")) +
  labs(x = "Estimate", y = "") +
  scale_y_discrete(limits = rev(levels(coef.ci.support.exp.dist$coef.label))) +
  coord_cartesian(xlim = c(-0.4, 0.4)) +
  scale_x_continuous(breaks = seq(-0.4, 0.4, by = 0.2)) +
  guides(colour = guide_legend(override.aes = list(shape = c(16,15,18), size = c(0.5,0.5,0.6)), reverse = TRUE)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 9, colour = "black"), 
        axis.text.y = element_text(size = 8, colour = "black"),
        text = element_text(size = 9, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.14, 0.92),
        legend.text = element_text(size = 9, colour = "black"))

fig.2a

ggsave(plot = fig.2a, file = "./manuscript/Fig 2a-Coefficient plot-2026 10 26.png",
       device = "png",  width = 6, height = 6, units = "in", dpi = 1200)

## Fig. 3: Effect of power station/reactor frames
score_reactor <- c("England", "Scotland", "Wales", "Northern Ireland") %>%
  as_tibble() %>%
  mutate(Country = as_factor(value)) %>%
  select(-value) %>%
  crossing(as_tibble(c("Power Station", "Reactor"))) %>%
  mutate(RPS = as_factor(value)) %>%
  select(-value) %>%
  mutate(Female = 1,
         ln_age_years = 0,
         Educ = 0,
         LOC = "UK",
         ln_nuclear_dist_km = 0,
         follow_elec = 0,
         need_more_elec = 0,
         elec_self_suff = 0,
         elec_price_low = 0,
         trust_govt = 0,
         trust_elec = 0,
         climate = 0,
         left_right = 0,
         Scotland_Local = 0)

pred.country.reactor <- fitted(m3, score_reactor, probs = c(0.085, 0.915), cores = 12) %>%
  as_tibble() %>%
  bind_cols(., score_reactor) %>%
  rename(LCL = Q8.5,
         UCL = Q91.5) %>%
  mutate(Country = fct_relevel(Country, "England", "Scotland", "Wales", "Northern Ireland")) %>%
  select(Country, RPS, Estimate, Est.Error, LCL, UCL)
print(pred.country.reactor, digits = 3)

fig.3 <- ggplot(data = pred.country.reactor, aes(x = RPS, y = Estimate, fill = RPS)) +
  geom_bar(aes(y = Estimate+0.5), stat = "identity", width = 0.6, position = position_dodge(0.75)) +
  scale_fill_manual(values = c("Power Station" = "grey84", "Reactor" = "grey30")) +
  geom_errorbar(aes(ymin = LCL+0.5, ymax = UCL+0.5),
                position = position_dodge(0.75), width = 0.3, colour = "black", linewidth = 0.4) +
  labs(y = "Support for modular nuclear",
       x = "") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), expand = c(0,0),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  facet_wrap(~ Country, ncol = 2, axes = "all", scales = "fixed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.spacing = unit(1.5, "lines"),
        axis.text = element_text(size = 9, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 10), color = "black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 10, colour = "black"),
        legend.position = "none")

fig.3

ggsave(plot = fig.3, file = "./manuscript/Fig 3-Effect plot-Country x reactor 2026 10 26.png",
       device = "png",  width = 6.5, height = 6, units = "in", dpi = 1200)

## Fig. 4: Effect of UK/local frames
score_uk_local <- c("England", "Scotland", "Wales", "Northern Ireland") %>%
  as_tibble() %>%
  mutate(Country = as_factor(value)) %>%
  select(-value) %>%
  crossing(as_tibble(c("UK", "Local"))) %>%
  mutate(LOC = as_factor(value)) %>%
  select(-value) %>%
  mutate(Female = 1,
         ln_age_years = 0,
         Educ = 0,
         RPS = "Power Station",
         ln_nuclear_dist_km = 0,
         follow_elec = 0,
         need_more_elec = 0,
         elec_self_suff = 0,
         elec_price_low = 0,
         trust_govt = 0,
         trust_elec = 0,
         climate = 0,
         left_right = 0) %>%
  mutate(Scotland_Local = case_when(
    Country == "Scotland" & LOC == "Local" ~ 1,
    TRUE ~ 0))

pred.country.local <- fitted(m3, score_uk_local, probs = c(0.085, 0.915), cores = 12) %>%
  as_tibble() %>%
  bind_cols(., score_uk_local) %>%
  rename(LCL = Q8.5,
         UCL = Q91.5) %>%
  mutate(Country = fct_relevel(Country, "England", "Scotland", "Wales", "Northern Ireland")) %>%
  mutate(LOC = as_factor(case_when(
    LOC == "UK" ~ "UK",
    LOC == "Local" ~ "Local area"
  ))) %>%
  mutate(LOC = fct_relevel(LOC, "UK", "Local area")) %>%
  select(Country, LOC, Estimate, Est.Error, LCL, UCL)
print(pred.country.local, digits = 3)

fig.4 <- ggplot(data = pred.country.local, aes(x = LOC, y = Estimate, fill = LOC)) +
  geom_bar(aes(y = Estimate+0.5), stat = "identity", width = 0.6, position = position_dodge(0.75)) +
  scale_fill_manual(values = c("UK" = "grey84", "Local area" = "grey30")) +
  geom_errorbar(aes(ymin = LCL+0.5, ymax = UCL+0.5), position = position_dodge(0.75), width = 0.3, colour = "black", linewidth = 0.4) +
  labs(y = "Support for modular nuclear",
       x = "") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), expand = c(0,0),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  facet_wrap(~ Country, ncol = 2, axes = "all", scales = "fixed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.spacing = unit(1.5, "lines"),
        axis.text = element_text(size = 9, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 10), color = "black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 10, colour = "black"),
        legend.position = "none")

fig.4

ggsave(plot = fig.4, file = "./manuscript/Fig 4-Effect plot-Country x local 2026 10 26.png",
       device = "png",  width = 6.5, height = 6, units = "in", dpi = 1200)

## Fig. 5: Effect of distance to nuclear site (m3)
score_dist <- as_tibble(seq(from = 0.1, to = 20, by = 0.1)) %>%
  bind_rows(as_tibble(seq(from = 21, to = 600, by = 1))) %>%
  rename(nuclear_dist_km = value) %>%
  mutate(ln_nuclear_dist_km = log(nuclear_dist_km) - weighted.mean(x = data.0$ln_nuclear_dist_km, w = data.0$weight)) %>%
  crossing(as_tibble(c("UK", "Local"))) %>%
  mutate(LOC = as_factor(value)) %>%
  select(-value) %>%
  crossing(as_tibble(c("England", "Scotland"))) %>%
  mutate(Country = as_factor(value)) %>%
  select(-value) %>%
  mutate(Female = 1,
         ln_age_years = 0,
         Educ = 0,
         RPS = "Power Station",
         follow_elec = 0,
         need_more_elec = 0,
         elec_self_suff = 0,
         elec_price_low = 0,
         trust_govt = 0,
         trust_elec = 0,
         climate = 0,
         left_right = 0) %>%
  mutate(Scotland_Local = case_when(
    Country == "Scotland" & LOC == "Local" ~ 1,
    TRUE ~ 0
  ))

pred.country.dist.uk.local <- fitted(m3, score_dist, probs = c(0.085, 0.915), cores = 12) %>%
  as_tibble() %>%
  bind_cols(., score_dist) %>%
  rename(LCL = Q8.5,
         UCL = Q91.5) %>%
  mutate(Country = fct_relevel(as_factor(Country), "England", "Scotland")) %>%
  mutate(UK_Local = as_factor(case_when(
    LOC == "UK" ~ "UK",
    LOC == "Local" ~ "Local area"
  ))) %>%
  mutate(UK_Local = fct_relevel(UK_Local, "UK", "Local area")) %>%
  mutate(Country_UK_Local = str_c(Country, "–", UK_Local, " frame")) %>%
  mutate(Country_UK_Local = fct_relevel(Country_UK_Local, "England–UK frame", "England–Local area frame",
                                        "Scotland–UK frame","Scotland–Local area frame")) %>%
  arrange(Country_UK_Local, nuclear_dist_km)
  
pred.10.200 <- pred.country.dist.uk.local %>%
  filter(UK_Local == "Local area" & nuclear_dist_km %in% c(10, 50, 100, 200)) %>%
  select(Country, nuclear_dist_km, Estimate, Est.Error, LCL, UCL)
print(pred.10.200, digits = 8)

data.rug <- data.2 %>%
  filter(Country %in% c("England", "Scotland")) %>%
  mutate(UK_Local = as_factor(case_when(
    LOC == "UK" ~ "UK",
    LOC == "Local" ~ "Local area"
  ))) %>%
  mutate(Country_UK_Local = str_c(Country, "–", UK_Local, " frame")) %>%
  mutate(Country_UK_Local = fct_relevel(Country_UK_Local, "England–UK frame", "England–Local area frame",
                                        "Scotland–UK frame","Scotland–Local area frame")) %>%
  select(ResponseId, Country, UK_Local, Country_UK_Local, nuclear_dist_km)

fig.5 <- ggplot(data = pred.country.dist.uk.local, aes(y = Estimate+0.5, x = nuclear_dist_km, linetype = UK_Local)) +
  geom_line(aes(y = Estimate+0.5), linewidth = 0.5) +
  geom_ribbon(aes(ymin = LCL+0.5, ymax = UCL+0.5), alpha = 0.1) +
  geom_rug(data = data.rug, aes(x = nuclear_dist_km), sides = "b", alpha = 0.4, linewidth = 0.4, inherit.aes = FALSE) +
  labs(y = "Support for modular nuclear",
       x = "Distance to nuclear site") +
  coord_cartesian(xlim = c(4.5, 500), ylim = c(0, 1)) +
  scale_x_continuous(trans = "log", breaks = c(5, 10, 20, 50, 100, 200, 500), labels = label_number()) +
  scale_linetype_manual(values = c("UK" = "solid", "Local area" = "dashed")) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), expand = c(0,0),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  facet_wrap(~ Country, ncol = 2, axes = "all", scales = "fixed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        axis.text = element_text(size = 9, colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25), color = "black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 10, colour = "black"),
        axis.line.x.top = element_line(color = "black"),
        axis.ticks.x.top = element_line(color = "black"),
        axis.text.x.top = element_text(color = "black"),
        legend.title = element_blank(),
        legend.position = c(0.125, 0.225),
        legend.text = element_text(size = 9, colour = "black"))
  
fig.5
  
ggsave(plot = fig.5, file = "./manuscript/Fig 5-Effect plot-Country frame dist 2026 10 26.png",
       device = "png",  width = 6.5, height = 3, units = "in", dpi = 1200)

fig.5a <- ggplot(data = pred.country.dist.uk.local, aes(y = Estimate+0.5, x = nuclear_dist_km, linetype = UK_Local)) +
  geom_line(aes(y = Estimate+0.5), linewidth = 0.5) +
  geom_ribbon(aes(ymin = LCL+0.5, ymax = UCL+0.5), alpha = 0.1) +
  geom_rug(data = data.rug, aes(x = nuclear_dist_km), sides = "b", alpha = 0.4, linewidth = 0.4, inherit.aes = FALSE) +
  labs(y = "Support for modular nuclear",
       x = "Distance to nuclear site") +
  coord_cartesian(xlim = c(4.5, 500), ylim = c(0, 1)) +
  scale_x_continuous(trans = "log", breaks = c(5, 10, 20, 50, 100, 200, 500), labels = label_number()) +
  scale_linetype_manual(values = c("UK" = "solid", "Local area" = "dashed")) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), expand = c(0,0),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  facet_wrap(~ Country_UK_Local, ncol = 2, axes = "all", scales = "fixed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        axis.text = element_text(size = 9, colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 10), color = "black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 10, colour = "black"),
        axis.line.x.top = element_line(color = "black"),
        axis.ticks.x.top = element_line(color = "black"),
        axis.text.x.top = element_text(color = "black"),
        legend.position = "none")

fig.5a

#ggsave(plot = fig.5a, file = "./manuscript/Fig 5a-Effect plot-Country frame dist 2026 10 26.png",
#       device = "png", width = 6.5, height = 6, units = "in", dpi = 1200)