## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)
library(scf)

## ----include = FALSE----------------------------------------------------------
vtd <- file.path(tempdir(), "scf_vig")
dir.create(vtd, showWarnings = FALSE)
src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
file.copy(src, file.path(vtd, "scf2022.rds"), overwrite = TRUE)
scf2022 <- scf_load(2022, data_directory = vtd)

## ----eval = FALSE-------------------------------------------------------------
# scf_download(2022)
# scf2022 <- scf_load(2022)

## -----------------------------------------------------------------------------
scf2022
scf2022$dollar_year

## -----------------------------------------------------------------------------
scf2022 <- scf_update(scf2022,
  senior       = age >= 65,
  female       = factor(hhsex, levels = 1:2, labels = c("Male", "Female")),
  rich         = networth > 1e6,
  networth     = ifelse(networth > 1, networth, 1),
  log_networth = log(networth),
  income       = ifelse(income > 1, income, 1),
  log_income   = log(income)
)

## ----eval = FALSE-------------------------------------------------------------
# scf2022 <- scf_update_by_implicate(scf2022, function(df) {
#   df$wealth_rank <- rank(df$networth) / nrow(df)
#   df
# })

## -----------------------------------------------------------------------------
seniors <- scf_subset(scf2022, age >= 65)
scf_median(seniors, ~networth)

## ----eval = FALSE-------------------------------------------------------------
# scf2016 <- scf_real(scf2016, ~x5702 + x3915, income = ~x5702)

## -----------------------------------------------------------------------------
scf_mean(scf2022, ~networth, by = ~senior)
scf_median(scf2022, ~income, by = ~female)
scf_percentile(scf2022, ~networth, q = 0.9)
scf_percentile(scf2022, ~networth, q = 0.75, by = ~female)

## -----------------------------------------------------------------------------
scf_pctile_sum(scf2022, ~networth, probs = c(0, 0.5, 0.9, 1),
               labels = c("bottom50", "50-90", "top10"),
               method = "stack", stat = "mean")

## -----------------------------------------------------------------------------
scf_freq(scf2022, ~senior)
scf_xtab(scf2022, ~senior, ~female, scale = "col")

## -----------------------------------------------------------------------------
scf_ratio(scf2022, ~networth, ~income, by = ~senior)
scf_corr(scf2022, ~log_income, ~log_networth)

## -----------------------------------------------------------------------------
scf_ttest(scf2022, ~networth, mu = 250000)
scf_ttest(scf2022, ~networth, group = ~senior)
scf_prop_test(scf2022, ~senior, p = 0.25)
scf_prop_test(scf2022, ~rich, ~female)

## -----------------------------------------------------------------------------
scf_ols(scf2022, log_networth ~ age + log_income)
scf_logit(scf2022, rich ~ age + log_income, odds = TRUE)
scf_glm(scf2022, own ~ age, family = binomial())

## -----------------------------------------------------------------------------
m1 <- scf_ols(scf2022, log_networth ~ age)
m2 <- scf_ols(scf2022, log_networth ~ age + log_income)
scf_regtable(m1, m2, model.names = c("Model 1", "Model 2"), digits = 3)

## -----------------------------------------------------------------------------
# Median regression
m_med <- scf_quantreg(scf2022, log_networth ~ age + senior, tau = 0.50)
print(m_med)

## -----------------------------------------------------------------------------
# 75th percentile, with fast standard errors (see below)
m_75 <- scf_quantreg(scf2022, log_networth ~ age + senior, tau = 0.75,
                     se = "nid")
summary(m_75)

## ----eval = FALSE-------------------------------------------------------------
# # Fast standard errors while exploring
# m_quick <- scf_quantreg(scf2022, log_networth ~ age + senior,
#                         tau = 0.50, se = "nid")

## -----------------------------------------------------------------------------
scf_regtable(m_med, m_75,
             model.names = c("Median", "75th Pct"),
             digits = 3)

## ----eval = FALSE-------------------------------------------------------------
# taus <- c(0.25, 0.50, 0.75, 0.90)
# models <- lapply(taus, function(t) {
#   scf_quantreg(scf2022, log_networth ~ age + senior, tau = t)
# })
# names(models) <- paste0("tau=", taus)
# do.call(scf_regtable, c(models, list(digits = 3)))

## -----------------------------------------------------------------------------
scf_plot_dbar(scf2022, ~senior)
scf_plot_bbar(scf2022, ~female, ~rich, scale = "percent")
scf_plot_cbar(scf2022, ~networth, ~edcl, stat = "median")
scf_plot_dist(scf2022, ~age, bins = 10)
scf_plot_hist(scf2022, ~networth)
scf_plot_smooth(scf2022, ~age)
scf_plot_hex(scf2022, ~income, ~networth)

## -----------------------------------------------------------------------------
freq_table <- scf_freq(scf2022, ~rich)
scf_implicates(freq_table, long = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# m <- scf_ols(scf2022, log_networth ~ age)
# summary(m$imps[[1]])   # first implicate svyglm object

## -----------------------------------------------------------------------------
SE(m2)
confint(m2)
SE(scf_mean(scf2022, ~networth, by = ~senior))

## ----include = FALSE----------------------------------------------------------
try(unlink(file.path(vtd, "scf2022.rds"), force = TRUE), silent = TRUE)
try(unlink(vtd, recursive = TRUE, force = TRUE), silent = TRUE)
rm(vtd)

