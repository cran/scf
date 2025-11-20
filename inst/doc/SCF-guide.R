## ----setup, include = FALSE---------------------------------------------------

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

library(dplyr)
library(scf)


## ----include = F--------------------------------------------------------------
# Using Mock data with distribution
vtd <- file.path(tempdir(), "scf_vig")
dir.create(vtd, showWarnings = FALSE)

src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
file.copy(src, file.path(vtd, "scf2022.rds"), overwrite = TRUE)
scf2022 <- scf_load(2022, data_directory = vtd)

## ----eval = F-----------------------------------------------------------------
# scf2022 <- scf_download(2022)
# scf2022 <- scf_load(scf2022)
# 

## -----------------------------------------------------------------------------

scf2022 <- scf_update(scf2022,
  senior = age >= 65,
  female = factor(hhsex, levels = 1:2, labels = c("Male", "Female")),
  rich = networth > 1e6,
  networth = ifelse(networth > 1, networth, 1),
  log_networth = log(networth),
  income = ifelse(income > 1, income, 1),
  log_income = log(income),
  npeople = x101
)


## -----------------------------------------------------------------------------
scf_mean(scf2022, ~networth, by = ~senior)
scf_median(scf2022, ~income, by = ~female)
scf_percentile(scf2022, ~networth, q = 0.9)
scf_percentile(scf2022, ~networth, q = 0.75, by = ~female)

## -----------------------------------------------------------------------------
scf_ttest(scf2022, ~networth, mu = 250000)
scf_ttest(scf2022, ~networth, group = ~senior)
scf_prop_test(scf2022, ~senior, p = 0.25)
scf_prop_test(scf2022, ~rich, ~female)

## -----------------------------------------------------------------------------
scf_ols(scf2022, networth ~ age + log_income)
scf_logit(scf2022, rich ~ age + log_income)
scf_logit(scf2022, rich ~ age + log_income, odds = TRUE)
scf_glm(scf2022, own ~ age , family = binomial())

## -----------------------------------------------------------------------------
scf_plot_dbar(scf2022, ~senior)
scf_plot_bbar(scf2022, ~female, ~rich, scale = "percent")
scf_plot_cbar(scf2022, ~networth, ~edcl, stat = "median")
scf_plot_dist(scf2022, ~age, bins = 10) 
scf_plot_smooth(scf2022, ~age)
scf_plot_hex(scf2022, ~income, ~networth)

## -----------------------------------------------------------------------------
freq_table <- scf_freq(scf2022, ~rich)
scf_implicates(freq_table, long = TRUE)

## ----include = F--------------------------------------------------------------
# Cleanup to avoid NOTE about leftover files
try(unlink(file.path(vtd, "scf2022.rds"), force=TRUE), silent=TRUE)
try(unlink(vtd, recursive=TRUE, force=TRUE), silent=TRUE)
rm(vtd)

