# Test stationary assumption for remainder principal components

library(broom)
library(vital)
library(tidyr)
library(targets)
library(dplyr)
library(fabletools)
library(feasts)

tar_load(model_Ext_discipline)
pv <- time_components(model_Ext_discipline) |>
  pivot_longer(
    cols = starts_with("beta"),
    names_to = "component",
    values_to = "value"
  ) |>
  features(value, unitroot_kpss) |>
  arrange(kpss_pvalue)

# Given we are doing 36 tests, we use the Bonferonni correction.
# need the p-value to be greater than 0.05/36 = 0.0014
# to not reject the null hypothesis of stationary.

pv |>
  filter(kpss_pvalue < 0.05 / 36)
