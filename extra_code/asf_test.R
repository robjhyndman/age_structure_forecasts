library(vital)
library(ggplot2)
library(dplyr)

# Check graduate forecasts
targets::tar_load(c(census4_1, future_pop_science_2016))

show_component_forecast(
  data = census4_1,
  forecasts = future_pop_science_2016,
  component = working,
  age_x = 30
)

show_component_forecast(
  data = census4_1,
  forecasts = future_pop_science_2016,
  component = remainder,
  age_x = 30
)

show_component_forecast(
  data = census4_1,
  forecasts = future_pop_science_2016,
  component = graduates,
  age_x = 30
)
