rm(list = ls())

library(tidyverse)
library(zoo)

train_target <- read_csv("dc_proj/Y_train_sl9m6Jh.csv")

train_data <- read_csv("dc_proj/X_train_v2.csv") %>%
  mutate(
    Time = dmy_hm(Time)
  ) %>%
  left_join(
    train_target,
    by = "ID"
  ) %>%
  mutate(
    across(
      ((contains("NWP2") | contains("NWP3")) &
         (contains("_U") | contains("_V"))),
      \(x) zoo::na.approx(x, maxgap = 2, na.rm = F))
  ) %>%
  pivot_longer(
    cols = `NWP1_00h_D-2_U`:NWP4_12h_D_CLCT,
    names_to = "name",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  separate(
    col = name,
    sep = "_",
    into = c("met_model", "model_time", "forecast_time", "met_variable")
  ) %>%
  pivot_wider(
    names_from = met_variable,
    values_from = value
  )


train_data_raw <- read_csv("dc_proj/X_train_v2.csv") %>%
  mutate(
    Time = dmy_hm(Time)
  ) %>%
  left_join(
    train_target,
    by = "ID"
  ) %>%
  pivot_longer(
    cols = `NWP1_00h_D-2_U`:NWP4_12h_D_CLCT,
    names_to = "name",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  separate(
    col = name,
    sep = "_",
    into = c("met_model", "model_time", "forecast_time", "met_variable")
  ) %>%
  pivot_wider(
    names_from = met_variable,
    values_from = value
  )

test_data <- read_csv("dc_proj/X_test_v2.csv") %>%
  mutate(
    Time = dmy_hm(Time)
  ) %>%
  mutate(
    across(
      (
        (contains("NWP2") | contains("NWP3")) &
          (contains("_U") | contains("_V"))),
      \(x) zoo::na.approx(x, maxgap = 2, na.rm = F))
  ) %>%
  pivot_longer(
    cols = `NWP1_00h_D-2_U`:NWP4_12h_D_CLCT,
    names_to = "name",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  separate(
    col = name,
    sep = "_",
    into = c("met_model", "model_time", "forecast_time", "met_variable")
  ) %>%
  pivot_wider(
    names_from = met_variable,
    values_from = value
  )


saveRDS(train_data_interp, file = "dc_proj/data_rds_files/x_train")
saveRDS(train_data_raw, file = "dc_proj/data_rds_files/x_train_raw")
saveRDS(test_data, file = "dc_proj/data_rds_files/x_test")
