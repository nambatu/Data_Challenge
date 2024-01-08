rm(list = ls())

library(plotly)
library(caret)
library(mgcv)
library(tidyverse)


data <- readRDS("dc_proj/data_rds_files/x_train") %>%
  select(ID:T) %>%
  filter(
    model_time == "00h",
    forecast_time == "D-1"
  )


## modelling ----

cape <- function(actual, predicted){
  100 * sum(abs(actual - predicted)/sum(actual))
}

gam_cv <- function(data, folds = 4){
  errors <- 1:folds
  
  folds_def <- sample(
    1:folds,
    size = nrow(data),
    replace = T,
    prob = rep(1/folds, folds))
  
  for(i in 1:folds){
    
    train_fits <- data %>%
      filter(folds_def != i) %>%
      nest(.by = c(WF, t_cut), .key = "train_data") %>%
      mutate(
        gam_fit = map(
          train_data,
          ~ mgcv::gam(Production ~ s(windspeed), data = .x)
        )
      )
    
    test_fits <- data %>%
      filter(folds_def == i) %>%
      nest(.by = c(WF, t_cut), .key = "test_data") %>% 
      left_join(
        #left side: test data
        .,
        #right side: fitted models (for each combination of WF, t_cut)
        train_fits,
        by = join_by(WF, t_cut)
      ) %>%
      mutate(
        predicted = map2(.x = gam_fit, .y = test_data, .f = predict.gam)
      ) %>%
      unnest(c(test_data, predicted))
    
    errors[i] <- cape(
      test_fits$Production,
      test_fits$predicted
    )
  }
  
  return(mean(errors))
}

fit_data <- data %>%
  filter(met_model == "NWP2") %>%
  mutate(
    windspeed = sqrt(U^2+V^2)
  ) %>%
  select(!T) %>%
  left_join(
    # left side: model 2 weather data, interpolated
    .,
    # right side: model 1 temperature data
    data %>%
      filter(met_model == "NWP1") %>%
      select(ID, Time, T),
    # join by:
    by = c("ID", "Time")
  ) %>%
  drop_na(T) %>%
  filter(
    Production != 0
  ) %>%
  group_by(WF)


fit_data %>%
  mutate(
    t_cut = cut(T, breaks = 3)
  ) %>% 
  ungroup() %>%
  gam_cv()

fit_data %>%
  mutate(
    t_cut = cut(T, breaks = 5)
  ) %>% 
  ungroup() %>%
  gam_cv()

fit_data %>%
  mutate(
    t_cut = cut(T, breaks = 7)
  ) %>% 
  ungroup() %>%
  gam_cv()


## get prediction from gam model:

data <- readRDS("dc_proj/data_rds_files/x_train") %>%
  select(ID:T) %>%
  filter(
    model_time == "00h",
    forecast_time == "D-1"
  ) %>%
  mutate(
    class = "train"
  )

public_test <- readRDS("dc_proj/data_rds_files/x_test") %>%
  select(ID:T) %>%
  filter(
    model_time == "00h",
    forecast_time == "D-1"
  ) %>%
  drop_na(U) %>%
  mutate(
    class = "test"
  )


total <- bind_rows(
  data,
  public_test
)


t2 <- total %>%
  filter(met_model == "NWP2") %>%
  mutate(
    windspeed = sqrt(U^2+V^2)
  ) %>%
  select(!T) %>%
  left_join(
    # left side: model 2 weather data, interpolated
    .,
    # right side: model 1 temperature data
    total %>%
      filter(met_model == "NWP1") %>%
      select(ID, Time, T),
    # join by:
    by = c("ID", "Time")
  )

train <- total %>%
  ungroup %>%
  filter(
    class == "train",
    Production != 0
  ) %>%
  nest(.by = c(WF, t_cut), .key = "train_data") %>%
  mutate(
    gam_fit = map(
      train_data,
      ~ mgcv::gam(Production ~ s(windspeed), data = .x)
    )
  )

to_predict <- total %>%
  ungroup %>%
  filter(
    class == "test"
  ) %>%
  nest(.by = c(WF, t_cut), .key = "test_data") %>%
  left_join(
    train %>%
      select(!train_data)
  ) %>%
  mutate(
    predicted = map2(.x = gam_fit, .y = test_data, .f = predict.gam)
  ) %>%
  unnest(c(test_data, predicted))

to_predict %>%
  select(ID, "Production" = predicted) %>%
  mutate(Production = as.numeric(Production)) %>%
  arrange(ID) %>%
  write_csv(
    file = "gam_prediction_1.csv")
