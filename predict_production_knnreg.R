rm(list = ls())

library(plotly)
library(caret)
library(tidyverse)

data <- readRDS("dc_proj/data_rds_files/x_train") %>%
  select(ID:T) %>%
  filter(
    model_time == "00h",
    forecast_time == "D-1"
  )

# set.seed(3007)
# 
# train_idx <- sample(
#   c(T,F),
#   size = nrow(data),
#   replace = T,
#   prob = c(0.8, 0.2))
# 
# train <- data[train_idx, ]
# test <- data[!train_idx, ]
# 
# knn_1 <- train %>%
#   knnreg(
#     Production ~ (U + V) * WF,
#     data = .,
#     k = 5
#   )
# 
# knn_2 <- train %>%
#   knnreg(
#     Production ~ ((U + V) * WF) * before_ten,
#     data = .,
#     k = 5
#   )

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

cape <- function(actual, predicted){
  100 * sum(abs(actual - predicted)/sum(actual))
}

knn_cv <- function(k, data, folds = 4){
  errors <- rep(NULL, 5)
  
  folds_def <- sample(
    1:folds,
    size = nrow(data),
    replace = T,
    prob = rep(1/folds, folds))
  
  for(i in 1:folds){
    
    train_idx <- folds_def != i
    
    mod <- knnreg(
      data = data[train_idx, ],
      Production ~ (U + V) * WF,
      k = k
    )
    
    errors[i] <- cape(
      data[!train_idx, ]$Production,
      predict(mod, newdata = data[!train_idx, ]))
  }
  
  return(mean(errors))
}

t1 <- Sys.time()

k = c(10, 12, 14, 16, 18, 20)
nwp1_pred <- sapply(k, knn_cv, data = (data %>% filter(met_model == "NWP1")))
nwp4_pred <- sapply(k, knn_cv, data = (data %>% filter(met_model == "NWP4")))

nwp2_pred <- sapply(k, knn_cv, data = (data %>% filter(met_model == "NWP2")))
nwp3_pred <- sapply(k, knn_cv, data = (data %>% filter(met_model == "NWP3")))

preds <- cbind(k, nwp1_pred, nwp2_pred, nwp3_pred, nwp4_pred)
preds
print(Sys.time()-t1)

preds %>%
  as.data.frame %>%
  pivot_longer(cols = nwp1_pred:nwp4_pred) %>%
  ggplot(
    aes(x = k, y = value, col = name)
  ) +
  geom_point()

## old code ----

# pred_knn_1 <- test %>%
#   predict(knn_1, newdata = .)
# 
# pred_knn_2 <- test %>%
#   predict(knn_2, newdata = .)
# 
# data.frame(
#   mod = c("mod_1", "mod_2"),
#   CAPE = c(
#     100 * sum(abs(test$Production - pred_knn_1)/sum(test$Production)),
#     100 * sum(abs(test$Production - pred_knn_2)/sum(test$Production))
#   )
# )

# prediction on NWP2 with U, V ----

knn_final <- data %>%
  filter(
    met_model == "NWP2",
    model_time == "00h",
    forecast_time == "D-1"
  ) %>%
  knnreg(
    Production ~ (U + V) * WF,
    data = .,
    k = 12
  )

public_test <- readRDS("ChallengeData/data_rds_files/x_test") %>%
  select(ID:T) %>%
  filter(
    met_model == "NWP2",
    model_time == "00h",
    forecast_time == "D-1"
  ) %>%
  drop_na(U)

pred_knn <- public_test %>%
  predict(knn_final, newdata = .)

bind_cols(
  ID = public_test$ID,
  Production = pred_knn
) %>%
  # NA values in dem NWP mit Nullen auffüllen:
  bind_rows(
    bind_cols(
      ID = pbt$ID,
      Production = rep(0, length(pbt$ID))
    )
  ) %>%
  arrange(ID) %>%
  write_csv(
    file = "knn_prediction_3.csv")


# prediction with U, V from NWP2, T from NWP1 ----
knn_model_uvt <- data %>%
  filter(
    met_model == "NWP2"
  ) %>%
  left_join(
    data %>%
      filter(
        met_model == "NWP1"
      ) %>%
      select(WF, Time, temp = T),
    by = c("WF", "Time")
  ) %>%
  drop_na(U, V, temp) %>%
  knnreg(
    Production ~ (U + V + temp) * WF,
    data = .,
    k = 3
  )

test <- readRDS("ChallengeData/data_rds_files/x_test") %>%
  filter(
    model_time == "00h",
    forecast_time == "D-1"
  )

test <- test %>%
  filter(
    met_model == "NWP2"
  ) %>%
  left_join(
    test %>%
      filter(
        met_model == "NWP1"
      ) %>%
      mutate(
        T = zoo::na.approx(T, maxgap = 2)
      ) %>%
      select(WF, Time, U1 = U, V1 = V, temp = T),
    by = c("WF", "Time")
  ) %>%
  mutate(
    U = coalesce(U, U1),
    V = coalesce(V, V1)
  ) %>%
  select(
    ID, WF, Time, U, V, temp
  )

pred_knn <- test %>%
  predict(knn_model_uvt, newdata = .)

bind_cols(
  ID = test$ID,
  Production = pred_knn
) %>%
  write_csv(
    file = "knn_prediction_4.csv")


# prediction with U, V from NWP2, classified T from NWP1 ----
data <- data %>%
  filter(
    met_model == "NWP2"
  ) %>%
  left_join(
    data %>%
      filter(
        met_model == "NWP1"
      ) %>%
      select(WF, Time, temp = T),
    by = c("WF", "Time")
  ) %>%
  drop_na(U, V, temp) %>%
  mutate(
    temp_c = cut(
      temp,
      breaks = c(-Inf, 
                 quantile(temp, probs = c(1/3, 2/3), names=F),
                 Inf),
      labels = c("low", "mid", "high")
    )
  )

knn_model_uvt <- data %>%
  knnreg(
    Production ~ (U + V + temp_c) * WF,
    data = .,
    k = 12
  )

test <- readRDS("ChallengeData/data_rds_files/x_test") %>%
  filter(
    model_time == "00h",
    forecast_time == "D-1"
  )

test <- test %>%
  filter(
    met_model == "NWP2"
  ) %>%
  left_join(
    test %>%
      filter(
        met_model == "NWP1"
      ) %>%
      mutate(
        T = zoo::na.approx(T, maxgap = 2)
      ) %>%
      select(WF, Time, U1 = U, V1 = V, temp = T),
    by = c("WF", "Time")
  ) %>%
  mutate(
    U = coalesce(U, U1),
    V = coalesce(V, V1),
    temp_c = cut(
      temp,
      breaks = c(-Inf, 
                 quantile(data$temp, probs = c(1/3, 2/3), names=F),
                 Inf),
      labels = c("low", "mid", "high")
    )
  )

pred_knn <- test %>%
  predict(knn_model_uvt, newdata = .)

bind_cols(
  ID = test$ID,
  Production = pred_knn
) %>%
  write_csv(
    file = "knn_prediction_5.csv")
